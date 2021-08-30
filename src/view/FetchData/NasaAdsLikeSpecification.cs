using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using Pidgin;
using static Pidgin.Parser;
using static Pidgin.Parser<char>;

namespace DIPolWeb.FetchData;

internal class NasaAdsLikeSpecification : ISearchSpecification
{
    private static readonly Parser<char, string> WordParser = LetterOrDigit.Or(OneOf('-', '+')).AtLeastOnceString();

    private static readonly Func<string, int> ParseInt = ParseIntImpl;
    private static readonly Parser<char, ColumnSpecification[]> ConditionsParser;

    private readonly ColumnSpecification[] _specs;
    public NasaAdsLikeSpecification(string stringRepresentation)
    {

        var trimmedInput = stringRepresentation.Trim();
        Result<char, ColumnSpecification[]> parsingResult = ConditionsParser.Parse(trimmedInput);

        if (parsingResult.Success)
        {
            var results = new Dictionary<Type, ColumnSpecification>(parsingResult.Value.Length);

            foreach (var item in parsingResult.Value)
            {
                if (results.TryAdd(item.GetType(), item))
                {
                    continue;
                }
                throw new ArgumentException(message: $"Multiple search conditions of type ${item.GetType()}", paramName: nameof(stringRepresentation));
            }

            _specs = parsingResult.Value.ToArray();
        }

        else
        {
            if (trimmedInput.AsSpan().IndexOfAny(stackalloc char[] { ':', ';', '"' }) >= 0)
            {
                throw new ArgumentException(message: "Invalid search string", paramName: nameof(stringRepresentation));
            }
            _specs = new ColumnSpecification[] { new ObjectSpecification(new CaseInsensitiveTextCondition(trimmedInput)) };
        }
    }

    public bool SatisfiesConditions(DipolObservation obs) =>
        _specs.All(spec => spec.Matches(obs));


    static NasaAdsLikeSpecification()
    {
        ConditionsParser = OneOf(
            BuildYearParser(),
            BuildObjectParser(),
            BuildInstrumentParser(),
            BuildTelescopeParser(),
            BuildDateParser()
        ).SeparatedAndOptionallyTerminatedAtLeastOnce(Whitespaces).Before(End)
         .Map(x => x.ToArray());
    }
    private static Parser<char, T> BuildEntryParser<T>(string name, Parser<char, T> implementation) =>
        CIString(name)
           .Then(SkipWhitespaces)
           .Then(Char(':'))
           .Then(SkipWhitespaces)
           .Then(implementation);

    private static Parser<char, T> BuildEntryParser<T>(string name, params Parser<char, T>[] implementations)
    {
        if (implementations.Length == 0)
        {
            throw new ArgumentOutOfRangeException(nameof(implementations));
        }

        var copiedImpl = new Parser<char, T>[implementations.Length];

        for (var i = 0; i < implementations.Length - 1; i++)
        {
            copiedImpl[i] = Try(implementations[i]);
        }

        copiedImpl[^1] = implementations[^1];

        return BuildEntryParser(name, OneOf(copiedImpl));
    }


    private static Parser<char, ColumnSpecification> BuildTelescopeParser()
    {
        return BuildEntryParser(
            "Telescope",
            InDelimiters(WordParser),
            WordParser
        ).Map(x => new TelescopeSpecification(new CaseInsensitiveTextCondition(x)) as ColumnSpecification);
    }

    private static Parser<char, ColumnSpecification> BuildInstrumentParser()
    {
        return BuildEntryParser(
            "Instrument",
            InDelimiters(WordParser),
            WordParser
        ).Map(x => new InstrumentSpecification(new CaseInsensitiveTextCondition(x)) as ColumnSpecification);
    }

    private static Parser<char, ColumnSpecification> BuildObjectParser()
    {
        Parser<char, string> objectStringInQuotes = OneOf(LetterOrDigit, Char('_'), Char('-'), Char('+'), Whitespace).AtLeastOnceString();
        Parser<char, string> objectString = OneOf(LetterOrDigit, Char('_'), Char('-'), Char('+')).AtLeastOnceString();

        return BuildEntryParser(
            "Object",
            InDelimiters(objectStringInQuotes),
            objectString
        ).Map(x => new ObjectSpecification(new CaseInsensitiveTextCondition(x)) as ColumnSpecification);
    }

    private static Parser<char, ColumnSpecification> BuildYearParser()
    {
        Parser<char, int> numericValue = Digit.RepeatString(4).Map(ParseInt);
        Parser<char, (int Lower, int Upper)> numericRange = ParseRange(numericValue, (x, y) => (x, y));

        return BuildEntryParser(
            "Year",
            numericRange,
            numericValue.Map(x => (Lower: x, Upper: x))
        ).Map(
            x => new DateSpecification(
                new DateRangeCondition(
                    new DateCondition(new DateOnly(x.Lower, 1, 1)),
                    new DateCondition(new DateOnly(x.Upper, 12, 31))
                )
            ) as ColumnSpecification
        );
    }

    private static Parser<char, ColumnSpecification> BuildDateParser()
    {
        Parser<char, int> twoDigits = Try(Digit.RepeatString(2)).Or(Digit.Map(x => new string(x, 1))).Map(ParseInt);
        Parser<char, int> fourDigits = Try(Digit.RepeatString(4)).Or(Digit.RepeatString(2)).Map(ParseInt);

        Parser<char, DateOnly> regularDate = Map(
            (day, month, year) => new DateOnly(year, month, day),
            twoDigits.Before(Char('.')),
            twoDigits.Before(Char('.')),
            fourDigits
        );

        Parser<char, DateCondition> date = regularDate.Map(x => new DateCondition(x));
        Parser<char, DateRangeCondition> dateRange = ParseRange(date, (x, y) => new DateRangeCondition(x, y));


        return BuildEntryParser(
            "Date",
            dateRange.Cast<Condition<DateOnly>>(),
            date.Cast<Condition<DateOnly>>()
        ).Map(x => new DateSpecification(x) as ColumnSpecification);
    }

    private static Parser<char, T> InDelimiters<T>(Parser<char, T> parser, char left = '"', char right = '"') =>
        Char(left).Then(SkipWhitespaces).Then(parser).Before(SkipWhitespaces).Before(Char(right));
    private static int ParseIntImpl(string s) => int.Parse(s, NumberStyles.Integer, NumberFormatInfo.InvariantInfo);

    private static Parser<char, TRet> ParseRange<T, TRet>(Parser<char, T> parser, Func<T, T, TRet> mapper) =>
        Map((x, _, y) => mapper(x, y), parser, Char('-'), parser);
}
