using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Security.Cryptography.X509Certificates;
using Pidgin;
using static Pidgin.Parser;
using static Pidgin.Parser<char>;

namespace DIPolWeb.FetchData;

internal class NasaAdsLikeSpecification : ISearchSpecification
{


    private static Parser<char, ColumnSpecification[]> FullParser { get; }

    private readonly string _strRep;
    private readonly ColumnSpecification[]? _specs;
    public NasaAdsLikeSpecification(string stringRepresentation)
    {


        var fullResult = FullParser.Parse(stringRepresentation.Trim());

        if (fullResult.Success)
        {
            var results = new Dictionary<Type, ColumnSpecification>(fullResult.Value.Length);

            foreach (var item in fullResult.Value)
            {
                if (!results.TryAdd(item.GetType(), item))
                {
                    // TODO: handle multiple instances of the same condition
                }
            }

            _specs = fullResult.Value.ToArray();
        }
        
        _strRep = stringRepresentation;
    }

    public bool SatisfiesConditions(DipolObservation obs)
    {
        if (_specs is not null)
        {
            foreach (var spec in _specs)
            {
                if (!spec.Matches(obs))
                {
                    return false;
                }
            }

            return true;
        }

        // Temporary bypass
        return obs.Object.Contains(_strRep, StringComparison.OrdinalIgnoreCase);
    }


    static NasaAdsLikeSpecification()
    {
        Parser<char, NumericCondition> numericValue = Digit.RepeatString(4).Map(x => new NumericCondition(Parse(x)));
        Parser<char, NumericRangeCondition> numericRange = Map((x, _, z) => new NumericRangeCondition(x, z), numericValue, Char('-'), numericValue);
        
        Parser<char, string> objectStringInQuotes = OneOf(LetterOrDigit, Char('_'), Char('-'), Char('+'), Whitespace).AtLeastOnceString();
        Parser<char, string> objectString = OneOf(LetterOrDigit, Char('_'), Char('-'), Char('+')).AtLeastOnceString();

        Parser<char, string> instrumentParser = CIString("dipol-2").Or(CIString("dipol-uf"));

        Parser<char, string> telescopeParser = LetterOrDigit.Or(OneOf('-', '+')).AtLeastOnceString();

        Parser<char, int> twoDigits = Try(Digit.RepeatString(2)).Or(Digit.Map(x => new string(x, 1))).Map(Parse);
        Parser<char, int> fourDigits = Try(Digit.RepeatString(4)).Or(Digit.RepeatString(2)).Map(Parse);

        Parser<char, DateOnly> regularDate = Map(
            (day, month, year) => new DateOnly(year, month, day),
            twoDigits.Before(Char('.')),
            twoDigits.Before(Char('.')),
            fourDigits
        );

        Parser<char, DateOnly> isoDate = Map(
            (year, month, day) => new DateOnly(year, month, day),
            fourDigits.Before(Char('-')),
            twoDigits.Before(Char('-')),
            twoDigits
        );

        Parser<char, DateOnly> usDate = Map(
            (month, day, year) => new DateOnly(year, month, day),
            twoDigits.Before(Char('/')),
            twoDigits.Before(Char('/')),
            fourDigits
        );

        Parser<char, DateCondition> date = OneOf(regularDate, isoDate, usDate).Map(x => new DateCondition(x));

        // TODO: Allow for ranges like 1980- 
        Parser<char, DateRangeCondition> dateRange = Map((x, _, y) => new DateRangeCondition(x, y), date, Char('-'), date);

        Parser<char, ColumnSpecification> yearEntry = BuildEntryParser(
            "Year",
            OneOf(
                Try(numericRange.Cast<Condition<int>>()),
                numericValue.Cast<Condition<int>>()
            )
        ).Map(x => new YearSpecification(x) as ColumnSpecification);

        Parser<char, ColumnSpecification> dateEntry = BuildEntryParser(
            "Date",
            OneOf(
                Try(dateRange.Cast<Condition<DateOnly>>()),
                date.Cast<Condition<DateOnly>>()
            )
        ).Map(x => new DateSpecification(x) as ColumnSpecification);

        Parser<char, ColumnSpecification> objectEntry = BuildEntryParser(
            "Object",
            OneOf(
                objectString,
                InDelimiters(objectStringInQuotes)
            )
        ).Map(x => new ObjectSpecification(new CaseInsensitiveTextCondition(x)) as ColumnSpecification);

        Parser<char, ColumnSpecification> instrumentEntry = BuildEntryParser(
            "Instrument",
            OneOf(
                instrumentParser,
                InDelimiters(instrumentParser)
            )
        ).Map(x => new InstrumentSpecification(new CaseInsensitiveTextCondition(x)) as ColumnSpecification);

        Parser<char, ColumnSpecification> telescopeEntry = BuildEntryParser(
            "Telescope",
            OneOf(
                telescopeParser,
                InDelimiters(telescopeParser)
            )
        ).Map(x => new TelescopeSpecification(new CaseInsensitiveTextCondition(x)) as ColumnSpecification);

        FullParser = OneOf(
            yearEntry,
            objectEntry,
            instrumentEntry,
            telescopeEntry,
            dateEntry
        ).SeparatedAndOptionallyTerminatedAtLeastOnce(Whitespaces).Before(End)
         .Map(x => x.ToArray());
    }
    private static Parser<char, T> BuildEntryParser<T>(string name, Parser<char, T> implementation) =>
        CIString(name)
           .Then(SkipWhitespaces)
           .Then(Char(':'))
           .Then(SkipWhitespaces)
           .Then(implementation);

    private static Parser<char, T> InDelimiters<T>(Parser<char, T> parser, char left = '"', char right = '"') =>
        Char(left).Then(SkipWhitespaces).Then(parser).Before(SkipWhitespaces).Before(Char(right));
    private static int Parse(string s) => int.Parse(s, NumberStyles.Integer, NumberFormatInfo.InvariantInfo);
}
