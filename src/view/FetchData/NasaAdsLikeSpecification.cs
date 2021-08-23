using System;
using System.Globalization;
using System.Linq;
using Pidgin;
using static Pidgin.Parser;
using static Pidgin.Parser<char>;

namespace DIPolWeb.FetchData;

internal class NasaAdsLikeSpecification : ISearchSpecification
{
    private static Parser<char, Condition> YearEntry { get; }
    private static Parser<char, Condition> ObjectEntry { get; }
    private static Parser<char, Condition> InstrumentEntry { get; }
    private static Parser<char, Condition> TelescopeEntry { get; }


    private readonly string _strRep;

    public NasaAdsLikeSpecification(string stringRepresentation)
    {
       

        var yearResult = YearEntry.Parse(stringRepresentation);
        var objectResult = ObjectEntry.Parse(stringRepresentation);
        _strRep = stringRepresentation;
    }

    public bool SatisfiesConditions(DipolObservation obs) => obs.Object.Contains(_strRep, StringComparison.OrdinalIgnoreCase);


    static NasaAdsLikeSpecification()
    {
        Parser<char, NumericCondition> numericValue = Digit.RepeatString(4).Map(x => new NumericCondition(Parse(x)));
        Parser<char, NumericRangeCondition> numericRange = Map((x, _, z) => new NumericRangeCondition(x, z), numericValue, Char('-'), numericValue);
        
        Parser<char, string> objectStringInQuotes = OneOf(LetterOrDigit, Char('_'), Char('-'), Char('+'), Whitespace).AtLeastOnceString();
        Parser<char, string> objectString = OneOf(LetterOrDigit, Char('_'), Char('-'), Char('+')).AtLeastOnceString();

        Parser<char, string> instrumentParser = CIString("dipol-2").Or(CIString("dipol-uf"));

        Parser<char, string> telescopeParser = LetterOrDigit.Or(OneOf('-', '+')).AtLeastOnceString();


        YearEntry = BuildEntryParser(
            "Year",
            OneOf(
                numericValue.Cast<Condition>(),
                numericRange.Cast<Condition>()
            )
        );

        ObjectEntry = BuildEntryParser(
            "Object",
            OneOf(
                objectString,
                InDelimiters(objectStringInQuotes)
            ).Map(x => new CaseInsensitiveTextCondition(x.Trim()) as Condition)
        );

        InstrumentEntry = BuildEntryParser(
            "Instrument",
            OneOf(
                instrumentParser,
                InDelimiters(instrumentParser)
            ).Map(x => new CaseInsensitiveTextCondition(x) as Condition)
        );

        TelescopeEntry = BuildEntryParser(
            "Telescope",
            OneOf(
                telescopeParser,
                InDelimiters(telescopeParser)
            ).Map(x => new CaseInsensitiveTextCondition(x) as Condition)
        );


    }
    private static Parser<char, Condition> BuildEntryParser(string name, Parser<char, Condition> implementation) =>
        CIString(name)
           .Then(SkipWhitespaces)
           .Then(Char(':'))
           .Then(SkipWhitespaces)
           .Then(implementation);

    private static Parser<char, T> InDelimiters<T>(Parser<char, T> parser, char left = '"', char right = '"') =>
        Char(left).Then(SkipWhitespaces).Then(parser).Before(SkipWhitespaces).Before(Char(right));
    private static int Parse(string s) => int.Parse(s, NumberStyles.Integer, NumberFormatInfo.InvariantInfo);
}
