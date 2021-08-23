using System;

namespace DIPolWeb.FetchData;

internal class NasaAdsLikeSpecification : ISearchSpecification
{
    private readonly string _strRep;

    public NasaAdsLikeSpecification(ReadOnlySpan<char> stringRepresentation) =>
        _strRep = stringRepresentation.ToString();

    public bool SatisfiesConditions(DipolObservation obs) => obs.Object.Contains(_strRep, StringComparison.OrdinalIgnoreCase);
}
