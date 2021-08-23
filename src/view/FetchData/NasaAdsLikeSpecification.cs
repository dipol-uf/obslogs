using System;

namespace DIPolWeb.FetchData;

internal class NasaAdsLikeSpecification : ISearchSpecification
{
    public NasaAdsLikeSpecification(ReadOnlySpan<char> stringRepresentation) => throw new NotImplementedException();
    public bool SatisfiesConditions(DipolObservation obs) =>
        throw new NotImplementedException();
}
