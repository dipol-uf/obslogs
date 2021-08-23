
using System;

namespace DIPolWeb.FetchData;
internal class NasaAdsLikeSpecificationProvider : ISearchSpecificationProvider
{
    public ISearchSpecification GetSpecification(ReadOnlySpan<char> input) => new NasaAdsLikeSpecification(input);

}
