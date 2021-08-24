
using System;

namespace DIPolWeb.FetchData;
internal class NasaAdsLikeSpecificationProvider : ISearchSpecificationProvider
{
    public ISearchSpecification GetSpecification(string input) => new NasaAdsLikeSpecification(input);

}
