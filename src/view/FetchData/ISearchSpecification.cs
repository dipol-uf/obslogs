using DIPolWeb.FetchData.Json;

namespace DIPolWeb.FetchData;

internal interface ISearchSpecification
{
    bool SatisfiesConditions(IData obs);
}