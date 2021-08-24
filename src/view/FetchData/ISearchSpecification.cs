namespace DIPolWeb.FetchData;

internal interface ISearchSpecification
{
    bool SatisfiesConditions(DipolObservation obs);
}