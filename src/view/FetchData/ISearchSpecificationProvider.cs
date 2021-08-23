using System;

namespace DIPolWeb.FetchData;

internal interface ISearchSpecificationProvider
{
    ISearchSpecification GetSpecification(string input);
}
