using System.Collections.Generic;

namespace DIPolWeb.FetchData
{
    internal interface IDataSource
    {
        IAsyncEnumerable<IData> GetData();
    }
}
