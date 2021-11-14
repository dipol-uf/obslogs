using System;
using System.Collections.Generic;
using System.Net.Http;
using System.Net.Http.Json;

namespace DIPolWeb.FetchData.Json;

internal sealed class JsonSource : IDataSource
{
    private readonly HttpClient _client;
    public JsonSource(HttpClient client) => _client = client ?? throw new ArgumentNullException(nameof(client));
    public async IAsyncEnumerable<IData> GetData()
    {
        DipolObservation[] data = await _client.GetFromJsonAsync<DipolObservation[]>("sample-data/obslog.json")
                               ?? throw new InvalidOperationException("Failed to load data");

        foreach (var item in data)
        {
            yield return item;
        }
    }
}