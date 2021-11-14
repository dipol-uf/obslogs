using System;

namespace DIPolWeb.FetchData
{
    internal interface IData
    {
        DateOnly Date { get; }
        string Object { get; }
        string? Type { get; }
        double? ExposureTime { get; }
        int? ImageCount { get; }
        double? Focus { get; }
        string Instrument { get; }
        string Telescope { get; }
        string? Comment { get; }
    }
}
