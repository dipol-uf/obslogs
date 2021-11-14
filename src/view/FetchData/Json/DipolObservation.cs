using System;
using System.Text.Json.Serialization;

namespace DIPolWeb.FetchData.Json;

internal record DipolObservation(
    [property: JsonPropertyName("Date"), JsonConverter(typeof(DateOnlyConverter))] DateOnly Date,
    string Object,
    string? Type,
    [property: JsonPropertyName("ExpTime")]
    double? ExposureTime,
    [property: JsonPropertyName("N")] int? ImageCount,
    double? Focus,
    [property: JsonPropertyName("Inst")] string Instrument,
    [property: JsonPropertyName("Tlscp")] string Telescope,
    string? Comment
) : IData;
