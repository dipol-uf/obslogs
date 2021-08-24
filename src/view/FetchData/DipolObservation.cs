using System;
using System.Globalization;
using System.Text.Json.Serialization;

namespace DIPolWeb.FetchData;
internal record DipolObservation(
    [property: JsonPropertyName("Date")] string DateStr,
    string Object,
    string? Type,
    double? ExpTime,
    int? N,
    double? Focus,
    [property: JsonPropertyName("Inst")] string Instrument,
    [property: JsonPropertyName("Tlscp")] string Telescope,
    string? Comment
)
{
    [JsonIgnore]
    public DateOnly Date => DateTimeOffset.TryParseExact(
        DateStr,
        "yyyy-MM-dd",
        DateTimeFormatInfo.InvariantInfo,
        DateTimeStyles.AssumeUniversal,
        out var date
    ) ? DateOnly.FromDateTime(date.UtcDateTime) : default;
}
