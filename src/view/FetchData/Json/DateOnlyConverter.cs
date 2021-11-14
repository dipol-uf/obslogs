using System;
using System.Text.Json;
using System.Text.Json.Serialization;

namespace DIPolWeb.FetchData.Json;

internal sealed class DateOnlyConverter : JsonConverter<DateOnly>
{
    public override DateOnly Read(ref Utf8JsonReader reader, Type typeToConvert, JsonSerializerOptions options)
    {
        var str = reader.GetString();
        if (str is not null && DateOnly.TryParseExact(
                str.AsSpan(), "yyyy-MM-dd", out var date
            ))
        {
            return date;
        }

        throw new InvalidOperationException($"Unable to parse string '{str}' into {nameof(DateOnly)}.");
    }

    public override void Write(Utf8JsonWriter writer, DateOnly value, JsonSerializerOptions options) =>
        writer.WriteStringValue(value.ToString("yyyy-MM-dd"));
}