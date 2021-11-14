using System;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

namespace DIPolWeb.FetchData.DataBase
{
    internal sealed record DbEntry(
        [property: Key] int Key,
        [property: Column("Date")] DateOnly Date,
        string Object,
        [property: Column("Type")] string? Type,
        [property: Column("ExpTime")] double? ExposureTime,
        [property: Column("N")] int? ImageCount,
        double? Focus,
        [property: Column("Inst")] string Instrument,
        [property: Column("Tlscp")] string Telescope,
        string? Comment
    ) : IData;

}
