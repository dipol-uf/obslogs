using System;

namespace DIPolWeb.Services.Implementations
{
    public sealed class SimpleMjdConverter : IMjdConverter
    {
        private static readonly DateTime MjdOrigin = new(1858, 11, 17, 0, 0, 0, DateTimeKind.Utc);

        public DateOnly FromMjd(long mjd) => DateOnly.FromDateTime(MjdOrigin.AddDays(mjd));

        public long ToMjd(DateOnly from) => (long)(from.ToDateTime(default, DateTimeKind.Utc) - MjdOrigin).TotalDays;

    }
}
