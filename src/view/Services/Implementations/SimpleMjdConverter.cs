using System;

namespace DIPolWeb.Services.Implementations
{
    public sealed class SimpleMjdConverter : IMjdConverter
    {
        private static readonly DateTime MjdOrigin = new(
            1858, 11, 17, 0, 0, 0,
            DateTimeKind.Utc
        );

        public DateTime FromMjd(double mjd) => MjdOrigin.AddDays(mjd);

        public double ToMjd(DateTime from) => (from - MjdOrigin).TotalDays;

    }
}
