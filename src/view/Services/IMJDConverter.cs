using System;

namespace DIPolWeb.Services
{
    public interface IMjdConverter
    {
        DateTime FromMjd(double mjd);
        double ToMjd(DateTime from);
    }
}
