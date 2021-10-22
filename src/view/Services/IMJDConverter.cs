using System;

namespace DIPolWeb.Services
{
    public interface IMjdConverter
    {
        DateOnly FromMjd(long mjd);
        long ToMjd(DateOnly from);
    }
}
