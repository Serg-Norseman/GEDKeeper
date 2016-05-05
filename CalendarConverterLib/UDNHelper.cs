using System;

namespace GKCommon
{
    public static class UDNHelper
    {
        public const uint IgnoreYear = 1u << 31;
        public const uint IgnoreMonth = 1u << 30;
        public const uint IgnoreDay = 1u << 29;

        public const uint YearMask = IgnoreYear;
        public const uint MonthMask = IgnoreMonth;
        public const uint DayMask = IgnoreDay;

        public const uint ValueMask = 0x1fffffff;

        public const int UnknownYear = -4713;
        public const int UnknownMonth = 0;
        public const int UnknownDay = 0;


        public static int CompareUDN(uint l, uint r)
        {
            if ((UDNHelper.YearMask & l) != UDNHelper.IgnoreYear)
            {
                if ((UDNHelper.YearMask & r) != UDNHelper.IgnoreYear)
                {
                    return ((int) (UDNHelper.ValueMask & l)) - ((int) (UDNHelper.ValueMask & r));
                }
                else
                {
                    return 1;
                }
            }
            else if ((UDNHelper.YearMask & r) != UDNHelper.IgnoreYear)
            {
                return -1;
            }
            else if ((UDNHelper.MonthMask & l) != UDNHelper.IgnoreMonth)
            {
                if ((UDNHelper.MonthMask & r) != UDNHelper.IgnoreMonth)
                {
                    return ((int) (UDNHelper.ValueMask & l)) - ((int) (UDNHelper.ValueMask & r));
                }
                else
                {
                    return 1;
                }
            }
            else if ((UDNHelper.MonthMask & r) != UDNHelper.IgnoreMonth)
            {
                return -1;
            }
            else if ((UDNHelper.DayMask & l) != UDNHelper.IgnoreDay)
            {
                if ((UDNHelper.DayMask & r) != UDNHelper.IgnoreDay)
                {
                    return ((int) (UDNHelper.ValueMask & l)) - ((int) (UDNHelper.ValueMask & r));
                }
                else
                {
                    return 1;
                }
            }
            else if ((UDNHelper.DayMask & r) != UDNHelper.IgnoreDay)
            {
                return -1;
            }
            else
            {
                return 0;
            }
        }

        public static uint GetGregorianUDN(int year, int month, int day)
        {
            uint result = (uint)CalendarConverter.gregorian_to_jd(
                Math.Max(UnknownYear + 1, year),
                Math.Max(UnknownMonth + 1, month),
                Math.Max(UnknownDay + 1, day));

            if (UnknownYear + 1 > year)
            {
                result |= IgnoreYear;
            }
            if (UnknownMonth + 1 > month)
            {
                result |= IgnoreMonth;
            }
            if (UnknownDay + 1 > day)
            {
                result |= IgnoreDay;
            }
            return result;
        }

        public static uint GetJulianUDN(int year, int month, int day)
        {
            uint result = (uint) CalendarConverter.julian_to_jd(
                Math.Max(UnknownYear + 1, year),
                Math.Max(UnknownMonth + 1, month),
                Math.Max(UnknownDay + 1, day));

            if (UnknownYear + 1 > year)
            {
                result |= IgnoreYear;
            }
            if (UnknownMonth + 1 > month)
            {
                result |= IgnoreMonth;
            }
            if (UnknownDay + 1 > day)
            {
                result |= IgnoreDay;
            }
            return result;
        }
    }
}
