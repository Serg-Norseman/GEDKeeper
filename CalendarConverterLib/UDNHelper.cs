using System;

namespace GKCommon
{
    public static class UDNHelper
    {
        public const uint IGNORE_YEAR = 1u << 31;
        public const uint IGNORE_MONTH = 1u << 30;
        public const uint IGNORE_DAY = 1u << 29;

        public const uint YEAR_MASK = IGNORE_YEAR;
        public const uint MONTH_MASK = IGNORE_MONTH;
        public const uint DAY_MASK = IGNORE_DAY;

        public const uint VALUE_MASK = 0x1fffffff;

        public const int UNKNOWN_YEAR = -4713;
        public const int UNKNOWN_MONTH = 0;
        public const int UNKNOWN_DAY = 0;
        

        public static int compareUDN(uint l, uint r)
        {
            if ((UDNHelper.YEAR_MASK & l) != UDNHelper.IGNORE_YEAR)
            {
                if ((UDNHelper.YEAR_MASK & r) != UDNHelper.IGNORE_YEAR)
                {
                    return ((int) (UDNHelper.VALUE_MASK & l)) - ((int) (UDNHelper.VALUE_MASK & r));
                }
                else
                {
                    return 1;
                }
            }
            else if ((UDNHelper.YEAR_MASK & r) != UDNHelper.IGNORE_YEAR)
            {
                return -1;
            }
            else if ((UDNHelper.MONTH_MASK & l) != UDNHelper.IGNORE_MONTH)
            {
                if ((UDNHelper.MONTH_MASK & r) != UDNHelper.IGNORE_MONTH)
                {
                    return ((int) (UDNHelper.VALUE_MASK & l)) - ((int) (UDNHelper.VALUE_MASK & r));
                }
                else
                {
                    return 1;
                }
            }
            else if ((UDNHelper.MONTH_MASK & r) != UDNHelper.IGNORE_MONTH)
            {
                return -1;
            }
            else if ((UDNHelper.DAY_MASK & l) != UDNHelper.IGNORE_DAY)
            {
                if ((UDNHelper.DAY_MASK & r) != UDNHelper.IGNORE_DAY)
                {
                    return ((int) (UDNHelper.VALUE_MASK & l)) - ((int) (UDNHelper.VALUE_MASK & r));
                }
                else
                {
                    return 1;
                }
            }
            else if ((UDNHelper.DAY_MASK & r) != UDNHelper.IGNORE_DAY)
            {
                return -1;
            }
            else
            {
                return 0;
            }
        }

        public static uint getGregorianUDN(int year, int month, int day)
        {
            uint result = (uint)CalendarConverter.gregorian_to_jd(
                Math.Max(UNKNOWN_YEAR + 1, year),
                Math.Max(UNKNOWN_MONTH + 1, month),
                Math.Max(UNKNOWN_DAY + 1, day));

            if (UNKNOWN_YEAR + 1 > year)
            {
                result |= IGNORE_YEAR;
            }
            if (UNKNOWN_MONTH + 1 > month)
            {
                result |= IGNORE_MONTH;
            }
            if (UNKNOWN_DAY + 1 > day)
            {
                result |= IGNORE_DAY;
            }
            return result;
        }

        public static uint getJulianUDN(int year, int month, int day)
        {
            uint result = (uint) CalendarConverter.julian_to_jd(
                Math.Max(UNKNOWN_YEAR + 1, year),
                Math.Max(UNKNOWN_MONTH + 1, month),
                Math.Max(UNKNOWN_DAY + 1, day));

            if (UNKNOWN_YEAR + 1 > year)
            {
                result |= IGNORE_YEAR;
            }
            if (UNKNOWN_MONTH + 1 > month)
            {
                result |= IGNORE_MONTH;
            }
            if (UNKNOWN_DAY + 1 > day)
            {
                result |= IGNORE_DAY;
            }
            return result;
        }
    }
}
