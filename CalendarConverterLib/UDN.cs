using System;
using System.Runtime.InteropServices;

namespace GKCommon
{
    public enum CalendarType { ctGregorian, ctJulian, ctHebrew, ctIslamic }

    [Serializable]
    [StructLayout(LayoutKind.Sequential, Pack = 1)]
    public struct UDN : ICloneable, IComparable
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


        private readonly uint fValue;


        private UDN(uint value)
        {
            this.fValue = value;
        }

        public UDN(CalendarType calendar, int year, int month, int day)
        {
            uint result = CreateVal(calendar, year, month, day);
            this.fValue = result;
        }

        public uint GetUnmaskedValue()
        {
            return (UDN.ValueMask & this.fValue);
        }

        public object Clone()
        {
            return new UDN(this.fValue);
        }

        public int CompareTo(object obj)
        {
            if (obj is UDN) {
                return CompareUDN(this.fValue, ((UDN)obj).fValue);
            }

            return -1;
        }

        private static int CompareUDN(uint l, uint r)
        {
            if ((UDN.YearMask & l) != UDN.IgnoreYear)
            {
                if ((UDN.YearMask & r) != UDN.IgnoreYear)
                {
                    return ((int) (UDN.ValueMask & l)) - ((int) (UDN.ValueMask & r));
                }
                else
                {
                    return 1;
                }
            }
            else if ((UDN.YearMask & r) != UDN.IgnoreYear)
            {
                return -1;
            }
            else if ((UDN.MonthMask & l) != UDN.IgnoreMonth)
            {
                if ((UDN.MonthMask & r) != UDN.IgnoreMonth)
                {
                    return ((int) (UDN.ValueMask & l)) - ((int) (UDN.ValueMask & r));
                }
                else
                {
                    return 1;
                }
            }
            else if ((UDN.MonthMask & r) != UDN.IgnoreMonth)
            {
                return -1;
            }
            else if ((UDN.DayMask & l) != UDN.IgnoreDay)
            {
                if ((UDN.DayMask & r) != UDN.IgnoreDay)
                {
                    return ((int) (UDN.ValueMask & l)) - ((int) (UDN.ValueMask & r));
                }
                else
                {
                    return 1;
                }
            }
            else if ((UDN.DayMask & r) != UDN.IgnoreDay)
            {
                return -1;
            }
            else
            {
                return 0;
            }
        }

        private static uint CreateVal(CalendarType calendar, int year, int month, int day)
        {
            uint result = 0;

            int uYear = Math.Max(UnknownYear + 1, year);
            int uMonth = Math.Max(UnknownMonth + 1, month);
            int uDay = Math.Max(UnknownDay + 1, day);

            switch (calendar)
            {
                case CalendarType.ctGregorian:
                    result = (uint)CalendarConverter.gregorian_to_jd(uYear, uMonth, uDay);
                    break;

                case CalendarType.ctJulian:
                    result = (uint)CalendarConverter.julian_to_jd(uYear, uMonth, uDay);
                    break;

                case CalendarType.ctHebrew:
                    result = (uint)CalendarConverter.hebrew_to_jd(uYear, uMonth, uDay);
                    break;

                case CalendarType.ctIslamic:
                    result = (uint)CalendarConverter.islamic_to_jd(uYear, uMonth, uDay);
                    break;
            }

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
