/*
 *  CalendarConverter, part of project "Fourmilab Calendar Converter"
 *  Partial copyright by John Walker
 *  http://www.fourmilab.ch/documents/calendar/
 *  This program is in the public domain.
 *
 *  C# implementation:
 *  Copyright (C) 2011 by Sergey V. Zhdanovskih.
 *
 *  Additional methods for transformations:
 *  Copyright (C) 2016 by Ruslan Garipov.
 */

using System;

namespace GKCommon
{
    public class CalendarConverter
    {
        #region Aux functions

        private static int iFloor(double x)
        {
            return (int)Math.Floor(x);
        }

        private static int iCeil(double x)
        {
            return (int)Math.Ceiling(x);
        }

        private static double _modf(double a, double b)
        {
            return (a - b * Math.Floor(a / b));
        }

        private static int _modi(double a, double b)
        {
            return (int)Math.Truncate(a - b * Math.Floor(a / b));
        }

        protected static int downwardRounding(int dividend, int divisor)
        {
            if (0 <= dividend)
            {
                dividend /= divisor;
            }
            else
            {
                dividend = (int)(dividend / ((double) (divisor)) - 1.0);
            }
            return dividend;
        }

        #endregion

        #region Julian day specific

        public static int jwday(double j)
        {
            return _modi(Math.Floor(j + 1.5), 7.0);
        }

        #endregion

        #region Gregorian calendar

        private const double GREGORIAN_EPOCH = 1721425.5;

        public static bool leap_gregorian(int year)
        {
            return (year % 4 == 0) && (year % 100 != 0 || year % 400 == 0);
        }

        // Based on "Fourmilab Calendar Converter"
        public static double gregorian_to_jd(int year, int month, int day)
        {
            int y = year - 1;

            return (GREGORIAN_EPOCH - 1) + 365 * y + (y / 4) - (y / 100) + (y / 400) + (367 * month - 362) / 12 + (month <= 2 ? 0 : (leap_gregorian(year) ? -1 : -2)) + day;
        }

        // Based on "Fourmilab Calendar Converter"
        public static void jd_to_gregorian(double jd, out int year, out int month, out int day)
        {
            double wjd = (Math.Floor((jd - 0.5)) + 0.5);
            double depoch = (wjd - GREGORIAN_EPOCH);
            int quadricent = iFloor((depoch / 146097.0));
            double dqc = _modf(depoch, 146097.0);
            int cent = iFloor((dqc / 36524.0));
            double dcent = _modf(dqc, 36524.0);
            int quad = iFloor((dcent / 1461.0));
            double dquad = _modf(dcent, 1461.0);
            int yindex = iFloor((dquad / 365.0));
            year = quadricent * 400 + cent * 100 + (quad << 2) + yindex;
            if (cent != 4 && yindex != 4)
            {
                year++;
            }
            double yearday = (wjd - gregorian_to_jd(year, 1, 1));
            int leapadj = (wjd < gregorian_to_jd(year, 3, 1) ? 0 : (leap_gregorian(year) ? 1 : 2));
            month = iFloor((((yearday + leapadj) * 12.0 + 373.0) / 367.0));
            day = (int)(Math.Truncate(wjd - gregorian_to_jd(year, month, 1)) + 1);
        }

        // Based on https://en.wikipedia.org/wiki/Julian_day
        public static int gregorian_to_jd2(int year, int month, int day)
        {
            int a = (14 - month) / 12;
            int y = year + 4800 - a;
            int m = month + 12 * a - 3;

            return (day + (153 * m + 2) / 5 + 365 * y + y / 4 - y / 100 + y / 400 - 32045);
        }

        // Based on https://en.wikipedia.org/wiki/Julian_day
        // astronomical years, 0 = "-4713/11/24"
        public static void jd_to_gregorian2(int jd, out int year, out int month, out int day)
        {
            int a = jd + 32044;
            int b = (4 * a + 3) / 146097;
            int c = a - (146097 * b) / 4;
            int d = (4 * c + 3) / 1461;
            int e = c - (1461 * d) / 4;
            int m = (5 * e + 2) / 153;

            day = e - (153 * m + 2) / 5 + 1;
            month = m + 3 - 12 * (m / 10);
            year = 100 * b + d - 4800 + m / 10;
        }

        #endregion

        #region Julian calendar

        private const double JULIAN_EPOCH = 1721423.5;

        public static bool leap_julian(int year)
        {
            return _modf(year, 4.0) == (year > 0 ? 0 : 3);
        }

        // Based on "Fourmilab Calendar Converter"
        public static double julian_to_jd(int year, int month, int day)
        {
            if (year < 1)
            {
                year++;
            }
            if (month <= 2)
            {
                year--;
                month += 12;
            }
            return (Math.Floor((365.25 * (year + 4716))) + Math.Floor((30.6001 * (month + 1))) + day - 1524.5);
        }

        // Based on "Fourmilab Calendar Converter"
        public static void jd_to_julian(double jd, out int year, out int month, out int day)
        {
            int b = iFloor(jd + 0.5) + 1524;
            int c = iFloor(((b - 122.1) / 365.25));
            int d = iFloor((365.25 * c));
            int e = iFloor(((b - d) / 30.6001));
            month = iFloor((e < 14 ? e - 1 : e - 13));
            year = iFloor((month > 2 ? c - 4716 : c - 4715));
            day = b - d - iFloor((30.6001 * e));
            if (year < 1)
            {
                year--;
            }
        }

        public static int julian_to_jd2(int year, int month, int day)
        {
            int a = (14 - month) / 12;
            int y = year + 4800 - a;
            int m = month + 12 * a - 3;

            return (day + (153 * m + 2) / 5 + 365 * y + y / 4 - 32083);
        }

        public static void jd_to_julian2(int jd, out int year, out int month, out int day)
        {
            int c = jd + 32082;
            int d = (4 * c + 3) / 1461;
            int e = c - (1461 * d) / 4;
            int m = (5 * e + 2) / 153;

            day = e - (153 * m + 2) / 5 + 1;
            month = m + 3 - 12 * (m / 10);
            year = d - 4800 + m / 10;
        }

        #endregion

        #region Hebrew calendar

        private const double HEBREW_EPOCH = 347995.5;

        public static bool leap_hebrew(int year)
        {
            return _modf(year * 7 + 1, 19.0) < 7f;
        }

        private static int hebrew_year_months(int year)
        {
            return (leap_hebrew(year) ? 13 : 12);
        }

        private static int hebrew_delay_1(int year)
        {
            int months = iFloor(((235 * year - 234) / 19.0));
            int parts = 12084 + 13753 * months;
            int day = months * 29 + iFloor((parts / 25920.0));
            if (_modf(3 * (day + 1), 7.0) < 3f)
            {
                day++;
            }
            return day;
        }

        private static int hebrew_delay_2(int year)
        {
            int last = hebrew_delay_1(year - 1);
            int present = hebrew_delay_1(year);
            int next = hebrew_delay_1(year + 1);
            return (next - present == 356 ? 2 : (present - last == 382 ? 1 : 0));
        }

        private static double hebrew_year_days(int year)
        {
            return (hebrew_to_jd(year + 1, 7, 1) - hebrew_to_jd(year, 7, 1));
        }

        private static int hebrew_month_days(int year, int month)
        {
            int result;
            if (month == 2 || month == 4 || month == 6 || month == 10 || month == 13)
            {
                result = 29;
            }
            else
            {
                if (month == 12 && !leap_hebrew(year))
                {
                    result = 29;
                }
                else
                {
                    if (month == 8 && _modf(hebrew_year_days(year), 10.0) != 5f)
                    {
                        result = 29;
                    }
                    else
                    {
                        if (month == 9 && _modf(hebrew_year_days(year), 10.0) == 3f)
                        {
                            result = 29;
                        }
                        else
                        {
                            result = 30;
                        }
                    }
                }
            }
            return result;
        }

        public static double hebrew_to_jd(int year, int month, int day)
        {
            int months = hebrew_year_months(year);
            double jd = (HEBREW_EPOCH + hebrew_delay_1(year) + hebrew_delay_2(year) + day + 1.0);
            if (month < 7)
            {
                int num = months;
                int mon = 7;
                if (num >= mon)
                {
                    num++;
                    do
                    {
                        jd = (jd + hebrew_month_days(year, mon));
                        mon++;
                    }
                    while (mon != num);
                }

                int num2 = month - 1;
                mon = 1;
                if (num2 >= mon)
                {
                    num2++;
                    do
                    {
                        jd = (jd + hebrew_month_days(year, mon));
                        mon++;
                    }
                    while (mon != num2);
                }
            }
            else
            {
                int num3 = month - 1;
                int mon = 7;
                if (num3 >= mon)
                {
                    num3++;
                    do
                    {
                        jd = (jd + hebrew_month_days(year, mon));
                        mon++;
                    }
                    while (mon != num3);
                }
            }
            return jd;
        }

        public static void jd_to_hebrew(double jd, out int year, out int month, out int day)
        {
            jd = (Math.Floor(jd) + 0.5);
            int count = iFloor(((jd - HEBREW_EPOCH) * 98496.0 / 35975351.0));
            year = count - 1;
            int i = count;
            while (jd >= hebrew_to_jd(i, 7, 1))
            {
                i++;
                year++;
            }
            int first = (jd < hebrew_to_jd(year, 1, 1) ? 7 : 1);
            month = first;
            i = first;
            while (jd > hebrew_to_jd(year, i, hebrew_month_days(year, i)))
            {
                i++;
                month++;
            }
            day = (int)Math.Truncate(jd - hebrew_to_jd(year, month, 1) + 1.0);
        }

        // Everything below is based on http://aa.quae.nl/en/reken/juliaansedag.html

        private static int getRunningMonthNumberOfTheFirstMonth(int year)
        {
            // It's `c1(x1)`.
            return downwardRounding(235 * year + 1, 19);
        }

        private static int getTheFirstDelay(int year)
        {
            // It's `v1(x1)`
            int c1x1 = getRunningMonthNumberOfTheFirstMonth(year);
            int qx1 = downwardRounding(c1x1, 1095);
            int rx1 = c1x1 - 1095 * qx1;
            int v1 = 15 * qx1 + 765433 * rx1 + 12084;
            v1 = downwardRounding(v1, 25920);
            return 32336 * qx1 + v1;
        }

        private static int getTheSecondDelay(int year)
        {
            // It's `v2(x1)`
            int v1x1 = getTheFirstDelay(year);
            int temp = v1x1 - 7 * downwardRounding(v1x1, 7);
            temp = downwardRounding(6 * temp, 7);
            temp = temp - 2 * downwardRounding(temp, 2);
            return v1x1 + temp;
        }

        private static int getTheThirdDelay(int year)
        {
            // It's `v3(x1)`
            int v3 = downwardRounding(getLengthOfYear(year) + 19, 15);
            v3 = v3 - 2 * downwardRounding(v3, 2);
            return 2 * v3;
        }

        private static int getTheFourthDelay(int year)
        {
            // It's `v4(x1)`
            int v4 = downwardRounding(getLengthOfYear(year) + 7, 15);
            return v4 - 2 * downwardRounding(v4, 2);
        }

        private static int getRunningDayNumberOfNewYear(int year)
        {
            // It's `c2(x1)`.
            return getTheSecondDelay(year) + getTheThirdDelay(year) +
                getTheFourthDelay(year - 1);
        }

        private static int getLengthOfYear(int year)
        {
            // It's `L2(x1)`
            return getTheSecondDelay(year + 1) - getTheSecondDelay(year);
        }

        public static int hebrew_to_jd3(int year, int month, int day)
        {
            int c0 = downwardRounding(13 - month, 7);
            int x1 = year - 1 + c0;
            int x3 = month - 1;
            int z4 = day - 1;
            int c2 = getRunningDayNumberOfNewYear(x1);
            int L = getRunningDayNumberOfNewYear(x1 + 1) - c2;
            int c8 = downwardRounding(L + 7, 2);
            c8 = c8 - 15 * downwardRounding(c8, 15);
            int c9 = downwardRounding(385 - L, 2);
            c9 = -(c9 - 15 * downwardRounding(c9, 15));
            int c3 = downwardRounding(384 * x3 + 7, 13) +
                c8 * downwardRounding(x3 + 4, 12) +
                c9 * downwardRounding(x3 + 3, 12);
            return (347821 + c2 + c3 + z4);
        }

        public static void jd_to_hebrew3(int jd, out int year, out int month,
                                         out int day)
        {
            int y4 = jd - 347821;
            int q = downwardRounding(y4, 1447);
            int r = y4 - 1447 * q;
            int gamma1 = 49 * q +
                downwardRounding(23 * q + 25920 * r + 13835, 765433) + 1;
            int xi1 = downwardRounding(19 * gamma1 + 17, 235);
            int mu1 = gamma1 - downwardRounding(235 * xi1 + 1, 19);
            int c2 = getRunningDayNumberOfNewYear(xi1);
            int L = getRunningDayNumberOfNewYear(xi1 + 1) - c2;
            int c8 = downwardRounding(L + 7, 2);
            c8 = c8 - 15 * downwardRounding(c8, 15);
            int c9 = downwardRounding(385 - L, 2);
            c9 = -(c9 - 15 * downwardRounding(c9, 15));
            int c3 = downwardRounding(384 * mu1 + 7, 13) +
                c8 * downwardRounding(mu1 + 4, 12) +
                c9 * downwardRounding(mu1 + 3, 12);
            int gamma2 = gamma1 + downwardRounding(y4 - (c2 + c3), 33);
            int xi2 = downwardRounding(19 * gamma2 + 17, 235);
            int mu2 = gamma2 - downwardRounding(235 * xi2 + 1, 19);
            c2 = getRunningDayNumberOfNewYear(xi2);
            L = getRunningDayNumberOfNewYear(xi2 + 1) - c2;
            c8 = downwardRounding(L + 7, 2);
            c8 = c8 - 15 * downwardRounding(c8, 15);
            c9 = downwardRounding(385 - L, 2);
            c9 = -(c9 - 15 * downwardRounding(c9, 15));
            c3 = downwardRounding(384 * mu2 + 7, 13) +
                c8 * downwardRounding(mu2 + 4, 12) +
                c9 * downwardRounding(mu2 + 3, 12);
            int gamma3 = gamma2 + downwardRounding(y4 - (c2 + c3), 33);
            int xi3 = downwardRounding(19 * gamma3 + 17, 235);
            int mu3 = gamma3 - downwardRounding(235 * xi3 + 1, 19);
            c2 = getRunningDayNumberOfNewYear(xi3);
            L = getRunningDayNumberOfNewYear(xi3 + 1) - c2;
            c8 = downwardRounding(L + 7, 2);
            c8 = c8 - 15 * downwardRounding(c8, 15);
            c9 = downwardRounding(385 - L, 2);
            c9 = -(c9 - 15 * downwardRounding(c9, 15));
            c3 = downwardRounding(384 * mu3 + 7, 13) +
                c8 * downwardRounding(mu3 + 4, 12) +
                c9 * downwardRounding(mu3 + 3, 12);
            int z4 = y4 - (c2 + c3);
            int c = downwardRounding(12 - mu3, 7);
            year = xi3 + 1 - c;
            month = mu3 + 1;
            day = z4 + 1;
        }

        #endregion

        #region Islamic calendar

        public static bool leap_islamic(int year)
        {
            return (year * 11 + 14) % 30 < 11;
        }

        private const double ISLAMIC_EPOCH = 1948439.5;

        // Based on "Fourmilab Calendar Converter"
        public static double islamic_to_jd(int year, int month, int day)
        {
            return (day + Math.Ceiling((29.5 * (month - 1))) + (year - 1) * 354 + Math.Floor(((3 + 11 * year) / 30.0)) + ISLAMIC_EPOCH - 1.0);
        }

        // Based on "Fourmilab Calendar Converter"
        public static void jd_to_islamic(double jd, out int year, out int month, out int day)
        {
            jd = (Math.Floor(jd) + 0.5);
            year = iFloor(((30.0 * (jd - ISLAMIC_EPOCH) + 10646.0) / 10631.0));
            month = Math.Min(12, iCeil(((jd - (29.0 + islamic_to_jd(year, 1, 1))) / 29.5)) + 1);
            day = (int)Math.Truncate(jd - islamic_to_jd(year, month, 1) + 1.0);
        }

        // Based on http://aa.quae.nl/en/reken/juliaansedag.html
        public static int islamic_to_jd3(int year, int month, int day)
        {
            year = downwardRounding(10631 * year - 10617, 30);
            month = downwardRounding(325 * month - 320, 11);
            return (year + month + day + 1948439);
        }

        // Based on http://aa.quae.nl/en/reken/juliaansedag.html
        public static void jd_to_islamic3(int jd, out int year, out int month,
                                          out int day)
        {
            int k2 = 30 * (jd - 1948440) + 15;
            int temp = k2 - 10631 * downwardRounding(k2, 10631);
            int k1 = downwardRounding(temp, 30) * 11 + 5;
            year = downwardRounding(k2, 10631) + 1;
            month = downwardRounding(k1, 325) + 1;
            temp = k1 - 325 * downwardRounding(k1, 325);
            day = downwardRounding(temp, 11) + 1;
        }

        #endregion

        #region Persian calendar

        private const double PERSIAN_EPOCH = 1948320.5;

        public static bool leap_persian(int year)
        {
            return ((year - (year > 0 ? 474 : 473)) % 2820 + 474 + 38) * 682 % 2816 < 682;
        }

        // Based on "Fourmilab Calendar Converter"
        public static double persian_to_jd(int year, int month, int day)
        {
            double epbase = year - (year >= 0 ? 474 : 473);
            double epyear = (474.0 + _modf(epbase, 2820.0));
            return (day + (month <= 7 ? (month - 1) * 31 : (month - 1) * 30 + 6) + Math.Floor(((epyear * 682.0 - 110.0) / 2816.0)) + (epyear - 1.0) * 365.0 + (Math.Floor((epbase / 2820.0)) * 1029983) + 1948319.5);
        }

        // Based on "Fourmilab Calendar Converter"
        public static void jd_to_persian(double jd, out int year, out int month, out int day)
        {
            jd = (Math.Floor(jd) + 0.5);
            double depoch = (jd - persian_to_jd(475, 1, 1));
            int cycle = iFloor((depoch / 1029983.0));
            int cyear = _modi(depoch, 1029983.0);
            int ycycle;
            if (cyear == 1029982)
            {
                ycycle = 2820;
            }
            else
            {
                int aux = iFloor((cyear / 366.0));
                int aux2 = _modi(cyear, 366.0);
                ycycle = iFloor(((2134 * aux + 2816 * aux2 + 2815) / 1028522.0)) + aux + 1;
            }
            year = ycycle + 2820 * cycle + 474;
            if (year <= 0)
            {
                year--;
            }
            double yday = (jd - persian_to_jd(year, 1, 1) + 1.0);
            month = (yday <= 186f ? iCeil((yday / 31.0)) : iCeil(((yday - 6.0) / 30.0)));
            day = (int)Math.Truncate(jd - persian_to_jd(year, month, 1) + 1.0);
        }

        #endregion

        #region Indian civil calendar

        // Based on "Fourmilab Calendar Converter"
        public static double indian_civil_to_jd(int year, int month, int day)
        {
            int gyear = year + 78;
            bool leap = leap_gregorian(gyear);
            double start = gregorian_to_jd(gyear, 3, (leap ? 21 : 22));
            int caitra = (leap ? 31 : 30);
            double jd;
            if (month == 1)
            {
                jd = (start + (day - 1));
            }
            else
            {
                jd = (start + caitra);
                int i = month - 2;
                i = Math.Min(i, 5);
                jd = (jd + (i * 31));
                if (month >= 8)
                {
                    i = month - 7;
                    jd = (jd + (i * 30));
                }
                jd = (jd + day - 1.0);
            }
            return jd;
        }

        // Based on "Fourmilab Calendar Converter"
        public static void jd_to_indian_civil(double jd, out int year, out int month, out int day)
        {
            const int saka = 78;
            const double start = 80.0;
            jd = (Math.Floor(jd) + 0.5);

            int gregY, gregM, gregD;
            jd_to_gregorian(jd, out gregY, out gregM, out gregD);

            bool leap = leap_gregorian(gregY);
            year = gregY - saka;
            double greg = gregorian_to_jd(gregY, 1, 1);
            double yday = (jd - greg);
            int caitra = (leap ? 31 : 30);
            if (yday < start)
            {
                year--;
                yday = (yday + caitra + 155.0 + 90.0 + 10.0 + start);
            }
            yday = (yday - start);
            if (yday < caitra)
            {
                month = 1;
                day = iFloor((yday + 1.0));
            }
            else
            {
                int mday = iFloor((yday - caitra));
                if (mday < 155)
                {
                    month = iFloor((mday / 31.0)) + 2;
                    day = mday % 31 + 1;
                }
                else
                {
                    mday -= 155;
                    month = iFloor((mday / 30.0)) + 7;
                    day = mday % 30 + 1;
                }
            }
        }

        #endregion

        #region Bahai calendar

        // Based on "Fourmilab Calendar Converter"
        public static double bahai_to_jd(int major, int cycle, int year, int month, int day)
        {
            int by, dummy;
            jd_to_gregorian(2394646.5, out by, out dummy, out dummy);

            int gy = 361 * (major - 1) + 19 * (cycle - 1) + (year - 1) + by;
            return (gregorian_to_jd(gy, 3, 20) + 19 * (month - 1) + (month != 20 ? 0 : (leap_gregorian(gy + 1) ? -14 : -15)) + day);
        }

        // Based on "Fourmilab Calendar Converter"
        public static void jd_to_bahai(double jd, out int major, out int cycle, out int year, out int month, out int day)
        {
            jd = (Math.Floor(jd) + 0.5);

            int gy, dummy;
            jd_to_gregorian(jd, out gy, out dummy, out dummy);

            int bstarty;
            jd_to_gregorian(2394646.5, out bstarty, out dummy, out dummy);

            int bys = gy - (bstarty + (gregorian_to_jd(gy, 1, 1) <= jd && jd <= gregorian_to_jd(gy, 3, 20) ? 1 : 0));
            major = iFloor((bys / 361.0)) + 1;
            cycle = iFloor((_modf(bys, 361.0) / 19.0)) + 1;
            year = _modi(bys, 19.0) + 1;
            double days = (jd - bahai_to_jd(major, cycle, year, 1, 1));
            double bld = bahai_to_jd(major, cycle, year, 20, 1);
            month = (jd >= bld ? 20 : iFloor((days / 19.0)) + 1);
            day = iFloor((jd + 1.0 - bahai_to_jd(major, cycle, year, month, 1)));
        }

        #endregion
    }
}
