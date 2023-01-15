/*
 *  Additional methods for calendar transformations:
 *  Copyright (C) 2016 by Ruslan Garipov.
 */

using System;

namespace GKCore.Calendar
{
    /// <summary>
    /// 
    /// </summary>
    public class CalendarAltConverter : CalendarConverter
    {
        #region Gregorian calendar

        // Based on http://aa.quae.nl/en/reken/juliaansedag.html
        public static int gregorian_to_jd3(int year, int month, int day)
        {
            int c0 = downwardRounding(month - 3, 12);
            int x4 = year + c0;
            int x3 = downwardRounding(x4, 100);
            int x2 = x4 - 100 * x3;
            int x1 = month - 3 - 12 * c0;
            return (downwardRounding(146097 * x3, 4) +
                    downwardRounding(36525 * x2, 100) +
                    downwardRounding(153 * x1 + 2, 5) + day + 1721119);
        }

        public static int gregorian_to_jd4(int year, int month, int day)
        {
            int c3 = downwardRounding(month - 3, 12);
            int x1 = month - 3 - 12 * c3;
            int z2 = downwardRounding(153 * x1 + 2, 5) + day - 1;
            int x2 = year + c3;
            int c2 = 365 * x2 + downwardRounding(x2, 4) -
                downwardRounding(x2, 100) + downwardRounding(x2, 400);
            return (c2 + z2 + 1721120);
        }

        public static int gregorian_to_jd5(int year, int month, int day)
        {
            int x2 = 12 * year + month - 1;
            int c2 = 30 * x2 + downwardRounding(7 * x2 + 5, 12) -
                2 * downwardRounding(x2 + 10, 12) +
                downwardRounding(x2 + 46, 48) -
                downwardRounding(x2 + 1198, 1200) +
                downwardRounding(x2 + 4798, 4800);
            return (c2 + day - 1 + 1721060);
        }

        // Based on http://aa.quae.nl/en/reken/juliaansedag.html
        public static void jd_to_gregorian3(int jd, out int year,
                                            out int month, out int day)
        {
            int sjd = (jd);
            int x3 = downwardRounding(4 * sjd - 6884477, 146097);
            int r3 = 4 * sjd - 6884477 - 146097 * x3;
            int temp = 100 * downwardRounding(r3, 4);
            int x2 = downwardRounding(temp + 99, 36525);
            int r2 = temp + 99 - 36525 * x2;
            temp = 5 * downwardRounding(r2, 100);
            int x1 = downwardRounding(temp + 2, 153);
            int r1 = temp + 2 - 153 * x1;
            day = downwardRounding(r1, 5) + 1;
            int c0 = downwardRounding(x1 + 2, 12);
            month = x1 - 12 * c0 + 3;
            year = 100 * x3 + x2 + c0;
        }

        public static void jd_to_gregorian4(int jd, out int year,
                                            out int month, out int day)
        {
            int sjd = (jd);
            int y2 = sjd - 1721120;
            int x2 = downwardRounding(400 * y2 + 799, 146097);
            int c2 = 365 * x2 + downwardRounding(x2, 4) -
                downwardRounding(x2, 100) + downwardRounding(x2, 400);
            int z2 = y2 - c2;
            int zeta = downwardRounding(z2, 367);
            x2 += zeta;
            c2 = 365 * x2 + downwardRounding(x2, 4) - downwardRounding(x2, 100) +
                downwardRounding(x2, 400);
            z2 = y2 - c2;
            int x1 = downwardRounding(5 * z2 + 2, 153);
            int c1 = downwardRounding(153 * x1 + 2, 5);
            int z1 = z2 - c1;
            int c0 = downwardRounding(x1 + 2, 12);
            year = x2 + c0;
            month = x1 - 12 * c0 + 3;
            day = z1 + 1;
        }

        public static void jd_to_gregorian5(int jd, out int year,
                                            out int month, out int day)
        {
            int sjd = (jd);
            int y2 = sjd - 1721060;
            int x2 = downwardRounding(4800 * y2 + 15793, 146097);
            int c2 = 30 * x2 + downwardRounding(7 * x2 + 5, 12) -
                2 * downwardRounding(x2 + 10, 12) +
                downwardRounding(x2 + 46, 48) -
                downwardRounding(x2 + 1198, 1200) +
                downwardRounding(x2 + 4798, 4800);
            int z2 = y2 - c2;
            int zeta = downwardRounding(z2, 33);
            x2 += zeta;
            c2 = 30 * x2 + downwardRounding(7 * x2 + 5, 12) -
                2 * downwardRounding(x2 + 10, 12) +
                downwardRounding(x2 + 46, 48) -
                downwardRounding(x2 + 1198, 1200) +
                downwardRounding(x2 + 4798, 4800);
            z2 = y2 - c2;
            int x1 = downwardRounding(x2, 12);
            int z1 = x2 - 12 * x1;
            year = x1;
            month = z1 + 1;
            day = z2 + 1;
        }

        #endregion

        #region Julian calendar

        // Based on http://aa.quae.nl/en/reken/juliaansedag.html
        public static int julian_to_jd3(int year, int month, int day)
        {
            int c0 = downwardRounding(month - 3, 12);
            int j1 = downwardRounding(1461 * (year + c0), 4);
            int j2 = downwardRounding(153 * month - 1836 * c0 - 457, 5);
            return (j1 + j2 + day + 1721117);
        }

        // Based on http://aa.quae.nl/en/reken/juliaansedag.html
        public static void jd_to_julian3(int jd, out int year, out int month,
                                         out int day)
        {
            int y2 = (jd) - 1721118;
            int k2 = 4 * y2 + 3;
            int temp = downwardRounding(k2, 1461);
            int k1 = 5 * downwardRounding(k2 - 1461 * temp, 4) + 2;
            int x1 = downwardRounding(k1, 153);
            int c0 = downwardRounding(x1 + 2, 12);
            year = downwardRounding(k2, 1461) + c0;
            month = x1 - 12 * c0 + 3;
            temp = downwardRounding(k1, 153);
            day = downwardRounding(k1 - 153 * temp, 5) + 1;
        }

        #endregion
    }
}
