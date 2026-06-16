/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using BenchmarkDotNet.Attributes;
using GKCore.Calendar;

namespace GKBenchmarks;

[Config(typeof(BenchmarkConfig))]
public class CalendarConverterBenchmark
{
    #region Gregorian Calendar Benchmarks

    // Test dates for Gregorian calendar
    private const int GREGORIAN_YEAR = 2023;
    private const int GREGORIAN_MONTH = 6;
    private const int GREGORIAN_DAY = 15;
    private const double GREGORIAN_JD = 2460105.5; // Julian day for 2023-06-15

    [Benchmark]
    public double GregorianToJD()
    {
        return CalendarConverter.gregorian_to_jd(GREGORIAN_YEAR, GREGORIAN_MONTH, GREGORIAN_DAY);
    }

    [Benchmark]
    public void JDToGregorian()
    {
        int year, month, day;
        CalendarConverter.jd_to_gregorian(GREGORIAN_JD, out year, out month, out day);
    }

    [Benchmark]
    public int GregorianToJD2()
    {
        return CalendarConverter.gregorian_to_jd2(GREGORIAN_YEAR, GREGORIAN_MONTH, GREGORIAN_DAY);
    }

    [Benchmark]
    public void JDToGregorian2()
    {
        int year, month, day;
        CalendarConverter.jd_to_gregorian2((int)GREGORIAN_JD, out year, out month, out day);
    }

    #endregion

    #region Julian Calendar Benchmarks

    // Test dates for Julian calendar
    private const int JULIAN_YEAR = 2023;
    private const int JULIAN_MONTH = 6;
    private const int JULIAN_DAY = 28;

    [Benchmark]
    public double JulianToJD()
    {
        return CalendarConverter.julian_to_jd(JULIAN_YEAR, JULIAN_MONTH, JULIAN_DAY);
    }

    [Benchmark]
    public void JDToJulian()
    {
        int year, month, day;
        CalendarConverter.jd_to_julian(GREGORIAN_JD, out year, out month, out day);
    }

    [Benchmark]
    public int JulianToJD2()
    {
        return CalendarConverter.julian_to_jd2(JULIAN_YEAR, JULIAN_MONTH, JULIAN_DAY);
    }

    [Benchmark]
    public void JDToJulian2()
    {
        int year, month, day;
        CalendarConverter.jd_to_julian2((int)GREGORIAN_JD, out year, out month, out day);
    }

    #endregion

    #region Hebrew Calendar Benchmarks

    // Test dates for Hebrew calendar
    private const int HEBREW_YEAR = 5783;
    private const int HEBREW_MONTH = 4;
    private const int HEBREW_DAY = 7;

    [Benchmark]
    public double HebrewToJD()
    {
        return CalendarConverter.hebrew_to_jd(HEBREW_YEAR, HEBREW_MONTH, HEBREW_DAY);
    }

    [Benchmark]
    public void JDToHebrew()
    {
        int year, month, day;
        CalendarConverter.jd_to_hebrew(GREGORIAN_JD, out year, out month, out day);
    }

    [Benchmark]
    public int HebrewToJD3()
    {
        return CalendarConverter.hebrew_to_jd3(HEBREW_YEAR, HEBREW_MONTH, HEBREW_DAY);
    }

    [Benchmark]
    public void JDToHebrew3()
    {
        int year, month, day;
        CalendarConverter.jd_to_hebrew3((int)GREGORIAN_JD, out year, out month, out day);
    }

    #endregion

    #region Islamic Calendar Benchmarks

    // Test dates for Islamic calendar
    private const int ISLAMIC_YEAR = 1444;
    private const int ISLAMIC_MONTH = 12;
    private const int ISLAMIC_DAY = 26;

    [Benchmark]
    public double IslamicToJD()
    {
        return CalendarConverter.islamic_to_jd(ISLAMIC_YEAR, ISLAMIC_MONTH, ISLAMIC_DAY);
    }

    [Benchmark]
    public void JDToIslamic()
    {
        int year, month, day;
        CalendarConverter.jd_to_islamic(GREGORIAN_JD, out year, out month, out day);
    }

    [Benchmark]
    public int IslamicToJD3()
    {
        return CalendarConverter.islamic_to_jd3(ISLAMIC_YEAR, ISLAMIC_MONTH, ISLAMIC_DAY);
    }

    [Benchmark]
    public void JDToIslamic3()
    {
        int year, month, day;
        CalendarConverter.jd_to_islamic3((int)GREGORIAN_JD, out year, out month, out day);
    }

    #endregion

    #region Persian Calendar Benchmarks
    /*
    // Test dates for Persian calendar
    private const int PERSIAN_YEAR = 1402;
    private const int PERSIAN_MONTH = 3;
    private const int PERSIAN_DAY = 25;
    
    [Benchmark]
    public double PersianToJD()
    {
        return CalendarConverter.persian_to_jd(PERSIAN_YEAR, PERSIAN_MONTH, PERSIAN_DAY);
    }

    [Benchmark]
    public void JDToPersian()
    {
        int year, month, day;
        CalendarConverter.jd_to_persian(GREGORIAN_JD, out year, out month, out day);
    }
    */
    #endregion

    #region Indian Civil Calendar Benchmarks
    /*
    // Test dates for Indian civil calendar
    private const int INDIAN_YEAR = 1945;
    private const int INDIAN_MONTH = 3;
    private const int INDIAN_DAY = 7;

    [Benchmark]
    public double IndianCivilToJD()
    {
        return CalendarConverter.indian_civil_to_jd(INDIAN_YEAR, INDIAN_MONTH, INDIAN_DAY);
    }

    [Benchmark]
    public void JDToIndianCivil()
    {
        int year, month, day;
        CalendarConverter.jd_to_indian_civil(GREGORIAN_JD, out year, out month, out day);
    }
    */
    #endregion
}
