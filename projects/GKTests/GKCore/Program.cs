#define LEFT_AND_RIGHT_BORDERS
#define TOP_AND_BOTTOM_BORDERS

using System;
using System.Collections.Generic;
using BSLib.Calendar;

namespace UDNTest
{
    public class UDNRecord
    {
        public readonly UDNCalendarType Calendar;
        public readonly string Description;
        public readonly UDN Value;

        public UDNRecord(UDNCalendarType calendar, int year, int month, int day, string description)
        {
            Calendar = calendar;
            Description = description;
            Value = new UDN(calendar, year, month, day);
        }

        public UDNRecord(UDN udn, UDNCalendarType calendar, string description)
        {
            Calendar = calendar;
            Description = description;
            Value = udn;
        }
    }

    public struct UDNDate
    {
        public UDNCalendarType Calendar;
        public int Year;
        public int Month;
        public int Day;

        public UDNDate(UDNCalendarType calendar, int year, int month, int day)
        {
            Calendar = calendar;
            Year = year;
            Month = month;
            Day = day;
        }
    }

    class Program
    {
        private static List<UDNRecord> fDates = new List<UDNRecord>();

        public static void Main(string[] args)
        {
            Console.WriteLine("Unified Date Numbers Test");
            Console.WriteLine();

            fDates.Add(new UDNRecord(UDNCalendarType.ctGregorian, 2016, 05, 05, "2016/05/05 [g]"));
            fDates.Add(new UDNRecord(UDNCalendarType.ctGregorian, 2016, 05, 04, "2016/05/04 [g]"));

            fDates.Add(new UDNRecord(UDNCalendarType.ctJulian, 2016, 04, 21, "2016/05/04 [g] = 2016/04/21 [j]"));
            fDates.Add(new UDNRecord(UDNCalendarType.ctJulian, 2016, 04, 23, "2016/05/06 [g] = 2016/04/23 [j]"));

            fDates.Add(new UDNRecord(UDNCalendarType.ctGregorian, 2016, 05, UDN.UnknownDay, "2016/05/?? [g]")); // must be first
            fDates.Add(new UDNRecord(UDNCalendarType.ctGregorian, 2016, 06, UDN.UnknownDay, "2016/06/?? [g]")); // must be last

            fDates.Add(new UDNRecord(UDNCalendarType.ctGregorian, UDN.UnknownYear, UDN.UnknownMonth, UDN.UnknownDay, "??/??/?? [g]"));
            fDates.Add(new UDNRecord(UDNCalendarType.ctGregorian, UDN.UnknownYear, 04, 23, "??/04/23 [g]"));
            fDates.Add(new UDNRecord(UDNCalendarType.ctGregorian, UDN.UnknownYear, 03, 23, "??/03/23 [g]"));
            fDates.Add(new UDNRecord(UDNCalendarType.ctGregorian, UDN.UnknownYear, UDN.UnknownMonth, 23, "??/??/23 [g]"));
            fDates.Add(new UDNRecord(UDNCalendarType.ctGregorian, 2016, UDN.UnknownMonth, UDN.UnknownDay, "2016/??/?? [g]"));
            fDates.Add(new UDNRecord(UDNCalendarType.ctGregorian, 2016, UDN.UnknownMonth, 10, "2016/??/10 [g]"));
            fDates.Add(new UDNRecord(UDNCalendarType.ctGregorian, 2015, 03, 23, "2015/03/23 [g]"));
            fDates.Add(new UDNRecord(UDNCalendarType.ctGregorian, 2014, UDN.UnknownMonth, 23, "2014/??/23 [g]"));
            fDates.Add(new UDNRecord(UDNCalendarType.ctGregorian, 2016, 05, 31, "2016/05/31 [g]"));
            fDates.Add(new UDNRecord(UDNCalendarType.ctGregorian, 2016, 05, 31, "2016/05/31 [g]"));
            fDates.Add(new UDNRecord(UDNCalendarType.ctGregorian, -4712, 1, 2, "-4712/01/02 [g]"));
            fDates.Add(new UDNRecord(UDNCalendarType.ctGregorian, -4712, 1, 3, "-4712/01/03 [g]"));

            fDates.Add(new UDNRecord(UDNCalendarType.ctHebrew, 5564, 04, 04, "1804/06/13 [g] = 5564/04/04 [h]"));
            fDates.Add(new UDNRecord(UDNCalendarType.ctIslamic, 1216, 01, 04, "1801/05/17 [g] = 1216/01/04 [i]"));
            fDates.Add(new UDNRecord(UDNCalendarType.ctGregorian, 1802, 05, 01, "1802/05/01 [g]"));

            fDates.Add(new UDNRecord(UDNCalendarType.ctGregorian, 0, 1, 3, "0000/01/03 [g]"));
            fDates.Add(new UDNRecord(UDNCalendarType.ctGregorian, -1, 1, 3, "-0001/01/03 [g]"));

            fDates.Add(new UDNRecord(UDNCalendarType.ctGregorian, 1, 1, 3, "0001/01/03 [g]"));
            fDates.Add(new UDNRecord(UDNCalendarType.ctGregorian, 2015, 2, 27, "2015/02/27 [g]"));
            fDates.Add(new UDNRecord(UDNCalendarType.ctGregorian, 3268, 1, 23, "3268/01/23 [g]"));

            // Add dates before.
            fDates.Add(new UDNRecord(UDN.CreateBefore(
                UDNCalendarType.ctGregorian, 1, 1, 4), UDNCalendarType.ctGregorian, "before 0001/01/04 [g]"));
            fDates.Add(new UDNRecord(UDN.CreateBefore(
                UDNCalendarType.ctGregorian, 2016, 05, 31), UDNCalendarType.ctGregorian, "before 2016/05/31 [g]"));
            fDates.Add(new UDNRecord(UDN.CreateBefore(
                UDNCalendarType.ctGregorian, -4712, 1, 2), UDNCalendarType.ctGregorian, "before -4712/01/02 [g]"));
            fDates.Add(new UDNRecord(UDN.CreateBefore(
                UDNCalendarType.ctGregorian, UDN.UnknownYear, 05, 31), UDNCalendarType.ctGregorian, "before ????/05/31 [g]"));
            fDates.Add(new UDNRecord(UDN.CreateBefore(
                UDNCalendarType.ctGregorian, 2015, UDN.UnknownMonth, 31), UDNCalendarType.ctGregorian, "before 2015/??/31 [g]"));
            // Add dates after.
            fDates.Add(new UDNRecord(UDN.CreateAfter(
                UDNCalendarType.ctGregorian, 2016, 05, 31), UDNCalendarType.ctGregorian, "after 2016/05/31 [g]"));
            fDates.Add(new UDNRecord(UDN.CreateAfter(
                UDNCalendarType.ctGregorian, UDN.UnknownYear, 05, 31), UDNCalendarType.ctGregorian, "after ????/05/31 [g]"));
            fDates.Add(new UDNRecord(UDN.CreateAfter(
                UDNCalendarType.ctGregorian, UDN.UnknownYear, 06, 15), UDNCalendarType.ctGregorian, "after ????/06/15 [g]"));
            fDates.Add(new UDNRecord(UDN.CreateAfter(
                UDNCalendarType.ctGregorian, 2015, UDN.UnknownMonth, 31), UDNCalendarType.ctGregorian, "after 2015/??/31 [g]"));
            fDates.Add(new UDNRecord(UDN.CreateAfter(
                UDNCalendarType.ctGregorian, 2015, UDN.UnknownMonth, 30), UDNCalendarType.ctGregorian, "after 2015/??/30 [g]"));
            // Add approximate dates.
            fDates.Add(new UDNRecord(UDN.CreateApproximate(
                UDNCalendarType.ctGregorian, 2016, 05, 31), UDNCalendarType.ctGregorian, "~ 2016/05/31 [g]"));
            fDates.Add(new UDNRecord(UDN.CreateApproximate(
                UDNCalendarType.ctGregorian, 1, 1, 4), UDNCalendarType.ctGregorian, "~ 0001/01/04 [g]"));
            fDates.Add(new UDNRecord(UDN.CreateApproximate(
                UDNCalendarType.ctGregorian, 1, 1, UDN.UnknownDay), UDNCalendarType.ctGregorian, "~ 0001/01/?? [g]"));
            fDates.Add(new UDNRecord(UDN.CreateApproximate(
                UDNCalendarType.ctGregorian, UDN.UnknownYear, 05, 31), UDNCalendarType.ctGregorian, "~ ????/05/31 [g]"));
            fDates.Add(new UDNRecord(UDN.CreateApproximate(
                UDNCalendarType.ctGregorian, 2015, UDN.UnknownMonth, 31), UDNCalendarType.ctGregorian, "~ 2015/??/31 [g]"));
            fDates.Add(new UDNRecord(UDN.CreateApproximate(
                UDNCalendarType.ctGregorian, 2015, 2, 28), UDNCalendarType.ctGregorian, "~ 2015/02/28 [g]"));

            fDates.Sort(delegate(UDNRecord left, UDNRecord right) { return left.Value.CompareTo(right.Value); });

            Console.WriteLine("Check UDNs ordering");
            int[] widths = {16, 16, 12, 32};
            string format =
                #if LEFT_AND_RIGHT_BORDERS
                string.Format("{{4}} {{0, {0}}} {{4}} {{1, {1}}} {{4}} {{2, {2}}} {{4}} {{3, {3}}} {{4}}", widths[0], widths[1], widths[2], widths[3]);
            #else
            string.Format("{{0, {0}}} {{4}} {{1, {1}}} {{4}} {{2, {2}}} {{4}} {{3, {3}}}", widths[0], widths[1], widths[2], widths[3]);
            #endif
            Object[] a =
            {
                new string('-', widths[0]),
                new string('-', widths[1]),
                new string('-', widths[2]),
                new string('-', widths[3]),
                new string('+', 1)
            };
            string delimiter = string.Format(format, a);
            #if TOP_AND_BOTTOM_BORDERS
            Console.WriteLine(delimiter);
            #endif
            a = new Object[] {"Value", "Unmasked value", "Calendar", "Description", "|"};
            Console.WriteLine(format, a);
            Console.WriteLine(delimiter);
            foreach (UDNRecord udn_rec in fDates)
            {
                a = new Object[] {udn_rec.Value, udn_rec.Value.GetUnmaskedValue(), udn_rec.Calendar.ToString(), udn_rec.Description, "|"};
                Console.WriteLine(format, a);
            }
            #if TOP_AND_BOTTOM_BORDERS
            Console.WriteLine(delimiter);
            #endif

            Console.WriteLine("\nCheck 'UDN *between* dates'");
            widths = new int[] {32, 32, 48};
            format =
                #if LEFT_AND_RIGHT_BORDERS
                string.Format("{{3}} {{0, {0}}} {{3}} {{1, {1}}} {{3}} {{2, {2}}} {{3}}", widths[0], widths[1], widths[2]);
            #else
            string.Format("{{0, {0}}} {{3}} {{1, {1}}} {{3}} {{2, {2}}}}", widths[0], widths[1], widths[2]);
            #endif
            a = new Object[]
            {
                new string('-', widths[0]),
                new string('-', widths[1]),
                new string('-', widths[2]),
                new string('+', 1)
            };
            delimiter = string.Format(format, a);
            #if TOP_AND_BOTTOM_BORDERS
            Console.WriteLine(delimiter);
            #endif
            a = new Object[] {"Left", "Right", "Between 'Left' and 'Right'", "|"};
            Console.WriteLine(format, a);
            Console.WriteLine(delimiter);
            for (int i = 0; i < fDates.Count - 1; i++)
            {
                string between;
                try
                {
                    UDN foo = UDN.CreateBetween(fDates[i].Value, fDates[i + 1].Value);
                    // Always use Gregorian calendar to show "between-date".
                    int year;
                    int month;
                    int day;
                    CalendarConverter.jd_to_gregorian(foo.GetUnmaskedValue(), out year, out month, out day);
                    // If 'left' or 'right' dates ain't in the Gregorian calendar, show "+f".
                    bool forced =
                        (UDNCalendarType.ctGregorian != fDates[i].Calendar) ||
                        (UDNCalendarType.ctGregorian != fDates[i + 1].Calendar);
                    between = string.Format(
                        "{0}/{1}/{2} [g{3}]",
                        year,
                        foo.HasKnownMonth() ? month.ToString() : "??",
                        foo.HasKnownDay() ? day.ToString() : "??",
                        forced ? "+f" : "");
                }
                catch (Exception e)
                {
                    between = e.Message;
                }
                a = new Object[] {fDates[i].Description, fDates[i + 1].Description, between, "|"};
                Console.WriteLine(format, a);
            }
            #if TOP_AND_BOTTOM_BORDERS
            Console.WriteLine(delimiter);
            #endif

            List<UDNDate> dates = new List<UDNDate>();
            dates.Add(new UDNDate(UDNCalendarType.ctGregorian, -4713, 11, 24));
            dates.Add(new UDNDate(UDNCalendarType.ctGregorian, -4712, 1, 2));
            dates.Add(new UDNDate(UDNCalendarType.ctGregorian, -4712, 1, 3));
            dates.Add(new UDNDate(UDNCalendarType.ctGregorian, -4700, 2, 28));
            dates.Add(new UDNDate(UDNCalendarType.ctGregorian, -4528, 2, 29));
            dates.Add(new UDNDate(UDNCalendarType.ctGregorian, -4528, 2, 28));
            dates.Add(new UDNDate(UDNCalendarType.ctGregorian, -4529, 2, 28));
            dates.Add(new UDNDate(UDNCalendarType.ctGregorian, -1, 1, 25));
            dates.Add(new UDNDate(UDNCalendarType.ctGregorian, 0, 1, 25));
            dates.Add(new UDNDate(UDNCalendarType.ctGregorian, 1, 1, 25));
            dates.Add(new UDNDate(UDNCalendarType.ctGregorian, 2015, 5, 30));
            dates.Add(new UDNDate(UDNCalendarType.ctGregorian, 2000, 2, 29));
            dates.Add(new UDNDate(UDNCalendarType.ctGregorian, 2016, 5, 30));
            dates.Add(new UDNDate(UDNCalendarType.ctGregorian, 2016, 5, 31));
            dates.Add(new UDNDate(UDNCalendarType.ctGregorian, 2016, 6, 30));
            dates.Add(new UDNDate(UDNCalendarType.ctGregorian, 2016, 7, 1));
            dates.Add(new UDNDate(UDNCalendarType.ctGregorian, 2016, 7, 15));
            dates.Add(new UDNDate(UDNCalendarType.ctGregorian, 2016, 7, 31));
            dates.Add(new UDNDate(UDNCalendarType.ctJulian, -1, 10, 15));
            dates.Add(new UDNDate(UDNCalendarType.ctJulian, 0, 10, 15));
            dates.Add(new UDNDate(UDNCalendarType.ctJulian, 1, 10, 15));
            dates.Add(new UDNDate(UDNCalendarType.ctJulian, 2013, 10, 15));
            dates.Add(new UDNDate(UDNCalendarType.ctJulian, 2016, 1, 31));
            dates.Add(new UDNDate(UDNCalendarType.ctJulian, 2016, 2, 29));
            dates.Add(new UDNDate(UDNCalendarType.ctIslamic, 1432, 8, 29));
            dates.Add(new UDNDate(UDNCalendarType.ctIslamic, 2000, 2, 29));
            dates.Add(new UDNDate(UDNCalendarType.ctIslamic, 2016, 8, 29));
            dates.Add(new UDNDate(UDNCalendarType.ctHebrew, 4682, 3, 18));
            dates.Add(new UDNDate(UDNCalendarType.ctHebrew, 4684, 7, 1));
            dates.Add(new UDNDate(UDNCalendarType.ctHebrew, 3761, 10, 18));
            dates.Add(new UDNDate(UDNCalendarType.ctHebrew, 3761, 12, 1));
            dates.Add(new UDNDate(UDNCalendarType.ctHebrew, 3761, 12, 2));
            dates.Add(new UDNDate(UDNCalendarType.ctHebrew, 3762, 10, 29));
            dates.Add(new UDNDate(UDNCalendarType.ctHebrew, 3762, 11, 1));
            dates.Add(new UDNDate(UDNCalendarType.ctHebrew, 3762, 12, 2));
            dates.Add(new UDNDate(UDNCalendarType.ctHebrew, 3762, 13, 29));
            dates.Add(new UDNDate(UDNCalendarType.ctHebrew, 3763, 1, 1));

            Console.WriteLine("\nCheck does a JDN algorithm make reversible dates");
            widths = new int[] {32, 48, 35};
            format =
                #if LEFT_AND_RIGHT_BORDERS
                string.Format("{{3}} {{0, {0}}} {{3}} {{1, {1}}} {{3}} {{2, {2}}} {{3}}", widths[0], widths[1], widths[2]);
            #else
            string.Format("{{0, {0}}} {{3}} {{1, {1}}} {{3}} {{2, {2}}}}", widths[0], widths[1], widths[2]);
            #endif
            a = new Object[]
            {
                new string('-', widths[0]),
                new string('-', widths[1]),
                new string('-', widths[2]),
                new string('+', 1)
            };
            delimiter = string.Format(format, a);
            #if TOP_AND_BOTTOM_BORDERS
            Console.WriteLine(delimiter);
            #endif
            a = new Object[] { "Date", "JDN", "Date restored from JDN", "|" };
            Console.WriteLine(format, a);
            Console.WriteLine(delimiter);
            for (int i = 0; i < dates.Count; i++)
            {
                UDNDate d = dates[i];
                string original = string.Format("{0}/{1}/{2}", d.Year, d.Month, d.Day);
                int year;
                int month;
                int day;
                string recDate;
                char ok;
                if (UDNCalendarType.ctGregorian == d.Calendar)
                {
                    int jdn;
                    string reconstructed;
                    try
                    {
                        jdn = CalendarAltConverter.gregorian_to_jd5(d.Year, d.Month, d.Day);
                        CalendarAltConverter.jd_to_gregorian5(jdn, out year, out month, out day);
                        recDate = string.Format("{0}/{1}/{2}", year, month, day);
                        ok = (recDate == original) ? '+' : 'o';
                        reconstructed = string.Format("{0}/{1}/{2} (must be {3})", year, month, day, original);
                    }
                    catch (Exception)
                    {
                        jdn = 0;
                        ok = 'o';
                        reconstructed = "**** error";
                    }
                    a = new Object[] {original, "by `gregorian_to_jd5`: " + jdn.ToString(), reconstructed + " " + ok, "|"};
                    Console.WriteLine(format, a);
                    jdn = CalendarAltConverter.gregorian_to_jd4(d.Year, d.Month, d.Day);
                    CalendarAltConverter.jd_to_gregorian4(jdn, out year, out month, out day);
                    recDate = string.Format("{0}/{1}/{2}", year, month, day);
                    ok = (recDate == original) ? '+' : 'o';
                    reconstructed = string.Format("{0}/{1}/{2} (must be {3})", year, month, day, original);
                    a = new Object[] {original, "by `gregorian_to_jd4`: " + jdn.ToString(), reconstructed + " " + ok, "|"};
                    Console.WriteLine(format, a);
                    try
                    {
                        jdn = CalendarAltConverter.gregorian_to_jd3(d.Year, d.Month, d.Day);
                        CalendarAltConverter.jd_to_gregorian3(jdn, out year, out month, out day);
                        recDate = string.Format("{0}/{1}/{2}", year, month, day);
                        ok = (recDate == original) ? '+' : 'o';
                        reconstructed = string.Format("{0}/{1}/{2} (must be {3})", year, month, day, original);
                    }
                    catch (Exception)
                    {
                        jdn = 0;
                        ok = 'o';
                        reconstructed = "**** error";
                    }
                    a = new Object[] {original, "by `gregorian_to_jd3`: " + jdn.ToString(), reconstructed + " " + ok, "|"};
                    Console.WriteLine(format, a);
                    jdn = CalendarConverter.gregorian_to_jd2(d.Year, d.Month, d.Day);
                    CalendarConverter.jd_to_gregorian2(jdn, out year, out month, out day);
                    recDate = string.Format("{0}/{1}/{2}", year, month, day);
                    ok = (recDate == original) ? '+' : 'o';
                    reconstructed = string.Format("{0}/{1}/{2} (must be {3})", year, month, day, original);
                    a = new Object[] {original, "by `gregorian_to_jd2`:  " + jdn.ToString(), reconstructed + " " + ok, "|"};
                    Console.WriteLine(format, a);
                    jdn = (int)CalendarConverter.gregorian_to_jd(d.Year, d.Month, d.Day);
                    CalendarConverter.jd_to_gregorian(jdn, out year, out month, out day);
                    recDate = string.Format("{0}/{1}/{2}", year, month, day);
                    ok = (recDate == original) ? '+' : 'o';
                    reconstructed = string.Format("{0}/{1}/{2} (must be {3})", year, month, day, original);
                    a = new Object[] {original, "by `gregorian_to_jd`:  " + jdn.ToString(), reconstructed + " " + ok, "|"};
                    Console.WriteLine(format, a);
                }
                else if (UDNCalendarType.ctJulian == d.Calendar)
                {
                    int jdn = CalendarAltConverter.julian_to_jd3(d.Year, d.Month, d.Day);
                    CalendarAltConverter.jd_to_julian3(jdn, out year, out month, out day);
                    recDate = string.Format("{0}/{1}/{2}", year, month, day);
                    ok = (recDate == original) ? '+' : 'o';
                    string reconstructed = string.Format("{0}/{1}/{2} (must be {3})", year, month, day, original);
                    a = new Object[] {original, "by `julian_to_jd3`: " + jdn.ToString(), reconstructed + " " + ok, "|"};
                    Console.WriteLine(format, a);
                    jdn = (int)CalendarConverter.julian_to_jd(d.Year, d.Month, d.Day);
                    CalendarConverter.jd_to_julian(jdn, out year, out month, out day);
                    recDate = string.Format("{0}/{1}/{2}", year, month, day);
                    ok = (recDate == original) ? '+' : 'o';
                    reconstructed = string.Format("{0}/{1}/{2} (must be {3})", year, month, day, original);
                    a = new Object[] {original, "by `julian_to_jd`:  " + jdn.ToString(), reconstructed + " " + ok, "|"};
                    Console.WriteLine(format, a);
                }
                else if (UDNCalendarType.ctIslamic == d.Calendar)
                {
                    int jdn = CalendarConverter.islamic_to_jd3(d.Year, d.Month, d.Day);
                    CalendarConverter.jd_to_islamic3(jdn, out year, out month, out day);
                    recDate = string.Format("{0}/{1}/{2}", year, month, day);
                    ok = (recDate == original) ? '+' : 'o';
                    string reconstructed = string.Format("{0}/{1}/{2} (must be {3})", year, month, day, original);
                    a = new Object[] {original, "by `islamic_to_jd3`: " + jdn.ToString(), reconstructed + " " + ok, "|"};
                    Console.WriteLine(format, a);
                    jdn = (int)CalendarConverter.islamic_to_jd(d.Year, d.Month, d.Day);
                    CalendarConverter.jd_to_islamic(jdn, out year, out month, out day);
                    recDate = string.Format("{0}/{1}/{2}", year, month, day);
                    ok = (recDate == original) ? '+' : 'o';
                    reconstructed = string.Format("{0}/{1}/{2} (must be {3})", year, month, day, original);
                    a = new Object[] {original, "by `islamic_to_jd`:  " + jdn.ToString(), reconstructed + " " + ok, "|"};
                    Console.WriteLine(format, a);
                }
                else if (UDNCalendarType.ctHebrew == d.Calendar)
                {
                    int jdn = CalendarConverter.hebrew_to_jd3(d.Year, d.Month, d.Day);
                    CalendarConverter.jd_to_hebrew3(jdn, out year, out month, out day);
                    recDate = string.Format("{0}/{1}/{2}", year, month, day);
                    ok = (recDate == original) ? '+' : 'o';
                    string reconstructed = string.Format("{0}/{1}/{2} (must be {3})", year, month, day, original);
                    a = new Object[] {original, "by `hebrew_to_jd3`: " + jdn.ToString(), reconstructed + " " + ok, "|"};
                    Console.WriteLine(format, a);
                    jdn = (int)CalendarConverter.hebrew_to_jd(d.Year, d.Month, d.Day);
                    CalendarConverter.jd_to_hebrew(jdn, out year, out month, out day);
                    recDate = string.Format("{0}/{1}/{2}", year, month, day);
                    ok = (recDate == original) ? '+' : 'o';
                    reconstructed = string.Format("{0}/{1}/{2} (must be {3})", year, month, day, original);
                    a = new Object[] {original, "by `hebrew_to_jd`:  " + jdn.ToString(), reconstructed + " " + ok, "|"};
                    Console.WriteLine(format, a);
                }
                if (dates.Count - 1 > i)
                {
                    Console.WriteLine(delimiter);
                }
            }
            #if TOP_AND_BOTTOM_BORDERS
            Console.WriteLine(delimiter);
            #endif

            Console.WriteLine();
            Console.Write("Press any key to continue . . . ");
            Console.ReadKey(true);

            double jdnB = CalendarConverter.gregorian_to_jd(2022, 10, 15);
            int yB, mB, dB;
            CalendarConverter.jd_to_byzantine(jdnB, out yB, out mB, out dB, CalendarConverter.ByzantineStyle.March);
            Console.WriteLine(string.Format("{0}/{1}/{2}", yB, mB, dB));

            CalendarConverter.jd_to_byzantine(jdnB, out yB, out mB, out dB, CalendarConverter.ByzantineStyle.September);
            Console.WriteLine(string.Format("{0}/{1}/{2}", yB, mB, dB));

            Console.ReadKey(true);
        }
    }
}
