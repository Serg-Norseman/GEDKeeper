using System;
using NUnit.Framework;

namespace BSLib.Calendar
{
    [TestFixture]
    public class CalendarTests
    {
        private static string d2s(int day, string month, int year, string weekday)
        {
            return string.Format("{0} {1} {2}, {3}", day, month, year, weekday);
        }

        [Test]
        public void Test_Common()
        {
            DateTime gdt = new DateTime(1990, 10, 10);
            string s;

            double jd = CalendarConverter.gregorian_to_jd2(gdt.Year, gdt.Month, gdt.Day);
            Assert.AreEqual(2448175, jd); //

            jd = CalendarConverter.gregorian_to_jd2(-4713, 11, 24);
            Assert.AreEqual(0.0, jd); //


            jd = CalendarConverter.gregorian_to_jd(-4713, 11, 24);
            Assert.AreEqual(0.5, jd); // bad result! needs 0.0f!

            jd = CalendarConverter.gregorian_to_jd(gdt.Year, gdt.Month, gdt.Day);
            Assert.AreEqual(2448174.5, jd); // not checked!


            int year, month, day;
            CalendarConverter.jd_to_julian(jd, out year, out month, out day);
            s = d2s(day, CalendarData.ClassicMonths[month - 1], year, "");
            Assert.AreEqual("27 September 1990, ", s); // +

            CalendarConverter.jd_to_hebrew(jd, out year, out month, out day);
            s = d2s(day, CalendarData.HebrewMonths[month - 1], year, CalendarData.HebrewWeekdays[CalendarConverter.jwday(jd)]);
            Assert.AreEqual("21 Tishri 5751, Dalet", s); // +

            CalendarConverter.jd_to_islamic(jd, out year, out month, out day);
            s = d2s(day, CalendarData.IslamicMonths[month - 1], year, CalendarData.IslamicWeekdays[CalendarConverter.jwday(jd)]);
            Assert.AreEqual("20 Rabi`al-Awwal 1411, Al-'arb`a'", s); // +

            CalendarConverter.jd_to_persian(jd, out year, out month, out day);
            s = d2s(day, CalendarData.PersianMonths[month - 1], year, CalendarData.PersianWeekdays[CalendarConverter.jwday(jd)]);
            Assert.AreEqual("18 Mehr 1369, Chaharshanbeh", s); // +

            CalendarConverter.jd_to_indian_civil(jd, out year, out month, out day);
            s = d2s(day, CalendarData.IndianCivilMonths[month - 1], year, CalendarData.IndianCivilWeekdays[CalendarConverter.jwday(jd)]);
            Assert.AreEqual("18 Asvina 1912, Budhavara", s); // +

            int major, cycle;
            CalendarConverter.jd_to_bahai(jd, out major, out cycle, out year, out month, out day);
            s = "major " + major.ToString() + ", cycle " + cycle.ToString() + ", ";
            s = s + d2s(day, CalendarData.BahaiMonths[month - 1], year, CalendarData.BahaiWeekdays[CalendarConverter.jwday(jd)]);
            Assert.AreEqual("major 1, cycle 8, 14 Mashíyyat 14, ‘Idál", s); // ???
        }

        [Test]
        public void Test_CP()
        {
            DateTime gdt = new DateTime(2016, 11, 28);

            string s;
            double jd;
            int year, month, day;

            jd = CalendarConverter.gregorian_to_jd2(gdt.Year, gdt.Month, gdt.Day);
            Assert.AreEqual(2457721, jd); // ok+

            CalendarConverter.jd_to_julian2((int)jd, out year, out month, out day);
            s = d2s(day, CalendarData.ClassicMonths[month - 1], year, "");
            Assert.AreEqual("15 November 2016, ", s); // ok+

            CalendarConverter.jd_to_hebrew3((int)jd, out year, out month, out day);
            s = d2s(day, CalendarData.HebrewMonths[month - 1], year, "");
            Assert.AreEqual("27 Heshvan 5777, ", s); // ok+

            CalendarConverter.jd_to_islamic3((int)jd, out year, out month, out day);
            s = d2s(day, CalendarData.IslamicMonths[month - 1], year, "");
            Assert.AreEqual("27 Safar 1438, ", s); // ok+
        }

        [Test]
        public void Test_GK()
        {
            double jd;
            int year, month, day;

            const double needJD = 2448174.5; // 1990-10-10 [g], 1990-09-27 [j], 5751-07-21 [h]

            //for (int i = 0; i < 1000000; i++) {
            jd = CalendarConverter.gregorian_to_jd2(-4713, 11, 24);
            Assert.AreEqual(0.0, jd);

            jd = CalendarConverter.gregorian_to_jd(1990, 10, 10);
            Assert.AreEqual(needJD, jd);

            jd = CalendarConverter.julian_to_jd(1990, 09, 27);
            Assert.AreEqual(needJD, jd);

            jd = CalendarConverter.hebrew_to_jd(5751, 07, 21);
            Assert.AreEqual(needJD, jd);
            //}

            jd = CalendarConverter.gregorian_to_jd2(1990, 10, 10);
            CalendarConverter.jd_to_gregorian2((int)jd, out year, out month, out day);
            Assert.AreEqual(1990, year, "g2jd 1");
            Assert.AreEqual(10, month, "g2jd 2");
            Assert.AreEqual(10, day, "g2jd 3");
        }
    }
}
