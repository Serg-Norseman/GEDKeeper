/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
 *
 *  This file is part of "GEDKeeper".
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

using System;
using GKCommon;
using NUnit.Framework;

namespace GKTests.GKCommon
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


            var dtx = CalendarConverter.jd_to_julian(jd);
            s = d2s(dtx.Day, CalendarData.ClassicMonths[dtx.Month - 1], dtx.Year, "");
            Assert.AreEqual("27 September 1990, ", s); // +

            dtx = CalendarConverter.jd_to_hebrew(jd);
            s = d2s(dtx.Day, CalendarData.HebrewMonths[dtx.Month - 1], dtx.Year, CalendarData.HebrewWeekdays[CalendarConverter.jwday(jd)]);
            Assert.AreEqual("21 Tishri 5751, Dalet", s); // +

            dtx = CalendarConverter.jd_to_islamic(jd);
            s = d2s(dtx.Day, CalendarData.IslamicMonths[dtx.Month - 1], dtx.Year, CalendarData.IslamicWeekdays[CalendarConverter.jwday(jd)]);
            Assert.AreEqual("20 Rabi`al-Awwal 1411, Al-'arb`a'", s); // +

            dtx = CalendarConverter.jd_to_persian(jd);
            s = d2s(dtx.Day, CalendarData.PersianMonths[dtx.Month - 1], dtx.Year, CalendarData.PersianWeekdays[CalendarConverter.jwday(jd)]);
            Assert.AreEqual("18 Mehr 1369, Chaharshanbeh", s); // +

            dtx = CalendarConverter.jd_to_indian_civil(jd);
            s = d2s(dtx.Day, CalendarData.IndianCivilMonths[dtx.Month - 1], dtx.Year, CalendarData.IndianCivilWeekdays[CalendarConverter.jwday(jd)]);
            Assert.AreEqual("18 Asvina 1912, Budhavara", s); // +

            dtx = CalendarConverter.jd_to_bahai(jd);
            s = "major " + dtx.BahaiMajor.ToString() + ", cycle " + dtx.BahaiCycle.ToString() + ", ";
            s = s + d2s(dtx.Day, CalendarData.BahaiMonths[dtx.Month - 1], dtx.Year, CalendarData.BahaiWeekdays[CalendarConverter.jwday(jd)]);
            Assert.AreEqual("major 1, cycle 8, 14 Mashíyyat 14, ‘Idál", s); // ???
        }

        [Test]
        public void Test_CP()
        {
            DateTime gdt = new DateTime(2016, 11, 28);

            string s;
            double jd;

            jd = CalendarConverter.gregorian_to_jd2(gdt.Year, gdt.Month, gdt.Day);
            Assert.AreEqual(2457721, jd); // ok+

            var dtx = CalendarConverter.jd_to_julian2((int)jd);
            s = d2s(dtx.Day, CalendarData.ClassicMonths[dtx.Month - 1], dtx.Year, "");
            Assert.AreEqual("15 November 2016, ", s); // ok+

            dtx = CalendarConverter.jd_to_hebrew3((int)jd);
            s = d2s(dtx.Day, CalendarData.HebrewMonths[dtx.Month - 1], dtx.Year, "");
            Assert.AreEqual("27 Heshvan 5777, ", s); // ok+

            dtx = CalendarConverter.jd_to_islamic3((int)jd);
            s = d2s(dtx.Day, CalendarData.IslamicMonths[dtx.Month - 1], dtx.Year, "");
            Assert.AreEqual("27 Safar 1438, ", s); // ok+
        }

        [Test]
        public void Test_GK()
        {
            double jd;

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
            var dtx = CalendarConverter.jd_to_gregorian2((int) jd);
            Assert.AreEqual(1990, dtx.Year, "g2jd 1");
            Assert.AreEqual(10, dtx.Month, "g2jd 2");
            Assert.AreEqual(10, dtx.Day, "g2jd 3");
        }
    }
}
