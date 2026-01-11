// Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
// This file is part of GEDKeeper, licensed under the GNU GPL v3.
// See LICENSE file in the project root for full license information.

using System;
using BSLib;
using GKCore.Utilities;
using GKTests;
using NUnit.Framework;

namespace GKCore.ExtData
{
    [TestFixture]
    public class HolidaysTests
    {
        private Holidays fHolidays;

        public HolidaysTests()
        {
            fHolidays = new Holidays();
        }

        [Test]
        public void Test_CollectTips()
        {
            Assume.That(SysUtils.IsUnix(), Is.False);
            Assert.Throws(typeof(ArgumentNullException), () => { fHolidays.CollectTips(null); });

            using (StringList tipsList = new StringList()) {
                fHolidays.CollectTips(tipsList);
                Assert.AreEqual(0, tipsList.Count);

                string holidaysFile = TestUtils.PrepareTestFile("test_holidays.yaml");
                try {
                    fHolidays.Load(holidaysFile);
                } finally {
                    TestUtils.RemoveTestFile(holidaysFile);
                }

                fHolidays.CollectTips(tipsList, new DateTime(DateTime.Now.Year, 01, 05));
                Assert.AreEqual(2, tipsList.Count); // one for header, and one with day
            }
        }
    }
}
