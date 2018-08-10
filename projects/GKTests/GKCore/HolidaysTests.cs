/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2017 by Sergey V. Zhdanovskih.
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
using BSLib;
using GKTests;
using NUnit.Framework;

namespace GKCore
{
    [TestFixture]
    public class HolidaysTests
    {
        private Holidays fHolidays;

        [TestFixtureSetUp]
        public void SetUp()
        {
            fHolidays = new Holidays();
        }

        [Test]
        public void Test_CollectTips()
        {
            Assert.Throws(typeof(ArgumentNullException), () => { fHolidays.CollectTips(null); });

            using (StringList tipsList = new StringList()) {
                fHolidays.CollectTips(tipsList);
                Assert.AreEqual(0, tipsList.Count);

                string holidaysFile = TestUtils.PrepareTestFile("test_holidays.yaml");
                fHolidays.Load(holidaysFile);

                fHolidays.CollectTips(tipsList, new DateTime(DateTime.Now.Year, 01, 05));
                Assert.AreEqual(2, tipsList.Count); // one for header, and one with day
            }
        }
    }
}
