/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
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
using System.Globalization;
using GDModel.Providers.GEDCOM;
using GKCore.Calendar;
using GKCore.Types;
using GKTests;
using NUnit.Framework;

namespace GDModel
{
    // TODO KBR leap year
    // TODO KBR how does get/set datetime handle values outside the range of the Date object?
    // TODO KBR setJulian(12,20,1980) throws exception
    // TODO KBR greg <> julian conversion
    // TODO KBR use UDN to check invalid date
    [TestFixture]
    public class GDMDateTests
    {
        public GDMDateTests()
        {
            TestUtils.InitUITest();
        }

        [Test]
        public void Test_Common()
        {
            using (GDMDate dtx1 = new GDMDate()) {
                Assert.IsNotNull(dtx1, "dtx1 != null");
                dtx1.ParseString("20 JAN 2013");

                DateTime dt = TestUtils.ParseDT("20.01.2013");
                Assert.IsTrue(dtx1.Date.Equals(dt), "dtx1.DateTime.Equals(dt)");
                Assert.AreEqual(2013, dtx1.GetChronologicalYear());

                //dtx1.DateCalendar = GEDCOMCalendar.dcFrench;
                Assert.AreEqual(GDMCalendar.dcGregorian, dtx1.DateCalendar);

                dtx1.Day = 21;
                Assert.AreEqual(21, dtx1.Day);

                dtx1.Month = 09;
                Assert.AreEqual(09, dtx1.Month);

                dtx1.Year = 1812;
                Assert.AreEqual(1812, dtx1.Year);

                dtx1.YearBC = true;
                Assert.AreEqual(true, dtx1.YearBC);

                dtx1.YearModifier = "2";
                Assert.AreEqual("2", dtx1.YearModifier);

                //
                dtx1.ParseString("01 FEB 1934/11B.C.");
                Assert.AreEqual(01, dtx1.Day);
                Assert.AreEqual(02, dtx1.Month);
                Assert.AreEqual(1934, dtx1.Year);
                Assert.AreEqual("11", dtx1.YearModifier);
                Assert.AreEqual(true, dtx1.YearBC);
                dtx1.ParseString("01 FEB 1934/11B.C.");
                Assert.AreEqual("01 FEB 1934/11B.C.", dtx1.StringValue);

                // gregorian

                dtx1.SetGregorian(1, 1, 1980);
                Assert.AreEqual(GDMCalendar.dcGregorian, dtx1.DateCalendar);
                Assert.AreEqual("01 JAN 1980", dtx1.StringValue);

                Assert.Throws(typeof(GDMDateException), () => {
                    dtx1.SetGregorian(1, "X", 1980, "", false);
                });

                // julian

                dtx1.SetJulian(1, "JAN", 1980, false);
                Assert.AreEqual(GDMCalendar.dcJulian, dtx1.DateCalendar);

                dtx1.SetJulian(1, 3, 1980);
                Assert.AreEqual(GDMCalendar.dcJulian, dtx1.DateCalendar);
                Assert.AreEqual("@#DJULIAN@ 01 MAR 1980", dtx1.StringValue);
                dtx1.ParseString("@#DJULIAN@ 01 MAR 1980");
                Assert.AreEqual("@#DJULIAN@ 01 MAR 1980", dtx1.StringValue);

                using (GDMDate dtx2 = new GDMDate()) {
                    Assert.IsNotNull(dtx2, "dtx2 != null");

                    Assert.Throws(typeof(ArgumentException), () => {
                        dtx2.Assign(null);
                    });

                    Assert.AreEqual("", dtx2.StringValue);
                    Assert.AreEqual(new DateTime(0), dtx2.GetDateTime());

                    Assert.IsFalse(dtx2.IsValidDate());

                    dtx2.Assign(dtx1);
                    Assert.AreEqual("@#DJULIAN@ 01 MAR 1980", dtx2.StringValue);

                    Assert.IsTrue(dtx2.IsValidDate());
                }

                // hebrew

                dtx1.SetHebrew(1, "TSH", 1980, false);
                Assert.AreEqual(GDMCalendar.dcHebrew, dtx1.DateCalendar);

                dtx1.SetHebrew(1, 2, 1980);
                Assert.AreEqual(GDMCalendar.dcHebrew, dtx1.DateCalendar);
                Assert.AreEqual("@#DHEBREW@ 01 CSH 1980", dtx1.StringValue);
                dtx1.ParseString("@#DHEBREW@ 01 CSH 1980");
                Assert.AreEqual("@#DHEBREW@ 01 CSH 1980", dtx1.StringValue);

                Assert.Throws(typeof(GDMDateException), () => {
                    dtx1.SetHebrew(1, "X", 1980, false);
                });

                // french

                dtx1.SetFrench(1, "VEND", 1980, false);
                Assert.AreEqual(GDMCalendar.dcFrench, dtx1.DateCalendar);

                dtx1.SetFrench(1, 2, 1980);
                Assert.AreEqual(GDMCalendar.dcFrench, dtx1.DateCalendar);
                Assert.AreEqual("@#DFRENCH R@ 01 BRUM 1980", dtx1.StringValue);
                dtx1.ParseString("@#DFRENCH R@ 01 BRUM 1980");
                Assert.AreEqual("@#DFRENCH R@ 01 BRUM 1980", dtx1.StringValue);

                Assert.Throws(typeof(GDMDateException), () => {
                    dtx1.SetFrench(1, "X", 1980, false);
                });

                // roman

                dtx1.SetRoman(1, "JAN", 1980, false);
                Assert.AreEqual(GDMCalendar.dcRoman, dtx1.DateCalendar);

                dtx1.SetUnknown(1, "JAN", 1980, false);
                Assert.AreEqual(GDMCalendar.dcUnknown, dtx1.DateCalendar);
            }
        }

        [Test]
        public void Test_Misc()
        {
            GDMDate dtx1 = new GDMDate();
            dtx1.ParseString("20 JAN 2013");

            GDMDate dtx2 = new GDMDate();
            dtx2.ParseString("20 JAN 2014");

            var dtA = GDMCustomDate.CreateApproximated(dtx1, GDMApproximated.daCalculated);
            Assert.IsNotNull(dtA);
            Assert.AreEqual(GDMApproximated.daCalculated, dtA.Approximated);
            dtA.GetHashCode();

            var dtP = GDMCustomDate.CreatePeriod(dtx1, dtx2);
            Assert.IsNotNull(dtP);

            var dtR = GDMCustomDate.CreateRange(dtx1, dtx2);
            Assert.IsNotNull(dtR);
        }

        [Test]
        public void Test_GetApproximated()
        {
            GDMDate instance = new GDMDate();
            Assert.AreEqual(GDMApproximated.daExact, instance.Approximated);
        }

        [Test]
        public void Test_SetApproximated()
        {
            GDMDate instance = new GDMDate();
            instance.Approximated = GDMApproximated.daAbout;
            Assert.AreEqual(GDMApproximated.daAbout, instance.Approximated);
        }

        [Test]
        public void Test_GetDateCalendar()
        {
            GDMDate instance = GDMDate.CreateByFormattedStr("20/12/1980", false);
            Assert.AreEqual(GDMCalendar.dcGregorian, instance.DateCalendar);

            instance = GDMDate.CreateByFormattedStr("20/12/1980", GDMCalendar.dcJulian, false);
            Assert.AreEqual(GDMCalendar.dcJulian, instance.DateCalendar);
        }

        [Test]
        public void Test_GetDay()
        {
            GDMDate instance = GDMDate.CreateByFormattedStr("20/12/1980", false);
            Assert.AreEqual(20, instance.Day);
        }

        [Test]
        public void Test_IsValidDate()
        {
            GDMDate instance = GDMDate.CreateByFormattedStr("31/11/1980", true);
            Assert.AreEqual(true, instance.IsValidDate());

            instance = new GDMDate();
            Assert.AreEqual(false, instance.IsValidDate());

            // month is unknown
            instance = GDMDate.CreateByFormattedStr("31/  /1980", true);
            Assert.AreEqual(false, instance.IsValidDate());
        }

        [Test]
        public void Test_SetDay()
        {
            GDMDate instance = new GDMDate();
            instance.Day = 20;
            Assert.AreEqual(20, instance.Day);
        }

        [Test]
        public void Test_GetMonth()
        {
            GDMDate instance = GDMDate.CreateByFormattedStr("20/12/1980", false);
            Assert.AreEqual(12, instance.Month);
        }

        [Test]
        public void Test_SetMonth()
        {
            GDMDate instance = GDMDate.CreateByFormattedStr("20/01/1980", false);
            instance.Month = 12;
            Assert.AreEqual(12, instance.Month);
        }

        [Test]
        public void Test_GetYear()
        {
            GDMDate instance = GDMDate.CreateByFormattedStr("20/01/1980", false);
            Assert.AreEqual(1980, instance.Year);
        }

        [Test]
        public void Test_SetYear()
        {
            GDMDate instance = GDMDate.CreateByFormattedStr("20/01/1980", false);
            instance.Year = 2001;
            Assert.AreEqual(2001, instance.Year);
        }

        [Test]
        public void Test_GetYearBC()
        {
            GDMDate instance = new GDMDate();
            Assert.AreEqual(false, instance.YearBC);
        }

        [Test]
        public void Test_SetYearBC()
        {
            GDMDate instance = new GDMDate();
            instance.YearBC = true;
            Assert.AreEqual(true, instance.YearBC);
        }

        [Test]
        public void Test_GetYearModifier()
        {
            GDMDate instance = new GDMDate();
            Assert.AreEqual("", instance.YearModifier);

            instance.ParseString("20 DEC 1980/1");
            Assert.AreEqual("1", instance.YearModifier);
        }

        [Test]
        public void Test_SetYearModifier()
        {
            GDMDate instance = GDMDate.CreateByFormattedStr("20/01/1980", false);
            instance.YearModifier = "2";
            Assert.AreEqual("20 JAN 1980/2", instance.StringValue);
        }

        [Test]
        public void Test_Create()
        {
            GDMTag result = new GDMDate(GEDCOMTagsTable.Lookup("BLAH"));
            Assert.IsNotNull(result);
            Assert.AreEqual("BLAH", result.GetTagName());
        }

        [Test]
        public void Test_Clear()
        {
            GDMDate instance = GDMDate.CreateByFormattedStr("20/01/1980", false);
            instance.Clear();
            Assert.AreEqual("", instance.StringValue);
        }

        [Test]
        public void Test_Equals()
        {
            GDMDate instance = GDMDate.CreateByFormattedStr("20/01/1980", false);
            GDMDate instance2 = GDMDate.CreateByFormattedStr("21/01/1980", false);

            Assert.IsFalse(instance.Equals(instance2));
            Assert.IsFalse(instance.Equals((object)instance2)); // other method

            instance2.Assign(instance);

            Assert.IsTrue(instance.Equals(instance2));
            Assert.IsTrue(instance.Equals((object)instance2)); // other method

            Assert.IsFalse(instance.Equals((object)null));
        }

        [Test]
        public void Test_IsEmpty()
        {
            GDMDate instance = new GDMDate();
            Assert.AreEqual(true, instance.IsEmpty());
        }

        [Test]
        public void Test_Assign()
        {
            GDMDate instance = new GDMDate();

            GDMTag source = null;
            Assert.Throws(typeof(ArgumentException), () => { instance.Assign(source); });
        }

        private static DateTime ParseDT(string dtx)
        {
            return DateTime.ParseExact(dtx, "yyyy-MM-dd", CultureInfo.InvariantCulture);
        }

        [Test]
        public void Test_GetDateTime()
        {
            GDMDate instance = GDMDate.CreateByFormattedStr("20/01/1980", false);
            DateTime expResult;
            try {
                expResult = ParseDT("1980-01-20");
                DateTime result = instance.GetDateTime();
                Assert.AreEqual(expResult, result);
            } catch (Exception) {
                Assert.Fail("Parse exception for date");
            }
        }

        [Test]
        public void Test_SetDateTime()
        {
            GDMDate instance = new GDMDate();
            DateTime expResult;
            try {
                expResult = ParseDT("1980-01-20");
                instance.SetDateTime(expResult);
                Assert.AreEqual(expResult, instance.GetDateTime());
            } catch (Exception) {
                Assert.Fail("Parse exception for date");
            }
        }

        [Test]
        public void Test_ParseString()
        {
            GDMDate instance = new GDMDate();
            Assert.AreEqual("", instance.ParseString("20 DEC 1980"));
            Assert.AreEqual("20 DEC 1980", instance.StringValue);
        }

        [Test]
        public void Test_ParseString_system()
        {
            GDMDate instance = new GDMDate();
            Assert.AreEqual("", instance.ParseString("20.12.1980"));
            Assert.AreEqual("20 DEC 1980", instance.StringValue);
        }

        [Test]
        public void Test_ParseStringIslamic()
        {
            // TODO: SetDateIslamic isn't finished!
            /*string strValue = "@#DISLAMIC@ 20 RAJAB 1980";
            GEDCOMDate instance = new GEDCOMDate(null, null, "", "");
            instance.SetDate(GEDCOMCalendar.dcIslamic, 0, 0, 0);
            string expResult = "";
            string result = instance.ParseString(strValue);
            Assert.AreEqual(expResult, result);
            Assert.AreEqual("@#DISLAMIC@ 20 RAJAB 1980", instance.StringValue);*/
        }

        [Test]
        public void Test_ParseStringUnknown()
        {
            GDMDate instance = new GDMDate();
            instance.SetDate(GDMCalendar.dcIslamic, 0, 0, 0);
            Assert.AreEqual(string.Empty, instance.ParseString("@#DUNKNOWN@ 20 DEC 1980"));
            Assert.AreEqual("@#DUNKNOWN@ 20 DEC 1980", instance.StringValue);
        }

        [Test]
        public void Test_GetStringValue()
        {
            GDMDate instance = new GDMDate();
            instance.SetJulian(1, 3, 1980);
            Assert.AreEqual("@#DJULIAN@ 01 MAR 1980", instance.StringValue);
        }

        [Test]
        public void Test_SetDate()
        {
            GDMDate instance = new GDMDate();
            instance.SetDate(GDMCalendar.dcGregorian, 20, 12, 1980);
            Assert.AreEqual("1980.12.20", instance.GetDisplayString(DateFormat.dfYYYY_MM_DD, false, false));
            Assert.AreEqual("20 DEC 1980", instance.StringValue);
        }

        [Test]
        public void Test_SetDateJulian()
        {
            GDMDate instance = new GDMDate();
            instance.SetDate(GDMCalendar.dcJulian, 20, 12, 1980);
            Assert.AreEqual("1980.12.20", instance.GetDisplayString(DateFormat.dfYYYY_MM_DD, false, false));
            Assert.AreEqual("@#DJULIAN@ 20 DEC 1980", instance.StringValue);
        }

        [Test]
        public void Test_SetDateFrench()
        {
            GDMDate instance = new GDMDate();
            instance.SetDate(GDMCalendar.dcFrench, 20, 12, 1980);
            Assert.AreEqual("1980.12.20", instance.GetDisplayString(DateFormat.dfYYYY_MM_DD, false, false));
            Assert.AreEqual("@#DFRENCH R@ 20 FRUC 1980", instance.StringValue);
        }

        [Test]
        public void Test_SetDateHebrew()
        {
            GDMDate instance = new GDMDate();
            instance.SetDate(GDMCalendar.dcHebrew, 20, 12, 1980);
            Assert.AreEqual("1980.12.20", instance.GetDisplayString(DateFormat.dfYYYY_MM_DD, false, false));
            Assert.AreEqual("@#DHEBREW@ 20 AAV 1980", instance.StringValue);
        }

        [Test]
        public void Test_SetDateRoman()
        {
            // TODO: SetDateRoman isn't finished!
            /*GEDCOMCalendar calendar = GEDCOMCalendar.dcRoman;
            int day = 20;
            int month = 12;
            int year = 1980;
            GEDCOMDate instance = new GEDCOMDate(null, null, "", "");
            instance.SetDate(calendar, day, month, year);
            string result = instance.GetDisplayString(DateFormat.dfYYYY_MM_DD, false, false);
            Assert.AreEqual("1980.12.20", result);*/
        }

        [Test]
        public void Test_SetGregorian_3args()
        {
            GDMDate instance = new GDMDate();
            instance.SetGregorian(20, 12, 1980);
            Assert.AreEqual("1980.12.20", instance.GetDisplayString(DateFormat.dfYYYY_MM_DD, false, false));
        }

        [Test]
        public void Test_SetGregorian_5args()
        {
            GDMDate instance = new GDMDate();
            instance.SetGregorian(20, "Dec", 1980, "", false);
            Assert.AreEqual("1980.12.20", instance.GetDisplayString(DateFormat.dfYYYY_MM_DD, false, false));
        }

        [Test]
        public void Test_SetGregorian_5argsBC()
        {
            GDMDate instance = new GDMDate();
            instance.SetGregorian(20, "Dec", 1980, "", true);
            Assert.AreEqual("BC 1980.12.20", instance.GetDisplayString(DateFormat.dfYYYY_MM_DD, true, false));
        }

        [Test]
        public void Test_SetJulian_3args()
        {
            GDMDate instance = new GDMDate();
            instance.SetJulian(20, 12, 1980);
            Assert.AreEqual("1980.12.20", instance.GetDisplayString(DateFormat.dfYYYY_MM_DD, false, false));
        }

        [Test]
        public void Test_SetJulian_4args()
        {
            GDMDate instance = new GDMDate();
            instance.SetJulian(20, "DEC", 1980, false);
            Assert.AreEqual("1980.12.20", instance.GetDisplayString(DateFormat.dfYYYY_MM_DD, false, false));
        }

        [Test]
        public void Test_SetHebrew_3args()
        {
            GDMDate instance = new GDMDate();
            instance.SetHebrew(1, 2, 1980);
            Assert.AreEqual(GDMCalendar.dcHebrew, instance.DateCalendar);
            Assert.AreEqual("@#DHEBREW@ 01 CSH 1980", instance.StringValue);
        }

        [Test]
        public void Test_SetHebrew_4args()
        {
            GDMDate instance = new GDMDate();
            instance.SetHebrew(1, "TSH", 1980, false);
            Assert.AreEqual(GDMCalendar.dcHebrew, instance.DateCalendar);
            Assert.AreEqual("@#DHEBREW@ 01 TSH 1980", instance.StringValue);

            // Code coverage
            instance.SetHebrew(1, "", 1980, false);
            Assert.AreEqual(GDMCalendar.dcHebrew, instance.DateCalendar);
        }

        [Test]
        public void Test_SetFrench_3args()
        {
            GDMDate instance = new GDMDate();
            instance.SetFrench(1, 2, 1980);
            Assert.AreEqual(GDMCalendar.dcFrench, instance.DateCalendar);
            Assert.AreEqual("@#DFRENCH R@ 01 BRUM 1980", instance.StringValue);
        }

        [Test]
        public void Test_SetFrench_4args()
        {
            GDMDate instance = new GDMDate();
            instance.SetFrench(1, "VEND", 1980, false);
            Assert.AreEqual(GDMCalendar.dcFrench, instance.DateCalendar);
            Assert.AreEqual("@#DFRENCH R@ 01 VEND 1980", instance.StringValue);

            // Code coverage
            instance.SetHebrew(1, "", 1980, false);
            Assert.AreEqual(GDMCalendar.dcHebrew, instance.DateCalendar);
        }

        [Test]
        public void Test_SetRoman()
        {
            GDMDate instance = new GDMDate();
            instance.SetRoman(1, "JAN", 1980, false);
            Assert.AreEqual(GDMCalendar.dcRoman, instance.DateCalendar);
        }

        [Test]
        public void Test_SetUnknown()
        {
            GDMDate instance = new GDMDate();
            instance.SetUnknown(0, "", 0, false);
            Assert.AreEqual(GDMCalendar.dcUnknown, instance.DateCalendar);
        }

        [Test]
        public void Test_DateChanged()
        {
            GDMDate instance = GDMDate.CreateByFormattedStr("31/11/1980", true);
            //instance.dateChanged();
            string result = instance.GetUDN().ToString();
            Assert.AreEqual("1980/12/01", result);
        }

        [Test]
        public void Test_GetUDN()
        {
            GDMDate instance = GDMDate.CreateByFormattedStr("20/12/1980", true);
            UDN result = instance.GetUDN();

            UDN expResult = new UDN(UDNCalendarType.ctGregorian, 1980, 12, 20);

            Assert.IsTrue(expResult.Equals(result));
            Assert.AreEqual(expResult, result);
        }

        [Test]
        public void Test_CreateByFormattedStr_String_boolean()
        {
            Assert.AreEqual(null, GDMDate.CreateByFormattedStr(null, false));
            Assert.AreEqual("20 DEC 1980", GDMDate.CreateByFormattedStr("20/12/1980", false).StringValue);
            Assert.AreEqual("DEC 1980", GDMDate.CreateByFormattedStr("__/12/1980", false).StringValue);
            Assert.AreEqual(null, GDMDate.CreateByFormattedStr("1980", false));
            //Assert.Throws(typeof(GEDCOMDateException), () => { GEDCOMDate.createByFormattedStr("1980", true); });
        }

        [Test]
        public void Test_CreateByFormattedStr_3args()
        {
            Assert.AreEqual(null, GDMDate.CreateByFormattedStr(null, GDMCalendar.dcGregorian, false));
            Assert.AreEqual("20 DEC 1980", GDMDate.CreateByFormattedStr("20/12/1980", GDMCalendar.dcGregorian, false).StringValue);
            Assert.AreEqual("DEC 1980", GDMDate.CreateByFormattedStr("__/12/1980", GDMCalendar.dcGregorian, false).StringValue);
            Assert.AreEqual(null, GDMDate.CreateByFormattedStr("1980", GDMCalendar.dcGregorian, false));
            //Assert.Throws(typeof(GEDCOMDateException), () => { GEDCOMDate.createByFormattedStr("1980", GEDCOMCalendar.dcGregorian, true); });
        }

        [Test]
        public void Test_GetUDNByFormattedStr()
        {
            UDN expResult = UDN.Unknown;
            UDN result = GDMDate.GetUDNByFormattedStr("", GDMCalendar.dcGregorian);
            Assert.AreEqual(expResult, result);
        }

        [Test]
        public void Test_GetDisplayString_DateFormat_boolean()
        {
            GDMDate instance = GDMDate.CreateByFormattedStr("20/12/1980", true);
            Assert.AreEqual("20.12.1980", instance.GetDisplayString(DateFormat.dfDD_MM_YYYY, false));
        }

        [Test]
        public void Test_GetDisplayString_DateFormat()
        {
            GDMDate instance = GDMDate.CreateByFormattedStr("20/12/1980", true);
            string result = instance.GetDisplayString(DateFormat.dfYYYY_MM_DD);
            Assert.AreEqual("1980.12.20", result);

            result = instance.GetDisplayString(DateFormat.dfDD_MM_YYYY);
            Assert.AreEqual("20.12.1980", result);

            result = instance.GetDisplayString(DateFormat.dfYYYY);
            Assert.AreEqual("1980", result);
        }

        [Test]
        public void Test_GetDisplayString_3args()
        {
            GDMDate instance = new GDMDate();
            instance.SetJulian(20, 12, 1980);
            instance.YearBC = true;
            string result = instance.GetDisplayString(DateFormat.dfYYYY_MM_DD, true, true);
            Assert.AreEqual("BC 1980.12.20 [J]", result);

            result = instance.GetDisplayString(DateFormat.dfDD_MM_YYYY, true, false);
            Assert.AreEqual("20.12.1980 BC", result);

            result = instance.GetDisplayString(DateFormat.dfYYYY, true, false);
            Assert.AreEqual("BC 1980", result);
        }

        [Test]
        public void Test_GetDisplayStringExt()
        {
            GDMDate instance = new GDMDate();
            instance.SetJulian(20, 12, 1980);
            instance.YearBC = true;
            string result = instance.GetDisplayStringExt(DateFormat.dfYYYY_MM_DD, true, true);
            Assert.AreEqual("BC 1980.12.20 [J]", result);

            instance.Approximated = GDMApproximated.daEstimated;
            result = instance.GetDisplayStringExt(DateFormat.dfYYYY_MM_DD, true, true);
            Assert.AreEqual("~ BC 1980.12.20 [J]", result);
        }

        [Test]
        public void Test_CreateByFormattedStr_exception()
        {
            Assert.Throws(typeof(GDMDateException), () => {
                              GDMDate.CreateByFormattedStr("1.2", true);
                          });
        }

        [Test]
        public void Test_getUDNByFormattedStr2()
        {
            //Assert.Throws(typeof(GEDCOMDateException), () => {
            var dtx = GDMDate.GetUDNByFormattedStr("20-12-1980", GDMCalendar.dcGregorian, true);
            Assert.AreEqual("1980/12/20", dtx.ToString());
            //});
        }

        [Test]
        public void Test_AhnenblattDate()
        {
            string gedcom = "0 HEAD\n1 SOUR AHN\n0 @I1@ INDI\n1 BIRT\n2 DATE (20/12-1980)";
            GDMIndividualRecord rec2 = TestUtils.ParseIndiRec(gedcom);

            Assert.AreEqual(1, rec2.Events.Count);
            GDMCustomEvent birt = rec2.Events.Extract(0);
            Assert.AreEqual("20 DEC 1980", birt.Date.StringValue);
        }

        [Test]
        public void Test_DateInterpreted()
        {
            DateTime expectDate = TestUtils.ParseDT("20.01.2013");

            using (var dtx1 = new GDMDateInterpreted()) {
                Assert.IsNotNull(dtx1, "dtx1 != null");

                dtx1.ParseString("INT 20 JAN 2013 (today)");
                Assert.AreEqual(expectDate, dtx1.Date);
                Assert.AreEqual("today", dtx1.DatePhrase);

                dtx1.DatePhrase = "now";
                Assert.AreEqual("INT 20 JAN 2013 (now)", dtx1.StringValue);

                dtx1.DatePhrase = "(yesterday)";
                Assert.AreEqual("INT 20 JAN 2013 (yesterday)", dtx1.StringValue);

                dtx1.ParseString("INT 20 JAN 2013 (yesterday)");
                Assert.AreEqual("INT 20 JAN 2013 (yesterday)", dtx1.StringValue);

                Assert.Throws(typeof(GEDCOMIntDateException), () => { dtx1.ParseString("10 JAN 2013 (today)"); });

                Assert.AreEqual(string.Empty, dtx1.ParseString(null));
            }
        }

        [Test]
        public void Test_CreateByFormattedStr()
        {
            Assert.AreEqual(null, GDMDate.CreateByFormattedStr(null, false));
            Assert.AreEqual("20 DEC 1980", GDMDate.CreateByFormattedStr("20/12/1980", false).StringValue);
            Assert.AreEqual("DEC 1980", GDMDate.CreateByFormattedStr("__/12/1980", false).StringValue);
            Assert.AreEqual(null, GDMDate.CreateByFormattedStr("1980", false));

            Assert.Throws(typeof(GDMDateException), () => { GDMDate.CreateByFormattedStr("1980", true); });
        }

        [Test]
        [TestCase("", "")]
        [TestCase("2024", "2025")]
        [TestCase("001B.C.", "001")]
        [TestCase("OCT 2024", "NOV 2024")]
        [TestCase("DEC 2024", "JAN 2025")]
        [TestCase("01 OCT 2024", "02 OCT 2024")]
        [TestCase("31 OCT 2024", "01 NOV 2024")]
        [TestCase("31 DEC 2024", "01 JAN 2025")]
        [TestCase("28 FEB 1900", "01 MAR 1900")]
        [TestCase("28 FEB 2000", "29 FEB 2000")]
        [TestCase("29 FEB 2000", "01 MAR 2000")]
        [TestCase("28 FEB 2023", "01 MAR 2023")]
        [TestCase("28 FEB 2024", "29 FEB 2024")]
        [TestCase("29 FEB 2024", "01 MAR 2024")]
        [TestCase("28 FEB 001B.C.", "29 FEB 001B.C.")]
        [TestCase("29 FEB 001B.C.", "01 MAR 001B.C.")]
        [TestCase("28 FEB 005B.C.", "29 FEB 005B.C.")]
        [TestCase("29 FEB 005B.C.", "01 MAR 005B.C.")]
        [TestCase("28 FEB 097B.C.", "29 FEB 097B.C.")]
        [TestCase("29 FEB 097B.C.", "01 MAR 097B.C.")]
        [TestCase("28 FEB 101B.C.", "01 MAR 101B.C.")]
        [TestCase("@#DJULIAN@ 29 FEB 2024", "@#DJULIAN@ 01 MAR 2024")]
        [TestCase("@#DJULIAN@ 28 FEB 1900", "@#DJULIAN@ 29 FEB 1900")]
        [TestCase("@#DJULIAN@ 29 FEB 1900", "@#DJULIAN@ 01 MAR 1900")]
        [TestCase("@#DJULIAN@ 28 FEB 2000", "@#DJULIAN@ 29 FEB 2000")]
        [TestCase("@#DJULIAN@ 29 FEB 2000", "@#DJULIAN@ 01 MAR 2000")]
        [TestCase("@#DJULIAN@ 28 FEB 2023", "@#DJULIAN@ 01 MAR 2023")]
        [TestCase("@#DJULIAN@ 29 FEB 2024", "@#DJULIAN@ 01 MAR 2024")]
        public void Test_Increment_Decrement(string value, string expected)
        {
            var d = new GDMDate();
            d.StringValue = value;
            Assert.AreEqual(expected, GDMDate.Increment(d).StringValue);
            d.StringValue = expected;
            Assert.AreEqual(value, GDMDate.Decrement(d).StringValue);
        }
    }
}
