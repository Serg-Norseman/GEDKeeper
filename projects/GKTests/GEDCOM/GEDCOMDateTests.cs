/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2019 by Sergey V. Zhdanovskih.
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
using BSLib.Calendar;
using GKCommon.GEDCOM;
using GKCore.Types;
using GKTests;
using NUnit.Framework;

namespace GKCommon.GEDCOM
{
    // TODO KBR date formats 20-DEC-1980,12/20/1980(american),others? createbyformattedstr() doesn't accept
    // TODO KBR leap year
    // TODO KBR how does get/set datetime handle values outside the range of the Date object?
    // TODO KBR setJulian(12,20,1980) throws exception
    // TODO KBR greg <> julian conversion
    // TODO KBR use UDN to check invalid date

    /**
     *
     * @author Sergey V. Zhdanovskih
     * Modified by Kevin Routley (KBR) aka fire-eggs
     */
    [TestFixture]
    public class GEDCOMDateTests
    {
        [Test]
        public void Test_GetApproximated()
        {
            GEDCOMDate instance = new GEDCOMDate(null, null, "", "");
            GEDCOMApproximated expResult = GEDCOMApproximated.daExact;
            GEDCOMApproximated result = instance.Approximated;
            Assert.AreEqual(expResult, result);
        }

        [Test]
        public void Test_SetApproximated()
        {
            GEDCOMApproximated value = GEDCOMApproximated.daAbout;
            GEDCOMDate instance = new GEDCOMDate(null, null, "", "");
            instance.Approximated = value;
            Assert.AreEqual(value, instance.Approximated);
        }

        [Test]
        public void Test_GetDateCalendar()
        {
            GEDCOMDate instance = GEDCOMDate.CreateByFormattedStr("20/12/1980", false);
            GEDCOMCalendar result = instance.DateCalendar;
            Assert.AreEqual(GEDCOMCalendar.dcGregorian, result);
            instance = GEDCOMDate.CreateByFormattedStr("20/12/1980", GEDCOMCalendar.dcJulian, false);
            result = instance.DateCalendar;
            Assert.AreEqual(GEDCOMCalendar.dcJulian, result);
        }

        [Test]
        public void Test_GetDay()
        {
            GEDCOMDate instance = GEDCOMDate.CreateByFormattedStr("20/12/1980", false);
            byte result = instance.Day;
            Assert.AreEqual(20, result);
        }

        [Test]
        public void Test_GetDayInvalid()
        {
            /*GEDCOMDate instance = GEDCOMDate.CreateByFormattedStr("31/11/1980", true);
            int result = instance.Day;
            Assert.AreNotEqual(31, result); // 31 is incorrect
            Assert.AreEqual(false, instance.IsValidDate());*/
            // TODO my expectation of what isValidDate meant is wrong
        }

        [Test]
        public void Test_SetDay()
        {
            GEDCOMDate instance = new GEDCOMDate(null, null, "", "");
            byte val = 20;
            instance.Day = val;
            Assert.AreEqual(val, instance.Day);
        }

        [Test]
        public void Test_SetDayInvalid()
        {
            GEDCOMDate instance = new GEDCOMDate(null, null, "", "");
            byte val = 99;
            instance.Day = val;
            Assert.AreEqual(val, instance.Day);
            Assert.AreEqual(false, instance.IsValidDate()); // TODO my expectation of what isValidDate meant is wrong
        }

        [Test]
        public void Test_GetMonth()
        {
            GEDCOMDate instance = GEDCOMDate.CreateByFormattedStr("20/12/1980", false);
            Assert.AreEqual(12, instance.Month);
        }

        [Test]
        public void Test_SetMonth()
        {
            GEDCOMDate instance = GEDCOMDate.CreateByFormattedStr("20/01/1980", false);
            instance.Month = 12;
            Assert.AreEqual(12, instance.Month);
        }

        [Test]
        public void Test_GetYear()
        {
            GEDCOMDate instance = GEDCOMDate.CreateByFormattedStr("20/01/1980", false);
            short expResult = 1980;
            short result = instance.Year;
            Assert.AreEqual(expResult, result);
        }

        [Test]
        public void Test_SetYear()
        {
            short value = 2001;
            GEDCOMDate instance = GEDCOMDate.CreateByFormattedStr("20/01/1980", false);
            instance.Year = value;
            short result = instance.Year;
            Assert.AreEqual(value, result);
        }

        [Test]
        public void Test_GetYearBC()
        {
            GEDCOMDate instance = new GEDCOMDate(null, null, "", "");
            bool expResult = false;
            bool result = instance.YearBC;
            Assert.AreEqual(expResult, result);
        }

        [Test]
        public void Test_SetYearBC()
        {
            bool value = true;
            GEDCOMDate instance = new GEDCOMDate(null, null, "", "");
            instance.YearBC = value;
            Assert.AreEqual(value, instance.YearBC);
        }

        [Test]
        public void Test_GetYearModifier()
        {
            GEDCOMDate instance = new GEDCOMDate(null, null, "", "");
            string expResult = "";
            string result = instance.YearModifier;
            Assert.AreEqual(expResult, result);
            instance.ParseString("20 DEC 1980/1");
            expResult = "1";
            result = instance.YearModifier;
            Assert.AreEqual(expResult, result);
        }

        [Test]
        public void Test_SetYearModifier()
        {
            string value = "2";
            GEDCOMDate instance = GEDCOMDate.CreateByFormattedStr("20/01/1980", false);
            instance.YearModifier = value;
            string result = instance.StringValue;
            Assert.AreEqual("20 JAN 1980/2", result);
        }

        [Test]
        public void Test_Create()
        {
            const string tagName = "BLAH";
            GEDCOMTag result = GEDCOMDate.Create(null, null, tagName, "");
            Assert.IsNotNull(result);
            Assert.AreEqual(tagName, result.Name);
        }

        [Test]
        public void Test_Clear()
        {
            GEDCOMDate instance = GEDCOMDate.CreateByFormattedStr("20/01/1980", false);
            instance.Clear();
            string result = instance.StringValue;
            Assert.AreEqual("", result);
        }

        [Test]
        public void Test_IsValidDate()
        {
            GEDCOMDate instance = new GEDCOMDate(null, null, "", "");
            bool expResult = false;
            bool result = instance.IsValidDate();
            Assert.AreEqual(expResult, result);
        }

        [Test]
        public void Test_IsEmpty()
        {
            GEDCOMDate instance = new GEDCOMDate(null, null, "", "");
            bool expResult = true;
            bool result = instance.IsEmpty();
            Assert.AreEqual(expResult, result);
        }

        [Test]
        public void Test_Assign()
        {
            GEDCOMDate instance = new GEDCOMDate(null, null);

            GEDCOMTag source = null;
            Assert.Throws(typeof(ArgumentException), () => { instance.Assign(source); });
        }

        private static DateTime ParseDT(string dtx)
        {
            return DateTime.ParseExact(dtx, "yyyy-MM-dd", CultureInfo.InvariantCulture);
        }

        [Test]
        public void Test_GetDateTime()
        {
            GEDCOMDate instance = GEDCOMDate.CreateByFormattedStr("20/01/1980", false);
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
            GEDCOMDate instance = new GEDCOMDate(null, null, "", "");
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
            string strValue = "20 DEC 1980";
            GEDCOMDate instance = new GEDCOMDate(null, null, "", "");
            string expResult = "";
            string result = instance.ParseString(strValue);
            Assert.AreEqual(expResult, result);
            Assert.AreEqual("20 DEC 1980", instance.StringValue);
        }

        [Test]
        public void Test_ParseString_system()
        {
            string strValue = "20.12.1980";
            GEDCOMDate instance = new GEDCOMDate(null, null, "", "");
            string expResult = "";
            string result = instance.ParseString(strValue);
            Assert.AreEqual(expResult, result);
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
            string strValue = "@#DUNKNOWN@ 20 DEC 1980";
            GEDCOMDate instance = new GEDCOMDate(null, null, "", "");
            instance.SetDate(GEDCOMCalendar.dcIslamic, 0, 0, 0);
            Assert.AreEqual(string.Empty, instance.ParseString(strValue));
            Assert.AreEqual("@#DUNKNOWN@ 20 DEC 1980", instance.StringValue);
        }

        [Test]
        public void Test_GetStringValue()
        {
            GEDCOMDate instance = new GEDCOMDate(null, null, "", "");
            instance.SetJulian(1, 3, 1980);
            Assert.AreEqual("@#DJULIAN@ 01 MAR 1980", instance.StringValue);
        }

        /*[Test]
        public void Test_GetMonthNumber()
        {
            GEDCOMDate instance = GEDCOMDate.CreateByFormattedStr("20/12/1980", false);
            int expResult = 12;
            int result = instance.GetMonthNumber();
            Assert.AreEqual(expResult, result);
        }*/

        [Test]
        public void Test_SetDate()
        {
            GEDCOMCalendar calendar = GEDCOMCalendar.dcGregorian;
            int day = 20;
            int month = 12;
            int year = 1980;
            GEDCOMDate instance = new GEDCOMDate(null, null, "", "");
            instance.SetDate(calendar, day, month, year);
            string result = instance.GetDisplayString(DateFormat.dfYYYY_MM_DD, false, false);
            Assert.AreEqual("1980.12.20", result);
            Assert.AreEqual("20 DEC 1980", instance.StringValue);
        }

        [Test]
        public void Test_SetDateJulian()
        {
            GEDCOMCalendar calendar = GEDCOMCalendar.dcJulian;
            int day = 20;
            int month = 12;
            int year = 1980;
            GEDCOMDate instance = new GEDCOMDate(null, null, "", "");
            instance.SetDate(calendar, day, month, year);
            string result = instance.GetDisplayString(DateFormat.dfYYYY_MM_DD, false, false);
            Assert.AreEqual("1980.12.20", result);
            Assert.AreEqual("@#DJULIAN@ 20 DEC 1980", instance.StringValue);
        }

        [Test]
        public void Test_SetDateFrench()
        {
            GEDCOMCalendar calendar = GEDCOMCalendar.dcFrench;
            int day = 20;
            int month = 12;
            int year = 1980;
            GEDCOMDate instance = new GEDCOMDate(null, null, "", "");
            instance.SetDate(calendar, day, month, year);
            string result = instance.GetDisplayString(DateFormat.dfYYYY_MM_DD, false, false);
            Assert.AreEqual("1980.12.20", result);
            Assert.AreEqual("@#DFRENCH R@ 20 FRUC 1980", instance.StringValue);
        }

        [Test]
        public void Test_SetDateHebrew()
        {
            GEDCOMCalendar calendar = GEDCOMCalendar.dcHebrew;
            int day = 20;
            int month = 12;
            int year = 1980;
            GEDCOMDate instance = new GEDCOMDate(null, null, "", "");
            instance.SetDate(calendar, day, month, year);
            string result = instance.GetDisplayString(DateFormat.dfYYYY_MM_DD, false, false);
            Assert.AreEqual("1980.12.20", result);
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
            int day = 20;
            int month = 12;
            int year = 1980;
            GEDCOMDate instance = new GEDCOMDate(null, null, "", "");
            instance.SetGregorian(day, month, year);
            string result = instance.GetDisplayString(DateFormat.dfYYYY_MM_DD, false, false);
            Assert.AreEqual("1980.12.20", result);
        }

        [Test]
        public void Test_SetGregorian_5args()
        {
            int day = 20;
            string month = "Dec";
            int year = 1980;
            string yearModifier = "";
            bool yearBC = false;
            GEDCOMDate instance = new GEDCOMDate(null, null, "", "");
            instance.SetGregorian(day, month, year, yearModifier, yearBC);
            string result = instance.GetDisplayString(DateFormat.dfYYYY_MM_DD, false, false);
            Assert.AreEqual("1980.12.20", result);
        }

        [Test]
        public void Test_SetGregorian_5argsBC()
        {
            int day = 20;
            string month = "Dec";
            int year = 1980;
            string yearModifier = "";
            bool yearBC = true;
            GEDCOMDate instance = new GEDCOMDate(null, null, "", "");
            instance.SetGregorian(day, month, year, yearModifier, yearBC);
            string result = instance.GetDisplayString(DateFormat.dfYYYY_MM_DD, true, false);
            Assert.AreEqual("BC 1980.12.20", result);
        }

        [Test]
        public void Test_SetJulian_3args()
        {
            int day = 20;
            int month = 12;
            int year = 1980;
            GEDCOMDate instance = new GEDCOMDate(null, null, "", "");
            instance.SetJulian(day, month, year);
            string result = instance.GetDisplayString(DateFormat.dfYYYY_MM_DD, false, false);
            Assert.AreEqual("1980.12.20", result);
        }

        [Test]
        public void Test_SetJulian_4args()
        {
            int day = 20;
            string month = "DEC";
            int year = 1980;
            bool yearBC = false;
            GEDCOMDate instance = new GEDCOMDate(null, null, "", "");
            instance.SetJulian(day, month, year, yearBC);
            string result = instance.GetDisplayString(DateFormat.dfYYYY_MM_DD, false, false);
            Assert.AreEqual("1980.12.20", result);
        }

        [Test]
        public void Test_SetHebrew_3args()
        {
            int day = 1;
            int month = 2;
            int year = 1980;
            GEDCOMDate instance = new GEDCOMDate(null, null, "", "");
            instance.SetHebrew(day, month, year);
            Assert.AreEqual(GEDCOMCalendar.dcHebrew, instance.DateCalendar);
            Assert.AreEqual("@#DHEBREW@ 01 CSH 1980", instance.StringValue);
        }

        [Test]
        public void Test_SetHebrew_4args()
        {
            int day = 1;
            string month = "TSH";
            int year = 1980;
            bool yearBC = false;
            GEDCOMDate instance = new GEDCOMDate(null, null, "", "");
            instance.SetHebrew(day, month, year, yearBC);
            Assert.AreEqual(GEDCOMCalendar.dcHebrew, instance.DateCalendar);
            Assert.AreEqual("@#DHEBREW@ 01 TSH 1980", instance.StringValue);

            // Code coverage
            instance.SetHebrew(day, "", year, yearBC);
            Assert.AreEqual(GEDCOMCalendar.dcHebrew, instance.DateCalendar);
        }

        [Test]
        public void Test_SetFrench_3args()
        {
            int day = 1;
            int month = 2;
            int year = 1980;
            GEDCOMDate instance = new GEDCOMDate(null, null, "", "");
            instance.SetFrench(day, month, year);
            Assert.AreEqual(GEDCOMCalendar.dcFrench, instance.DateCalendar);
            Assert.AreEqual("@#DFRENCH R@ 01 BRUM 1980", instance.StringValue);
        }

        [Test]
        public void Test_SetFrench_4args()
        {
            int day = 1;
            string month = "VEND";
            int year = 1980;
            bool yearBC = false;
            GEDCOMDate instance = new GEDCOMDate(null, null, "", "");
            instance.SetFrench(day, month, year, yearBC);
            Assert.AreEqual(GEDCOMCalendar.dcFrench, instance.DateCalendar);
            Assert.AreEqual("@#DFRENCH R@ 01 VEND 1980", instance.StringValue);

            // Code coverage
            instance.SetHebrew(day, "", year, yearBC);
            Assert.AreEqual(GEDCOMCalendar.dcHebrew, instance.DateCalendar);
        }

        [Test]
        public void Test_SetRoman()
        {
            int day = 1;
            string month = "JAN";
            int year = 1980;
            bool yearBC = false;
            GEDCOMDate instance = new GEDCOMDate(null, null, "", "");
            instance.SetRoman(day, month, year, yearBC);
            Assert.AreEqual(GEDCOMCalendar.dcRoman, instance.DateCalendar);
        }

        [Test]
        public void Test_SetUnknown()
        {
            int day = 0;
            string month = "";
            int year = 0;
            bool yearBC = false;
            GEDCOMDate instance = new GEDCOMDate(null, null, "", "");
            instance.SetUnknown(day, month, year, yearBC);
            Assert.AreEqual(GEDCOMCalendar.dcUnknown, instance.DateCalendar);
        }

        [Test]
        public void Test_DateChanged()
        {
            GEDCOMDate instance = GEDCOMDate.CreateByFormattedStr("31/11/1980", true);
            //instance.dateChanged();
            string result = instance.GetUDN().ToString();
            Assert.AreEqual("1980/12/01", result);
        }

        [Test]
        public void Test_GetUDN()
        {
            GEDCOMDate instance = GEDCOMDate.CreateByFormattedStr("20/12/1980", true);
            UDN expResult = new UDN(UDNCalendarType.ctGregorian, 1980, 12, 20);
            UDN result = instance.GetUDN();
            bool resu2 = expResult.Equals(result);
            Assert.IsTrue(resu2);
            Assert.AreEqual(expResult, result); // TODO Assert.AreEqual supposedly invokes .equals(), so why does this fail?
        }

        [Test]
        public void Test_CreateByFormattedStr_String_boolean()
        {
            Assert.AreEqual(null, GEDCOMDate.CreateByFormattedStr(null, false));
            Assert.AreEqual("20 DEC 1980", GEDCOMDate.CreateByFormattedStr("20/12/1980", false).StringValue);
            Assert.AreEqual("DEC 1980", GEDCOMDate.CreateByFormattedStr("__/12/1980", false).StringValue);
            Assert.AreEqual(null, GEDCOMDate.CreateByFormattedStr("1980", false));
            //Assert.Throws(typeof(GEDCOMDateException), () => { GEDCOMDate.createByFormattedStr("1980", true); });
        }

        [Test]
        public void Test_CreateByFormattedStr_3args()
        {
            Assert.AreEqual(null, GEDCOMDate.CreateByFormattedStr(null, GEDCOMCalendar.dcGregorian, false));
            Assert.AreEqual("20 DEC 1980", GEDCOMDate.CreateByFormattedStr("20/12/1980", GEDCOMCalendar.dcGregorian, false).StringValue);
            Assert.AreEqual("DEC 1980", GEDCOMDate.CreateByFormattedStr("__/12/1980", GEDCOMCalendar.dcGregorian, false).StringValue);
            Assert.AreEqual(null, GEDCOMDate.CreateByFormattedStr("1980", GEDCOMCalendar.dcGregorian, false));
            //Assert.Throws(typeof(GEDCOMDateException), () => { GEDCOMDate.createByFormattedStr("1980", GEDCOMCalendar.dcGregorian, true); });
        }

        [Test]
        public void Test_GetUDNByFormattedStr()
        {
            string dateStr = "";
            UDN expResult = UDN.CreateEmpty();
            UDN result = GEDCOMDate.GetUDNByFormattedStr(dateStr, GEDCOMCalendar.dcGregorian);
            Assert.AreEqual(expResult, result);
        }

        [Test]
        public void Test_GetDisplayString_DateFormat_boolean()
        {
            DateFormat format = DateFormat.dfDD_MM_YYYY;
            bool includeBC = false;
            GEDCOMDate instance = GEDCOMDate.CreateByFormattedStr("20/12/1980", true);
            string expResult = "20.12.1980";
            string result = instance.GetDisplayString(format, includeBC);
            Assert.AreEqual(expResult, result);
        }

        [Test]
        public void Test_GetDisplayString_DateFormat()
        {
            GEDCOMDate instance = GEDCOMDate.CreateByFormattedStr("20/12/1980", true);
            string expResult = "1980.12.20";
            string result = instance.GetDisplayString(DateFormat.dfYYYY_MM_DD);
            Assert.AreEqual(expResult, result);

            result = instance.GetDisplayString(DateFormat.dfDD_MM_YYYY);
            Assert.AreEqual("20.12.1980", result);

            result = instance.GetDisplayString(DateFormat.dfYYYY);
            Assert.AreEqual("1980", result);
        }

        [Test]
        public void Test_GetDisplayString_3args()
        {
            DateFormat format = DateFormat.dfYYYY_MM_DD;
            bool includeBC = true;
            bool showCalendar = true;
            GEDCOMDate instance = new GEDCOMDate(null, null, "", "");
            instance.SetJulian(20, 12, 1980);
            instance.YearBC = true;
            string expResult = "BC 1980.12.20 [J]";
            string result = instance.GetDisplayString(format, includeBC, showCalendar);
            Assert.AreEqual(expResult, result);

            result = instance.GetDisplayString(DateFormat.dfDD_MM_YYYY, true, false);
            Assert.AreEqual("20.12.1980 BC", result);

            result = instance.GetDisplayString(DateFormat.dfYYYY, true, false);
            Assert.AreEqual("BC 1980", result);
        }

        [Test]
        public void Test_GetDisplayStringExt()
        {
            DateFormat format = DateFormat.dfYYYY_MM_DD;
            bool showCalendar = true;
            GEDCOMDate instance = new GEDCOMDate(null, null, "", "");
            instance.SetJulian(20, 12, 1980);
            instance.YearBC = true;
            string expResult = "BC 1980.12.20 [J]";
            bool sign = true;
            string result = instance.GetDisplayStringExt(format, sign, showCalendar);
            Assert.AreEqual(expResult, result);

            instance.Approximated = GEDCOMApproximated.daEstimated;
            expResult = "~ BC 1980.12.20 [J]";
            result = instance.GetDisplayStringExt(format, sign, showCalendar);
            Assert.AreEqual(expResult, result);
        }

        [Test]
        public void Test_CreateByFormattedStr_exception()
        {
            Assert.Throws(typeof(GEDCOMDateException), () => {
                              GEDCOMDate.CreateByFormattedStr("1.2", true);
                          });
        }

        [Test]
        public void Test_getUDNByFormattedStr2()
        {
            //Assert.Throws(typeof(GEDCOMDateException), () => {
            var dtx = GEDCOMDate.GetUDNByFormattedStr("20-12-1980", GEDCOMCalendar.dcGregorian, true);
            Assert.AreEqual("1980/12/20", dtx.ToString());
            //});
        }

        /*
         * For code coverage: exercise Ahnenblatt date parsing
         */
        [Test]
        public void Test_AhnenblattDate()
        {
            string gedcom = "0 HEAD\n1 SOUR AHN\n0 @I1@ INDI\n1 BIRT\n2 DATE (20/12-1980)";

            // TODO this bit needs to go into utility class
            GEDCOMTree tee = new GEDCOMTree();
            GEDCOMProvider gp = new GEDCOMProvider(tee);
            try {
                gp.LoadFromString(gedcom);
            } catch (Exception) {
            }
            Assert.AreEqual(1, tee.RecordsCount);
            GEDCOMRecord rec = tee[0];
            Assert.IsTrue(rec is GEDCOMIndividualRecord);
            GEDCOMIndividualRecord rec2 = (GEDCOMIndividualRecord)rec;
            // end for utility class

            GEDCOMList<GEDCOMCustomEvent> events = rec2.Events;
            Assert.AreEqual(1, events.Count);
            GEDCOMCustomEvent birt = events.Extract(0);
            GEDCOMDateValue dv = birt.Date;
            Assert.AreEqual("20 DEC 1980", dv.StringValue);
        }

        [Test]
        public void Test_DateInterpreted()
        {
            DateTime expectDate = TestUtils.ParseDT("20.01.2013");

            using (var dtx1 = new GEDCOMDateInterpreted(null, null)) {
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

                Assert.Throws(typeof(GEDCOMDateException), () => { dtx1.ParseString("10 JAN 2013 (today)"); });
            }
        }

        [Test]
        public void Test_CreateByFormattedStr()
        {
            Assert.AreEqual(null, GEDCOMDate.CreateByFormattedStr(null, false));
            Assert.AreEqual("20 DEC 1980", GEDCOMDate.CreateByFormattedStr("20/12/1980", false).StringValue);
            Assert.AreEqual("DEC 1980", GEDCOMDate.CreateByFormattedStr("__/12/1980", false).StringValue);
            Assert.AreEqual(null, GEDCOMDate.CreateByFormattedStr("1980", false));
            
            Assert.Throws(typeof(GEDCOMDateException), () => { GEDCOMDate.CreateByFormattedStr("1980", true); });
        }
    }
}
