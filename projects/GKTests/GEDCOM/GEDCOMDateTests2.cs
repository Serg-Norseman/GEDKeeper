/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

using System;
using System.Globalization;
using GKCommon.GEDCOM;
using GKCore.Types;
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
    public class GEDCOMDateTest
    {
        public GEDCOMDateTest()
        {
        }

        [Test]
        public void testGetApproximated()
        {
            GEDCOMDate instance = new GEDCOMDate(null, null, "", "");
            GEDCOMApproximated expResult = GEDCOMApproximated.daExact;
            GEDCOMApproximated result = instance.Approximated;
            Assert.AreEqual(expResult, result);
        }

        [Test]
        public void testSetApproximated()
        {
            GEDCOMApproximated value = GEDCOMApproximated.daAbout;
            GEDCOMDate instance = new GEDCOMDate(null, null, "", "");
            instance.Approximated = value;
            Assert.AreEqual(value, instance.Approximated);
        }

        [Test]
        public void testGetDateCalendar()
        {
            GEDCOMDate instance = GEDCOMDate.CreateByFormattedStr("20/12/1980", false);
            GEDCOMCalendar result = instance.DateCalendar;
            Assert.AreEqual(GEDCOMCalendar.dcGregorian, result);
            instance = GEDCOMDate.CreateByFormattedStr("20/12/1980", GEDCOMCalendar.dcJulian, false);
            result = instance.DateCalendar;
            Assert.AreEqual(GEDCOMCalendar.dcJulian, result);
        }

        [Test]
        public void testGetDay()
        {
            GEDCOMDate instance = GEDCOMDate.CreateByFormattedStr("20/12/1980", false);
            byte result = instance.Day;
            Assert.AreEqual(20, result);
        }

        [Test]
        public void testGetDayInvalid()
        {
            /*GEDCOMDate instance = GEDCOMDate.CreateByFormattedStr("31/11/1980", true);
            int result = instance.Day;
            Assert.AreNotEqual(31, result); // 31 is incorrect
            Assert.AreEqual(false, instance.IsValidDate());*/
            // TODO my expectation of what isValidDate meant is wrong
        }
        
        [Test]
        public void testSetDay()
        {
            GEDCOMDate instance = new GEDCOMDate(null, null, "", "");
            byte val = 20;
            instance.Day = val;
            Assert.AreEqual(val, instance.Day);
        }

        [Test]
        public void testSetDayInvalid()
        {
            GEDCOMDate instance = new GEDCOMDate(null, null, "", "");
            byte val = 99;
            instance.Day = val;
            Assert.AreEqual(val, instance.Day);
            Assert.AreEqual(false, instance.IsValidDate()); // TODO my expectation of what isValidDate meant is wrong
        }
        
        [Test]
        public void testGetMonth()
        {
            GEDCOMDate instance = GEDCOMDate.CreateByFormattedStr("20/12/1980", false);
            string expResult = "DEC";
            string result = instance.Month;
            Assert.AreEqual(expResult, result);
        }

        [Test]
        public void testSetMonth()
        {
            string value = "DEC";
            GEDCOMDate instance = GEDCOMDate.CreateByFormattedStr("20/01/1980", false);
            instance.Month = value;
            string result = instance.Month;
            Assert.AreEqual(value, result);
        }

        [Test]
        public void testGetYear()
        {
            GEDCOMDate instance = GEDCOMDate.CreateByFormattedStr("20/01/1980", false);
            short expResult = 1980;
            short result = instance.Year;
            Assert.AreEqual(expResult, result);
        }

        [Test]
        public void testSetYear()
        {
            short value = 2001;
            GEDCOMDate instance = GEDCOMDate.CreateByFormattedStr("20/01/1980", false);
            instance.Year = value;
            short result = instance.Year;
            Assert.AreEqual(value, result);
        }

        [Test]
        public void testGetYearBC()
        {
            GEDCOMDate instance = new GEDCOMDate(null, null, "", "");
            bool expResult = false;
            bool result = instance.YearBC;
            Assert.AreEqual(expResult, result);
        }

        [Test]
        public void testSetYearBC()
        {
            bool value = true;
            GEDCOMDate instance = new GEDCOMDate(null, null, "", "");
            instance.YearBC = value;
            Assert.AreEqual(value, instance.YearBC);
        }

        [Test]
        public void testGetYearModifier()
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
        public void testSetYearModifier()
        {
            string value = "2";
            GEDCOMDate instance = GEDCOMDate.CreateByFormattedStr("20/01/1980", false);
            instance.YearModifier = value;
            string result = instance.StringValue;
            Assert.AreEqual("20 JAN 1980/2", result);
        }

        [Test]
        public void testCreate()
        {
            GEDCOMTree owner = null;
            GEDCOMObject parent = null;
            string tagName = "BLAH";
            string tagValue = "";
            GEDCOMTag expResult = null;
            GEDCOMTag result = GEDCOMDate.Create(owner, parent, tagName, tagValue);
            Assert.IsNotNull(result);
            Assert.AreEqual(tagName, result.Name);
        }

        [Test]
        public void testClear()
        {
            GEDCOMDate instance = GEDCOMDate.CreateByFormattedStr("20/01/1980", false);
            instance.Clear();
            string result = instance.StringValue;
            Assert.AreEqual("", result);
        }

        [Test]
        public void testIsValidDate()
        {
            GEDCOMDate instance = new GEDCOMDate(null, null, "", "");
            bool expResult = false;
            bool result = instance.IsValidDate();
            Assert.AreEqual(expResult, result);
        }

        [Test]
        public void testIsEmpty()
        {
            GEDCOMDate instance = new GEDCOMDate(null, null, "", "");
            bool expResult = true;
            bool result = instance.IsEmpty();
            Assert.AreEqual(expResult, result);
        }

        [Test]
        public void testAssign()
        {
            // TODO review the generated test code and remove the default call to fail.
            GEDCOMDate instance = new GEDCOMDate(null, null, "", "");

            GEDCOMTag source = null;
            //Assert.Throws(typeof(ArgumentNullException), () => {
            instance.Assign(source);
            //});
        }

        private static DateTime ParseDT(string dtx)
        {
            return DateTime.ParseExact(dtx, "yyyy-MM-dd", CultureInfo.InvariantCulture);
        }

        [Test]
        public void testGetDateTime()
        {
            GEDCOMDate instance = GEDCOMDate.CreateByFormattedStr("20/01/1980", false);
            DateTime expResult;
            try {
                expResult = ParseDT("1980-01-20");
                DateTime result = instance.GetDateTime();
                Assert.AreEqual(expResult, result);
            } catch (Exception ex) {
                Assert.Fail("Parse exception for date");
            }
        }

        [Test]
        public void testSetDateTime()
        {
            GEDCOMDate instance = new GEDCOMDate(null, null, "", "");
            DateTime expResult;
            try {
                expResult = ParseDT("1980-01-20");
                instance.SetDateTime(expResult);
                Assert.AreEqual(expResult, instance.GetDateTime());
            } catch (Exception ex) {
                Assert.Fail("Parse exception for date");
            }
        }

        [Test]
        public void testParseString()
        {
            string strValue = "20 DEC 1980";
            GEDCOMDate instance = new GEDCOMDate(null, null, "", "");
            string expResult = "";
            string result = instance.ParseString(strValue);
            Assert.AreEqual(expResult, result);
            Assert.AreEqual("20 DEC 1980", instance.StringValue);
        }

        [Test]
        public void testParseString_system()
        {
            string strValue = "20.12.1980";
            GEDCOMDate instance = new GEDCOMDate(null, null, "", "");
            string expResult = "";
            string result = instance.ParseString(strValue);
            Assert.AreEqual(expResult, result);
            Assert.AreEqual("20 DEC 1980", instance.StringValue);
        }
        
        [Test]
        public void testParseStringIslamic()
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
        public void testParseStringUnknown()
        {
            string strValue = "@#DUNKNOWN@ 20 DEC 1980";
            GEDCOMDate instance = new GEDCOMDate(null, null, "", "");
            instance.SetDate(GEDCOMCalendar.dcIslamic, 0, 0, 0);
            string expResult = "";
            string result = instance.ParseString(strValue);
            Assert.AreEqual(expResult, result);
            Assert.AreEqual("@#DUNKNOWN@ 20 DEC 1980", instance.StringValue);
        }

        [Test]
        public void testGetStringValue()
        {
            GEDCOMDate instance = new GEDCOMDate(null, null, "", "");
            instance.SetJulian(1, 3, 1980);
            Assert.AreEqual("@#DJULIAN@ 01 MAR 1980", instance.StringValue);
        }

        [Test]
        public void testGetMonthNumber()
        {
            GEDCOMDate instance = GEDCOMDate.CreateByFormattedStr("20/12/1980", false);
            int expResult = 12;
            int result = instance.GetMonthNumber();
            Assert.AreEqual(expResult, result);
        }

        [Test]
        public void testSetDate()
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
        public void testSetDateJulian()
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
        public void testSetDateFrench()
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
        public void testSetDateHebrew()
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
        public void testSetDateRoman()
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
        public void testSetGregorian_3args()
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
        public void testSetGregorian_5args()
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
        public void testSetGregorian_5argsBC()
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
        public void testSetJulian_3args()
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
        public void testSetJulian_4args()
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
        public void testSetHebrew_3args()
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
        public void testSetHebrew_4args()
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
        public void testSetFrench_3args()
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
        public void testSetFrench_4args()
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
        public void testSetRoman()
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
        public void testSetUnknown()
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
        public void testDateChanged()
        {
            GEDCOMDate instance = GEDCOMDate.CreateByFormattedStr("31/11/1980", true);
            //instance.dateChanged();
            string result = instance.GetUDN().ToString();
            Assert.AreEqual("1980/12/01", result);
        }

        [Test]
        public void testGetUDN()
        {
            GEDCOMDate instance = GEDCOMDate.CreateByFormattedStr("20/12/1980", true);
            UDN expResult = new UDN(UDNCalendarType.ctGregorian, 1980, 12, 20);
            UDN result = instance.GetUDN();
            bool resu2 = expResult.Equals(result);
            Assert.IsTrue(resu2);
            Assert.AreEqual(expResult, result); // TODO Assert.AreEqual supposedly invokes .equals(), so why does this fail?
        }

        [Test]
        public void testCreateByFormattedStr_String_boolean()
        {
            Assert.AreEqual(null, GEDCOMDate.CreateByFormattedStr(null, false));
            Assert.AreEqual("20 DEC 1980", GEDCOMDate.CreateByFormattedStr("20/12/1980", false).StringValue);
            Assert.AreEqual("DEC 1980", GEDCOMDate.CreateByFormattedStr("__/12/1980", false).StringValue);
            Assert.AreEqual(null, GEDCOMDate.CreateByFormattedStr("1980", false));
            //Assert.Throws(typeof(GEDCOMDateException), () => { GEDCOMDate.createByFormattedStr("1980", true); });
        }

        [Test]
        public void testCreateByFormattedStr_3args()
        {
            Assert.AreEqual(null, GEDCOMDate.CreateByFormattedStr(null, GEDCOMCalendar.dcGregorian, false));
            Assert.AreEqual("20 DEC 1980", GEDCOMDate.CreateByFormattedStr("20/12/1980", GEDCOMCalendar.dcGregorian, false).StringValue);
            Assert.AreEqual("DEC 1980", GEDCOMDate.CreateByFormattedStr("__/12/1980", GEDCOMCalendar.dcGregorian, false).StringValue);
            Assert.AreEqual(null, GEDCOMDate.CreateByFormattedStr("1980", GEDCOMCalendar.dcGregorian, false));
            //Assert.Throws(typeof(GEDCOMDateException), () => { GEDCOMDate.createByFormattedStr("1980", GEDCOMCalendar.dcGregorian, true); });
        }

        [Test]
        public void testGetUDNByFormattedStr()
        {
            string dateStr = "";
            UDN expResult = UDN.CreateEmpty();
            UDN result = GEDCOMDate.GetUDNByFormattedStr(dateStr, GEDCOMCalendar.dcGregorian);
            Assert.AreEqual(expResult, result);
        }

        [Test]
        public void testGetDisplayString_DateFormat_boolean()
        {
            DateFormat format = DateFormat.dfDD_MM_YYYY;
            bool includeBC = false;
            GEDCOMDate instance = GEDCOMDate.CreateByFormattedStr("20/12/1980", true);
            string expResult = "20.12.1980";
            string result = instance.GetDisplayString(format, includeBC);
            Assert.AreEqual(expResult, result);
        }

        [Test]
        public void testGetDisplayString_DateFormat()
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
        public void testGetDisplayString_3args()
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
        public void testGetDisplayStringExt()
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
        public void testCreateByFormattedStr_exception()
        {
            Assert.Throws(typeof(GEDCOMDateException), () => {
                GEDCOMDate.CreateByFormattedStr("1.2", true);
            });
        }

        [Test]
        public void testgetUDNByFormattedStr_exception()
        {
            Assert.Throws(typeof(GEDCOMDateException), () => {
                GEDCOMDate.GetUDNByFormattedStr("20-12-1980", GEDCOMCalendar.dcGregorian, true);
            });
        }

        /*
         * For code coverage: exercise Ahnenblatt date parsing
         */
        [Test]
        public void testAhnenblattDate()
        {
            string gedcom = "0 HEAD\n1 SOUR AHN\n0 @I1@ INDI\n1 BIRT\n2 DATE (20/12-1980)";
            
            // TODO this bit needs to go into utility class
            GEDCOMTree tee = new GEDCOMTree();
            GEDCOMProvider gp = new GEDCOMProvider(tee);
            try {
                gp.LoadFromString(gedcom);
            } catch (Exception e) {
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
    }
}