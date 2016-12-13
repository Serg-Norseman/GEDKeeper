using System;
using System.Collections.Generic;
using GKCommon;
using NUnit.Framework;

namespace GKTests.GKCommon
{
    public class UDNRecord
    {
        public readonly UDNCalendarType Calendar;
        public readonly string Description;
        public readonly UDN Value;
        public readonly string StrVal;

        public UDNRecord(UDNCalendarType calendar, int year, int month, int day, string description)
        {
            this.Calendar = calendar;
            this.Description = description;
            this.Value = new UDN(calendar, year, month, day);
            this.StrVal = this.Value.ToString();
        }

        public UDNRecord(UDN udn, UDNCalendarType calendar, string description)
        {
            this.Calendar = calendar;
            this.Description = description;
            this.Value = udn;
            this.StrVal = this.Value.ToString();
        }
    }

    /// <summary>
    /// Description of UDNTests.
    /// </summary>
    [TestFixture]
    public class UDNTests
    {
        private List<UDNRecord> fDates = new List<UDNRecord>();

        public UDNTests()
        {
        }

        [Test]
        public void UDNSort_Tests()
        {
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

            Assert.AreEqual("??/??/?? [g]", fDates[0].Description, "(00)");
            Assert.AreEqual("??/??/23 [g]", fDates[1].Description, "(01)");
            Assert.AreEqual("0000/01/03 [g]", fDates[2].Description, "(02)");
            Assert.AreEqual("??/03/23 [g]", fDates[3].Description, "(03)");
            Assert.AreEqual("??/04/23 [g]", fDates[4].Description, "(04)");
            Assert.AreEqual("before ????/05/31 [g]", fDates[5].Description, "(05)");
            Assert.AreEqual("~ ????/05/31 [g]", fDates[6].Description, "(06)");
            Assert.AreEqual("after ????/05/31 [g]", fDates[7].Description, "(07)");
            Assert.AreEqual("after ????/06/15 [g]", fDates[8].Description, "(08)");
            Assert.AreEqual("before -4712/01/02 [g]", fDates[9].Description, "(09)");

            Assert.AreEqual("-4712/01/02 [g]", fDates[10].Description, "(10)");
            Assert.AreEqual("-4712/01/03 [g]", fDates[11].Description, "(11)");
            Assert.AreEqual("-0001/01/03 [g]", fDates[12].Description, "(12)");
            Assert.AreEqual("~ 0001/01/?? [g]", fDates[13].Description, "(13)");
            Assert.AreEqual("0001/01/03 [g]", fDates[14].Description, "(14)");
            Assert.AreEqual("before 0001/01/04 [g]", fDates[15].Description, "(15)");
            Assert.AreEqual("~ 0001/01/04 [g]", fDates[16].Description, "(16)");
            Assert.AreEqual("1801/05/17 [g] = 1216/01/04 [i]", fDates[17].Description, "(17)");
            Assert.AreEqual("1802/05/01 [g]", fDates[18].Description, "(18)");
            Assert.AreEqual("1804/06/13 [g] = 5564/04/04 [h]", fDates[19].Description, "(19)");

            Assert.AreEqual("2014/??/23 [g]", fDates[20].Description, "(20)");
            Assert.AreEqual("after 2015/??/30 [g]", fDates[21].Description, "(21)");
            Assert.AreEqual("before 2015/??/31 [g]", fDates[22].Description, "(22)");
            Assert.AreEqual("~ 2015/??/31 [g]", fDates[23].Description, "(23)");
            Assert.AreEqual("after 2015/??/31 [g]", fDates[24].Description, "(24)");
            Assert.AreEqual("2015/02/27 [g]", fDates[25].Description, "(25)");
            Assert.AreEqual("~ 2015/02/28 [g]", fDates[26].Description, "(26)");
            Assert.AreEqual("2015/03/23 [g]", fDates[27].Description, "(27)");
            Assert.AreEqual("2016/??/?? [g]", fDates[28].Description, "(28)");
            Assert.AreEqual("2016/??/10 [g]", fDates[29].Description, "(29)");

            Assert.AreEqual("2016/05/?? [g]", fDates[30].Description, "(30)");
            Assert.AreEqual("2016/05/04 [g] = 2016/04/21 [j]", fDates[31].Description, "(31)");
            Assert.AreEqual("2016/05/04 [g]", fDates[32].Description, "(32)");
            Assert.AreEqual("2016/05/05 [g]", fDates[33].Description, "(33)");
            Assert.AreEqual("2016/05/06 [g] = 2016/04/23 [j]", fDates[34].Description, "(34)");
            Assert.AreEqual("before 2016/05/31 [g]", fDates[35].Description, "(35)");
            Assert.AreEqual("~ 2016/05/31 [g]", fDates[36].Description, "(36)");
            Assert.AreEqual("2016/05/31 [g]", fDates[37].Description, "(37)");
            Assert.AreEqual("2016/05/31 [g]", fDates[38].Description, "(38)");
            Assert.AreEqual("after 2016/05/31 [g]", fDates[39].Description, "(39)");

            Assert.AreEqual("2016/06/?? [g]", fDates[40].Description, "(40)");
            Assert.AreEqual("3268/01/23 [g]", fDates[41].Description, "(41)");

            UDNRecord rec;
            Assert.Throws(typeof(ArgumentOutOfRangeException), () => { rec = fDates[42]; }, "(42)"); // end
        }

        [Test(Description = "UDN Exceptions Test")]
        [ExpectedException(typeof(Exception))]
        public void UDNExceptions_Tests()
        {
            UDN.CreateBetween(new UDN(UDNCalendarType.ctGregorian, UDN.UnknownYear, 05, 05),
                              new UDN(UDNCalendarType.ctGregorian, UDN.UnknownYear, 05, 07));
        }

        [Test]
        public void UDNCommon_Tests()
        {
            UDN testUDNAft = UDN.CreateAfter(UDNCalendarType.ctGregorian, 1900, 11, 0);
            Assert.AreEqual(">1900/11/??", testUDNAft.ToString());

            UDN testUDNBef = UDN.CreateBefore(UDNCalendarType.ctGregorian, 1900, 0, 17);
            Assert.AreEqual("<1900/??/17", testUDNBef.ToString());

            UDN testUDNApp = UDN.CreateApproximate(UDNCalendarType.ctGregorian, 0, 5, 14);
            Assert.AreEqual("~????/05/14", testUDNApp.ToString());

            Assert.AreEqual(-1, testUDNBef.CompareTo(testUDNAft));
            Assert.AreEqual(-1, testUDNBef.CompareTo((object) testUDNAft));
            Assert.AreEqual(-1, testUDNBef.CompareTo(null));

            UDN testUDN2 = (UDN)testUDNApp.Clone();
            Assert.AreEqual("~????/05/14", testUDN2.ToString());
            Assert.IsFalse(testUDN2.IsEmpty());

            //Assert.IsTrue(testUDN2.GetHashCode() != 0);

            Assert.Throws(typeof(Exception), () => {
                              UDN.CreateBetween(new UDN(UDNCalendarType.ctGregorian, UDN.UnknownYear, 05, 05),
                                                new UDN(UDNCalendarType.ctGregorian, UDN.UnknownYear, 05, 07)); });

            // checkpoints by JD calendar bounds
            Assert.AreEqual("-4713/11/24", new UDN(UDNCalendarType.ctGregorian, -4713, 11, 24).ToString());
            Assert.AreEqual("-4713/11/24", new UDN(UDNCalendarType.ctJulian, -4712, 01, 01).ToString());

            // common tests

            Assert.AreEqual("2016/05/05", new UDN(UDNCalendarType.ctGregorian, 2016, 05, 05).ToString());
            Assert.AreEqual("2016/05/04", new UDN(UDNCalendarType.ctGregorian, 2016, 05, 04).ToString());

            Assert.AreEqual("2016/05/04", new UDN(UDNCalendarType.ctJulian, 2016, 04, 21).ToString());
            Assert.AreEqual("2016/05/06", new UDN(UDNCalendarType.ctJulian, 2016, 04, 23).ToString());

            Assert.AreEqual("2016/05/??", new UDN(UDNCalendarType.ctGregorian, 2016, 05, UDN.UnknownDay).ToString());
            Assert.AreEqual("2016/06/??", new UDN(UDNCalendarType.ctGregorian, 2016, 06, UDN.UnknownDay).ToString());

            Assert.AreEqual("????/??/??", new UDN(UDNCalendarType.ctGregorian, UDN.UnknownYear, UDN.UnknownMonth, UDN.UnknownDay).ToString());
            Assert.AreEqual("????/04/23", new UDN(UDNCalendarType.ctGregorian, UDN.UnknownYear, 04, 23).ToString());
            Assert.AreEqual("????/03/23", new UDN(UDNCalendarType.ctGregorian, UDN.UnknownYear, 03, 23).ToString());
            Assert.AreEqual("????/??/23", new UDN(UDNCalendarType.ctGregorian, UDN.UnknownYear, UDN.UnknownMonth, 23).ToString());
            Assert.AreEqual("2016/??/??", new UDN(UDNCalendarType.ctGregorian, 2016, UDN.UnknownMonth, UDN.UnknownDay).ToString());
            Assert.AreEqual("2016/??/10", new UDN(UDNCalendarType.ctGregorian, 2016, UDN.UnknownMonth, 10).ToString());
            Assert.AreEqual("2015/03/23", new UDN(UDNCalendarType.ctGregorian, 2015, 03, 23).ToString());
            Assert.AreEqual("2014/??/23", new UDN(UDNCalendarType.ctGregorian, 2014, UDN.UnknownMonth, 23).ToString());
            Assert.AreEqual("2016/05/31", new UDN(UDNCalendarType.ctGregorian, 2016, 05, 31).ToString());
            Assert.AreEqual("2016/05/31", new UDN(UDNCalendarType.ctGregorian, 2016, 05, 31).ToString());

            Assert.AreEqual("-4712/01/03", new UDN(UDNCalendarType.ctGregorian, -4712, 1, 3).ToString());
            Assert.AreEqual("-4528/02/29", new UDN(UDNCalendarType.ctGregorian, -4528, 2, 29).ToString());

            Assert.AreEqual("1804/06/13", new UDN(UDNCalendarType.ctHebrew, 5564, 04, 04).ToString());
            Assert.AreEqual("1801/05/17", new UDN(UDNCalendarType.ctIslamic, 1216, 01, 04).ToString());
            Assert.AreEqual("1802/05/01", new UDN(UDNCalendarType.ctGregorian, 1802, 05, 01).ToString());

            Assert.AreEqual("????/01/03", new UDN(UDNCalendarType.ctGregorian, 0, 1, 3).ToString());
            Assert.AreEqual("-0001/01/03", new UDN(UDNCalendarType.ctGregorian, -1, 1, 3).ToString());

            Assert.AreEqual("0001/01/03", new UDN(UDNCalendarType.ctGregorian, 1, 1, 3).ToString());
            Assert.AreEqual("2015/02/27", new UDN(UDNCalendarType.ctGregorian, 2015, 2, 27).ToString());
            Assert.AreEqual("3268/01/23", new UDN(UDNCalendarType.ctGregorian, 3268, 1, 23).ToString());

            // dates before
            Assert.AreEqual("<0001/01/04", UDN.CreateBefore(UDNCalendarType.ctGregorian, 1, 1, 4).ToString());
            Assert.AreEqual("<2016/05/31", UDN.CreateBefore(UDNCalendarType.ctGregorian, 2016, 05, 31).ToString());
            Assert.AreEqual("<-4712/01/02", UDN.CreateBefore(UDNCalendarType.ctGregorian, -4712, 1, 2).ToString());
            Assert.AreEqual("<????/05/31", UDN.CreateBefore(UDNCalendarType.ctGregorian, UDN.UnknownYear, 05, 31).ToString());
            Assert.AreEqual("<2015/??/31", UDN.CreateBefore(UDNCalendarType.ctGregorian, 2015, UDN.UnknownMonth, 31).ToString());

            // dates after
            Assert.AreEqual(">2016/05/31", UDN.CreateAfter(UDNCalendarType.ctGregorian, 2016, 05, 31).ToString());
            Assert.AreEqual(">????/05/31", UDN.CreateAfter(UDNCalendarType.ctGregorian, UDN.UnknownYear, 05, 31).ToString());
            Assert.AreEqual(">????/06/15", UDN.CreateAfter(UDNCalendarType.ctGregorian, UDN.UnknownYear, 06, 15).ToString());
            Assert.AreEqual(">2015/??/31", UDN.CreateAfter(UDNCalendarType.ctGregorian, 2015, UDN.UnknownMonth, 31).ToString());
            Assert.AreEqual(">2015/??/30", UDN.CreateAfter(UDNCalendarType.ctGregorian, 2015, UDN.UnknownMonth, 30).ToString());

            // approximate dates
            Assert.AreEqual("~2016/05/31", UDN.CreateApproximate(UDNCalendarType.ctGregorian, 2016, 05, 31).ToString());
            Assert.AreEqual("~0001/01/04", UDN.CreateApproximate(UDNCalendarType.ctGregorian, 1, 1, 4).ToString());
            Assert.AreEqual("~0001/01/??", UDN.CreateApproximate(UDNCalendarType.ctGregorian, 1, 1, UDN.UnknownDay).ToString());
            Assert.AreEqual("~????/05/31", UDN.CreateApproximate(UDNCalendarType.ctGregorian, UDN.UnknownYear, 05, 31).ToString());
            Assert.AreEqual("~2015/??/31", UDN.CreateApproximate(UDNCalendarType.ctGregorian, 2015, UDN.UnknownMonth, 31).ToString());
            Assert.AreEqual("~2015/02/28", UDN.CreateApproximate(UDNCalendarType.ctGregorian, 2015, 2, 28).ToString());

            // compare test
            Assert.AreEqual(1, new UDN(UDNCalendarType.ctGregorian, 1910, 10, 10).CompareTo(new UDN(UDNCalendarType.ctGregorian, 1910, 10, 09)));
            Assert.AreEqual(-1, new UDN(UDNCalendarType.ctGregorian, 1910, 10, 09).CompareTo(new UDN(UDNCalendarType.ctGregorian, 1910, 10, 10)));
        }
    }
}
