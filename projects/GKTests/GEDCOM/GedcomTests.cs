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
using System.IO;
using System.Reflection;
using System.Text;
using BSLib;
using BSLib.Calendar;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Types;
using GKTests;
using GKUI.Providers;
using NUnit.Framework;

namespace GKCommon.GEDCOM
{
    [TestFixture]
    public class GedcomTests
    {
        private BaseContext fContext;

        [TestFixtureSetUp]
        public void SetUp()
        {
            // TempDirtyHack: some functions are references to GlobalOptions (and GfxInit)
            // TODO: replace to mocks
            WFAppHost.ConfigureBootstrap(false);

            fContext = TestUtils.CreateContext();
            TestUtils.FillContext(fContext);
        }

        [TestFixtureTearDown]
        public void TearDown()
        {
        }

        #region True Tests

        private GEDCOMTag TagConstructorTest(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
        {
            return null;
        }

        [Test]
        public void GEDCOMFactory_Tests()
        {
            TagConstructor tagConst = TagConstructorTest;
            Assert.AreEqual(null, tagConst.Invoke(null, null, "x", "x"));

            //

            GEDCOMFactory f = GEDCOMFactory.GetInstance();
            Assert.IsNotNull(f, "f != null");

            f.RegisterTag("DATE", GEDCOMDateValue.Create);

            GEDCOMTag tag = f.CreateTag(null, null, "DATE", "");
            Assert.IsNotNull(tag, "tag != null");

            tag = f.CreateTag(null, null, "TEST", "");
            Assert.IsNull(tag, "tag == null");
        }

        [Test]
        public void GEDCOMMathes_Tests()
        {
            GEDCOMTree tree = new GEDCOMTree();
            Assert.IsNotNull(tree);

            GEDCOMIndividualRecord ind1, ind2;
            GEDCOMCustomEvent ev1, ev2;
            GEDCOMDateValue dtVal1, dtVal2;

            ind1 = tree.CreateIndividual();
            ind1.Sex = GEDCOMSex.svMale;
            GEDCOMPersonalName pn = ind1.AddPersonalName(new GEDCOMPersonalName(tree, ind1, "", ""));
            pn.SetNameParts("Ivan Ivanov", "Fedoroff", "");

            ind2 = tree.CreateIndividual();
            ind2.Sex = GEDCOMSex.svMale;
            pn = ind2.AddPersonalName(new GEDCOMPersonalName(tree, ind2, "", ""));
            pn.SetNameParts("Ivan Ivanovich", "Fedoroff", "");

            ev1 = new GEDCOMIndividualEvent(tree, ind1, "BIRT", "");
            dtVal1 = ev1.Date;
            ind1.AddEvent(ev1);

            ev2 = new GEDCOMIndividualEvent(tree, ind2, "BIRT", "");
            dtVal2 = ev2.Date;
            ind2.AddEvent(ev2);

            float res;
            MatchParams mParams;
            mParams.NamesIndistinctThreshold = 1.0f;
            mParams.DatesCheck = true;
            mParams.YearsInaccuracy = 0;
            mParams.CheckEventPlaces = false;

            // null
            res = dtVal1.IsMatch(null, mParams);
            Assert.AreEqual(0.0f, res);

            // null
            res = ev1.IsMatch(null, mParams);
            Assert.AreEqual(0.0f, res);

            // dtVal1 -> dtVal2, delta = 0
            dtVal1.SetDateTime(DateTime.Parse("10.10.2013"));
            dtVal2.SetDateTime(DateTime.Parse("10.10.2013"));
            res = dtVal1.IsMatch(dtVal2, mParams);
            Assert.AreEqual(100.0f, res);

            // ev1 -> ev2, delta = 0
            res = ev1.IsMatch(ev2, mParams);
            Assert.AreEqual(100.0f, res);

            // dtVal1 -> dtVal2, delta = 3
            mParams.YearsInaccuracy = 3;

            dtVal2.SetDateTime(DateTime.Parse("10.10.2015"));
            res = dtVal1.IsMatch(dtVal2, mParams);
            Assert.AreEqual(100.0f, res);

            // ev1 -> ev2, delta = 3
            res = ev1.IsMatch(ev2, mParams);
            Assert.AreEqual(100.0f, res);

            dtVal2.SetDateTime(DateTime.Parse("10.10.2009"));
            res = dtVal1.IsMatch(dtVal2, mParams);
            Assert.AreEqual(0.0f, res);

            // ev1 -> ev2, delta = 3
            res = ev1.IsMatch(ev2, mParams);
            Assert.AreEqual(0.0f, res);

            // //

            res = ind1.IsMatch(null, mParams);
            Assert.AreEqual(0.0f, res);

            res = ind1.IsMatch(ind2, mParams);
            Assert.AreEqual(0.0f, res);

            // Ivanov - Ivanov(ich) : 3 chars of difference -> 0.88
            mParams.NamesIndistinctThreshold = 0.85f;
            mParams.YearsInaccuracy = 4;

            res = ind1.IsMatch(ind2, mParams);
            Assert.AreEqual(100.0f, res);
        }

        [Test]
        public void GEDCOMData_Tests()
        {
            using (GEDCOMData data = GEDCOMData.Create(null, null, "", "") as GEDCOMData) {
                Assert.IsNotNull(data);
                
                data.Agency = "test agency";
                Assert.AreEqual("test agency", data.Agency);
                
                GEDCOMTag evenTag = data.AddTag(GEDCOMTagType.EVEN, "", null);
                Assert.IsNotNull(evenTag);
                
                GEDCOMEvent evt = data.Events[0];
                Assert.AreEqual(evenTag, evt);
                
                data.Clear();
                Assert.IsTrue(data.IsEmpty());

                GEDCOMTree otherTree = new GEDCOMTree();
                data.ResetOwner(otherTree);
                Assert.AreEqual(otherTree, data.Owner);
            }
        }

        [Test]
        public void GEDCOMEvent_Tests()
        {
            using (GEDCOMEvent evt = GEDCOMEvent.Create(null, null, "", "") as GEDCOMEvent)
            {
                Assert.IsNotNull(evt);
                
                Assert.IsNotNull(evt.Date);
                
                Assert.IsNotNull(evt.Place);
            }
        }

        [Test]
        public void GEDCOMDateStatus_Tests()
        {
            using (GEDCOMDateStatus dateStatus = GEDCOMDateStatus.Create(null, null, "", "") as GEDCOMDateStatus)
            {
                Assert.IsNotNull(dateStatus);
                Assert.IsNotNull(dateStatus.ChangeDate);
            }
        }

        [Test]
        public void GEDCOMIndividualEvent_Tests()
        {
            using (GEDCOMIndividualEvent iEvent = GEDCOMIndividualEvent.Create(null, null, "", "") as GEDCOMIndividualEvent)
            {
                Assert.IsNotNull(iEvent);
                Assert.IsNotNull(iEvent.Family);
            }
        }

        [Test]
        public void GEDCOMIndividualOrdinance_Tests()
        {
            using (GEDCOMIndividualOrdinance iOrd = GEDCOMIndividualOrdinance.Create(null, null, "", "") as GEDCOMIndividualOrdinance)
            {
                Assert.IsNotNull(iOrd);

                Assert.IsNotNull(iOrd.Date);

                iOrd.TempleCode = "temple code";
                Assert.AreEqual("temple code", iOrd.TempleCode);

                iOrd.Place.StringValue = "test place";
                Assert.AreEqual("test place", iOrd.Place.StringValue);
                
                iOrd.BaptismDateStatus = GEDCOMBaptismDateStatus.bdsCompleted;
                Assert.AreEqual(GEDCOMBaptismDateStatus.bdsCompleted, iOrd.BaptismDateStatus);
                
                Assert.IsNotNull(iOrd.BaptismChangeDate);
                
                iOrd.EndowmentDateStatus = GEDCOMEndowmentDateStatus.edsExcluded;
                Assert.AreEqual(GEDCOMEndowmentDateStatus.edsExcluded, iOrd.EndowmentDateStatus);
                
                Assert.IsNotNull(iOrd.EndowmentChangeDate);
                
                Assert.IsNotNull(iOrd.Family);
                
                iOrd.ChildSealingDateStatus = GEDCOMChildSealingDateStatus.cdsPre1970;
                Assert.AreEqual(GEDCOMChildSealingDateStatus.cdsPre1970, iOrd.ChildSealingDateStatus);
                
                Assert.IsNotNull(iOrd.ChildSealingChangeDate);
                
                Assert.IsNotNull(iOrd.DateStatus);
            }
        }

        [Test]
        public void GEDCOMSpouseSealing_Tests()
        {
            using (GEDCOMSpouseSealing spouseSealing = GEDCOMSpouseSealing.Create(null, null, "", "") as GEDCOMSpouseSealing)
            {
                Assert.IsNotNull(spouseSealing);

                Assert.IsNotNull(spouseSealing.Date);

                spouseSealing.TempleCode = "temple code";
                Assert.AreEqual("temple code", spouseSealing.TempleCode);

                spouseSealing.Place.StringValue = "test place";
                Assert.AreEqual("test place", spouseSealing.Place.StringValue);

                spouseSealing.SpouseSealingDateStatus = GEDCOMSpouseSealingDateStatus.sdsCanceled;
                Assert.AreEqual(GEDCOMSpouseSealingDateStatus.sdsCanceled, spouseSealing.SpouseSealingDateStatus);

                Assert.IsNotNull(spouseSealing.SpouseSealingChangeDate);

                Assert.IsNotNull(spouseSealing.DateStatus);
            }
        }

        [Test]
        public void XRefReplacer_Tests()
        {
            using (XRefReplacer replacer = new XRefReplacer())
            {
                Assert.IsNotNull(replacer);

                GEDCOMIndividualRecord iRec = fContext.CreatePersonEx("ivan", "ivanovich", "ivanov", GEDCOMSex.svMale, false);
                replacer.AddXRef(iRec, "I210", iRec.XRef);

                string newXRef = replacer.FindNewXRef("I210");
                Assert.AreEqual(iRec.XRef, newXRef);

                newXRef = replacer.FindNewXRef("I310");
                Assert.AreEqual("I310", newXRef);

                for (int i = 0; i < replacer.Count; i++) {
                    XRefReplacer.XRefEntry xre = replacer[i];
                    Assert.AreEqual(iRec, xre.Rec);
                }
            }
        }

        [Test]
        public void UDN1_Tests()
        {
            UDN emptyUDN = UDN.CreateEmpty();
            Assert.IsTrue(emptyUDN.IsEmpty());

            // BIRT: "28 DEC 1990"
            GEDCOMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I1") as GEDCOMIndividualRecord;

            //Assert.AreEqual(EmptyUDN, GEDCOMUtils.GetUDN(null));
            //Assert.AreEqual(EmptyUDN, GEDCOMUtils.GetUDN("0102"));

            UDN testUDN = iRec.GetUDN("BIRT");
            Assert.AreEqual("1990/12/28", testUDN.ToString());

            testUDN = GEDCOMDate.GetUDNByFormattedStr("28/12/1990", GEDCOMCalendar.dcGregorian);
            Assert.AreEqual("1990/12/28", testUDN.ToString());

            using (GEDCOMDateValue dateVal = new GEDCOMDateValue(null, null, "", "")) {
                dateVal.ParseString("28 DEC 1990");
                testUDN = dateVal.GetUDN();
                Assert.AreEqual("1990/12/28", testUDN.ToString());

                dateVal.ParseString("ABT 20 JAN 2013");
                testUDN = dateVal.GetUDN();
                Assert.AreEqual("~2013/01/20", testUDN.ToString());

                dateVal.ParseString("CAL 20 JAN 2013");
                testUDN = dateVal.GetUDN();
                Assert.AreEqual("~2013/01/20", testUDN.ToString());

                dateVal.ParseString("EST 20 DEC 2013");
                testUDN = dateVal.GetUDN();
                Assert.AreEqual("~2013/12/20", testUDN.ToString());

                dateVal.ParseString("BET 04 JAN 2013 AND 25 JAN 2013");
                testUDN = dateVal.GetUDN();
                Assert.AreEqual("2013/01/14", testUDN.ToString());

                dateVal.ParseString("BEF 20 JAN 2013");
                testUDN = dateVal.GetUDN();
                Assert.AreEqual("<2013/01/20", testUDN.ToString());

                dateVal.ParseString("AFT 20 JAN 2013");
                testUDN = dateVal.GetUDN();
                Assert.AreEqual(">2013/01/20", testUDN.ToString());
            }
        }

        [Test]
        public void GEDCOMTagWithLists_Tests()
        {
            // GEDCOMTagWithLists protected class, derived - GEDCOMEventDetail
            using (GEDCOMPlace tag = GEDCOMPlace.Create(null, null, "", "") as GEDCOMPlace)
            {
                Assert.IsNotNull(tag);

                Assert.IsNotNull(tag.Notes);
                Assert.IsNotNull(tag.SourceCitations);
                Assert.IsNotNull(tag.MultimediaLinks);

                Assert.IsNull(tag.AddNote(null));
                Assert.IsNull(tag.AddSource(null, "page", 1));
                Assert.IsNull(tag.AddMultimedia(null));

                Assert.IsNotNull(tag.AddNote(new GEDCOMNoteRecord(null, null, "", "")));
                Assert.IsNotNull(tag.AddSource(new GEDCOMSourceRecord(null, null, "", ""), "page", 1));
                Assert.IsNotNull(tag.AddMultimedia(new GEDCOMMultimediaRecord(null, null, "", "")));
            }
        }

        [Test]
        public void GEDCOMChangeDate_Tests()
        {
            using (GEDCOMChangeDate cd = GEDCOMChangeDate.Create(null, null, GEDCOMTagType.CHAN, "") as GEDCOMChangeDate)
            {
                Assert.IsNotNull(cd);

                Assert.IsNotNull(cd.Notes);

                DateTime dtNow = DateTime.Now;
                dtNow = dtNow.AddTicks(-dtNow.Ticks % 10000000);
                cd.ChangeDateTime = dtNow;

                DateTime dtx = cd.ChangeDateTime;
                Assert.AreEqual(dtNow, dtx);

                GEDCOMTime time = cd.ChangeTime;
                Assert.AreEqual(dtNow.Second, time.Seconds);
                Assert.AreEqual(dtNow.Minute, time.Minutes);
                Assert.AreEqual(dtNow.Hour, time.Hour);
                Assert.AreEqual(dtNow.Millisecond, time.Fraction);

                time.Seconds = 11;
                Assert.AreEqual(11, time.Seconds);
                time.Minutes = 22;
                Assert.AreEqual(22, time.Minutes);
                time.Hour = 12;
                Assert.AreEqual(12, time.Hour);
                
                Assert.AreEqual("12:22:11", time.StringValue);
                
                Assert.AreEqual(DateTime.Now.Date.ToString("yyyy.MM.dd") + " 12:22:11", cd.ToString());

                Assert.IsFalse(time.IsEmpty());
                time.Clear();
                Assert.IsTrue(time.IsEmpty());
            }
        }

        [Test]
        public void GEDCOMTime_Tests()
        {
            using (GEDCOMTime time = new GEDCOMTime(null, null, "TIME", "20:20:20.100"))
            {
                Assert.IsNotNull(time, "time != null");

                Assert.AreEqual(20, time.Hour);
                Assert.AreEqual(20, time.Minutes);
                Assert.AreEqual(20, time.Seconds);
                Assert.AreEqual(100, time.Fraction);

                time.Fraction = 200;
                Assert.AreEqual(200, time.Fraction);

                Assert.AreEqual("20:20:20.200", time.StringValue);

                time.Hour = 0;
                time.Minutes = 0;
                time.Seconds = 0;
                Assert.AreEqual("", time.StringValue);
            }
        }

        /*[Test]
        public void GEDCOMTimePerf_Tests()
        {
            using (var dtx1 = new GEDCOMDate(null, null, "DATE", ""))
            {
                for (int k = 0; k < 100000; k++) {
                    string rest = dtx1.ParseString("01 FEB 1934/11B.C.");
                    Assert.IsNullOrEmpty(rest);
                    string dts = dtx1.StringValue;

                    rest = dtx1.ParseStringTok("01 FEB 1934/11B.C.");
                    Assert.IsNullOrEmpty(rest);
                    Assert.AreEqual(dts, dtx1.StringValue);
                    Assert.AreEqual("01 FEB 1934/11B.C.", dtx1.StringValue);


                    rest = dtx1.ParseString("ABT @#DJULIAN@ 01 APR 1934/11B.C.");
                    Assert.IsNullOrEmpty(rest);
                    dts = dtx1.StringValue;

                    rest = dtx1.ParseStringTok("ABT @#DJULIAN@ 01 APR 1934/11B.C.");
                    Assert.IsNullOrEmpty(rest);
                    Assert.AreEqual(dts, dtx1.StringValue);
                    Assert.AreEqual("ABT @#DJULIAN@ 01 APR 1934/11B.C.", dtx1.StringValue);


                    rest = dtx1.ParseString("@#DJULIAN@ 01 APR 1934/11B.C.");
                    Assert.IsNullOrEmpty(rest);
                    dts = dtx1.StringValue;

                    rest = dtx1.ParseStringTok("@#DJULIAN@ 01 APR 1934/11B.C.");
                    Assert.IsNullOrEmpty(rest);
                    Assert.AreEqual(dts, dtx1.StringValue);
                    Assert.AreEqual("@#DJULIAN@ 01 APR 1934/11B.C.", dtx1.StringValue);


                    rest = dtx1.ParseString("01 APR 1934/11B.C.");
                    Assert.IsNullOrEmpty(rest);
                    dts = dtx1.StringValue;

                    rest = dtx1.ParseStringTok("01 APR 1934/11B.C.");
                    Assert.IsNullOrEmpty(rest);
                    Assert.AreEqual(dts, dtx1.StringValue);
                    Assert.AreEqual("01 APR 1934/11B.C.", dtx1.StringValue);


                    rest = dtx1.ParseString("01 1934/11B.C.");
                    Assert.IsNullOrEmpty(rest);
                    dts = dtx1.StringValue;

                    rest = dtx1.ParseStringTok("01 1934/11B.C.");
                    Assert.IsNullOrEmpty(rest);
                    Assert.AreEqual(dts, dtx1.StringValue);
                    Assert.AreEqual("01 1934/11B.C.", dtx1.StringValue);


                    rest = dtx1.ParseString("AUG 1934/11");
                    Assert.IsNullOrEmpty(rest);
                    dts = dtx1.StringValue;

                    rest = dtx1.ParseStringTok("AUG 1934/11");
                    Assert.IsNullOrEmpty(rest);
                    Assert.AreEqual(dts, dtx1.StringValue);
                    Assert.AreEqual("AUG 1934/11", dtx1.StringValue);
                }
            }
        }*/

        [Test]
        public void GEDCOMDate_Tests()
        {
            using (GEDCOMDate dtx1 = new GEDCOMDate(null, null, "DATE", "20 JAN 2013"))
            {
                Assert.IsNotNull(dtx1, "dtx1 != null");

                DateTime dt = ParseDT("20.01.2013");
                Assert.IsTrue(dtx1.Date.Equals(dt), "dtx1.DateTime.Equals(dt)");

                //dtx1.DateCalendar = GEDCOMCalendar.dcFrench;
                Assert.AreEqual(GEDCOMCalendar.dcGregorian, dtx1.DateCalendar);

                dtx1.Day = 21;
                Assert.AreEqual(21, dtx1.Day);

                dtx1.Month = "SEP";
                Assert.AreEqual("SEP", dtx1.Month);

                dtx1.Year = 1812;
                Assert.AreEqual(1812, dtx1.Year);

                dtx1.YearBC = true;
                Assert.AreEqual(true, dtx1.YearBC);

                dtx1.YearModifier = "2";
                Assert.AreEqual("2", dtx1.YearModifier);

                //
                dtx1.ParseString("01 FEB 1934/11B.C.");
                Assert.AreEqual(01, dtx1.Day);
                Assert.AreEqual("FEB", dtx1.Month);
                Assert.AreEqual(1934, dtx1.Year);
                Assert.AreEqual("11", dtx1.YearModifier);
                Assert.AreEqual(true, dtx1.YearBC);
                dtx1.ParseString("01 FEB 1934/11B.C.");
                Assert.AreEqual("01 FEB 1934/11B.C.", dtx1.StringValue);

                // gregorian

                dtx1.SetGregorian(1, 1, 1980);
                Assert.AreEqual(GEDCOMCalendar.dcGregorian, dtx1.DateCalendar);
                Assert.AreEqual("01 JAN 1980", dtx1.StringValue);

                Assert.Throws(typeof(GEDCOMDateException), () => { dtx1.SetGregorian(1, "X", 1980, "", false); });

                // julian

                dtx1.SetJulian(1, "JAN", 1980, false);
                Assert.AreEqual(GEDCOMCalendar.dcJulian, dtx1.DateCalendar);

                dtx1.SetJulian(1, 3, 1980);
                Assert.AreEqual(GEDCOMCalendar.dcJulian, dtx1.DateCalendar);
                Assert.AreEqual("@#DJULIAN@ 01 MAR 1980", dtx1.StringValue);
                dtx1.ParseString("@#DJULIAN@ 01 MAR 1980");
                Assert.AreEqual("@#DJULIAN@ 01 MAR 1980", dtx1.StringValue);

                using (GEDCOMDate dtx2 = new GEDCOMDate(null, null, "DATE", ""))
                {
                    Assert.IsNotNull(dtx2, "dtx2 != null");

                    dtx2.Assign(null);
                    Assert.AreEqual("", dtx2.StringValue);
                    Assert.AreEqual(new DateTime(0), dtx2.GetDateTime());

                    Assert.IsFalse(dtx2.IsValidDate());

                    dtx2.Assign(dtx1);
                    Assert.AreEqual("@#DJULIAN@ 01 MAR 1980", dtx2.StringValue);

                    Assert.IsTrue(dtx2.IsValidDate());
                }

                // hebrew

                dtx1.SetHebrew(1, "TSH", 1980, false);
                Assert.AreEqual(GEDCOMCalendar.dcHebrew, dtx1.DateCalendar);

                dtx1.SetHebrew(1, 2, 1980);
                Assert.AreEqual(GEDCOMCalendar.dcHebrew, dtx1.DateCalendar);
                Assert.AreEqual("@#DHEBREW@ 01 CSH 1980", dtx1.StringValue);
                dtx1.ParseString("@#DHEBREW@ 01 CSH 1980");
                Assert.AreEqual("@#DHEBREW@ 01 CSH 1980", dtx1.StringValue);

                Assert.Throws(typeof(GEDCOMDateException), () => { dtx1.SetHebrew(1, "X", 1980, false); });

                // french

                dtx1.SetFrench(1, "VEND", 1980, false);
                Assert.AreEqual(GEDCOMCalendar.dcFrench, dtx1.DateCalendar);

                dtx1.SetFrench(1, 2, 1980);
                Assert.AreEqual(GEDCOMCalendar.dcFrench, dtx1.DateCalendar);
                Assert.AreEqual("@#DFRENCH R@ 01 BRUM 1980", dtx1.StringValue);
                dtx1.ParseString("@#DFRENCH R@ 01 BRUM 1980");
                Assert.AreEqual("@#DFRENCH R@ 01 BRUM 1980", dtx1.StringValue);

                Assert.Throws(typeof(GEDCOMDateException), () => { dtx1.SetFrench(1, "X", 1980, false); });

                // roman

                dtx1.SetRoman(1, "JAN", 1980, false);
                Assert.AreEqual(GEDCOMCalendar.dcRoman, dtx1.DateCalendar);

                dtx1.SetUnknown(1, "JAN", 1980, false);
                Assert.AreEqual(GEDCOMCalendar.dcUnknown, dtx1.DateCalendar);
            }
        }

        [Test]
        public void GEDCOMDateRange_Tests()
        {
            using (var dtx1 = (GEDCOMDateRange)GEDCOMDateRange.Create(null, null, "DATE", ""))
            {
                Assert.IsNotNull(dtx1, "dtx1 != null");
                Assert.AreEqual("", dtx1.StringValue);
                Assert.AreEqual(new DateTime(0), dtx1.GetDateTime());
                Assert.AreEqual("", dtx1.GetDisplayStringExt(DateFormat.dfYYYY_MM_DD, true, true)); // date is empty
                UDN udn = dtx1.GetUDN();
                Assert.IsTrue(udn.IsEmpty());

                dtx1.ParseString("BET 04 JAN 2013 AND 25 JAN 2013");
                Assert.AreEqual("BET 04 JAN 2013 AND 25 JAN 2013", dtx1.StringValue);
                Assert.AreEqual(new DateTime(0), dtx1.Date);
                Assert.AreEqual("2013.01.04 [G] - 2013.01.25 [G]", dtx1.GetDisplayStringExt(DateFormat.dfYYYY_MM_DD, true, true));
                udn = dtx1.GetUDN();
                Assert.IsFalse(udn.IsEmpty());

                dtx1.ParseString("BEF 20 JAN 2013");
                Assert.AreEqual("BEF 20 JAN 2013", dtx1.StringValue);
                Assert.AreEqual(ParseDT("20.01.2013"), dtx1.Date);
                Assert.AreEqual("< 2013.01.20 [G]", dtx1.GetDisplayStringExt(DateFormat.dfYYYY_MM_DD, true, true));
                udn = dtx1.GetUDN();
                Assert.IsFalse(udn.IsEmpty());

                dtx1.ParseString("AFT 20 JAN 2013");
                Assert.AreEqual("AFT 20 JAN 2013", dtx1.StringValue);
                Assert.AreEqual(ParseDT("20.01.2013"), dtx1.Date);
                Assert.AreEqual("2013.01.20 [G] >", dtx1.GetDisplayStringExt(DateFormat.dfYYYY_MM_DD, true, true));
                udn = dtx1.GetUDN();
                Assert.IsFalse(udn.IsEmpty());

                Assert.Throws(typeof(NotSupportedException), () => { dtx1.SetDateTime(DateTime.Now); });
            }
        }

        [Test]
        public void GEDCOMDatePeriod_Tests()
        {
            using (GEDCOMDatePeriod dtx1 = new GEDCOMDatePeriod(null, null, "DATE", ""))
            {
                Assert.IsNotNull(dtx1, "dtx1 != null");
                Assert.AreEqual("", dtx1.StringValue);
                Assert.AreEqual(new DateTime(0), dtx1.GetDateTime());

                Assert.AreEqual("", dtx1.GetDisplayStringExt(DateFormat.dfYYYY_MM_DD, true, true)); // date is empty
                UDN udn = dtx1.GetUDN();
                Assert.IsTrue(udn.IsEmpty());

                dtx1.ParseString("FROM 04 JAN 2013 TO 23 JAN 2013");

                dtx1.ResetOwner(fContext.Tree);
                Assert.AreEqual(fContext.Tree, dtx1.Owner);

                Assert.AreEqual("FROM 04 JAN 2013 TO 23 JAN 2013", dtx1.StringValue);
                Assert.AreEqual(new DateTime(0), dtx1.Date);
                Assert.AreEqual("2013.01.04 [G] - 2013.01.23 [G]", dtx1.GetDisplayStringExt(DateFormat.dfYYYY_MM_DD, true, true));
                udn = dtx1.GetUDN();
                Assert.IsFalse(udn.IsEmpty());

                dtx1.ParseString("FROM 04 JAN 2013 TO 04 JAN 2013");
                Assert.AreEqual("FROM 04 JAN 2013 TO 04 JAN 2013", dtx1.StringValue);
                Assert.AreEqual(ParseDT("04.01.2013"), dtx1.Date);
                udn = dtx1.GetUDN();
                Assert.IsFalse(udn.IsEmpty());

                dtx1.Clear();
                dtx1.ParseString("FROM 04 JAN 2013");
                Assert.AreEqual("FROM 04 JAN 2013", dtx1.StringValue);
                Assert.AreEqual(ParseDT("04.01.2013"), dtx1.Date);
                Assert.AreEqual("2013.01.04 [G] >", dtx1.GetDisplayStringExt(DateFormat.dfYYYY_MM_DD, true, true));
                udn = dtx1.GetUDN();
                Assert.IsFalse(udn.IsEmpty());

                dtx1.Clear();
                dtx1.ParseString("TO 23 JAN 2013");
                Assert.AreEqual("TO 23 JAN 2013", dtx1.StringValue);
                Assert.AreEqual(ParseDT("23.01.2013"), dtx1.Date);
                Assert.AreEqual("< 2013.01.23 [G]", dtx1.GetDisplayStringExt(DateFormat.dfYYYY_MM_DD, true, true));
                udn = dtx1.GetUDN();
                Assert.IsFalse(udn.IsEmpty());

                Assert.Throws(typeof(NotSupportedException), () => { dtx1.SetDateTime(DateTime.Now); });
            }
        }

        [Test]
        public void GEDCOMDateValue_Tests()
        {
            // check empty dateval match
            using (GEDCOMDateValue dtx1 = new GEDCOMDateValue(null, null, "DATE", ""))
            {
                Assert.IsNotNull(dtx1, "dtx1 != null");

                using (GEDCOMDateValue dtx2 = new GEDCOMDateValue(null, null, "DATE", ""))
                {
                    Assert.IsNotNull(dtx2, "dtx1 != null");

                    Assert.AreEqual(0.0f, dtx1.IsMatch(dtx2, new MatchParams()));
                }
            }

            using (GEDCOMDateValue dtx1 = new GEDCOMDateValue(null, null, "DATE", ""))
            {
                Assert.IsNotNull(dtx1, "dtx1 != null");
                Assert.AreEqual("", dtx1.GetDisplayStringExt(DateFormat.dfYYYY_MM_DD, true, true)); // value is empty

                dtx1.ParseString("20 JAN 2013");
                Assert.AreEqual("2013.01.20 [G]", dtx1.GetDisplayStringExt(DateFormat.dfYYYY_MM_DD, true, true));
            }

            using (GEDCOMDateValue dtx1 = new GEDCOMDateValue(null, null, "DATE", "20 JAN 2013"))
            {
                Assert.IsNotNull(dtx1, "dtx1 != null");

                DateTime dt = ParseDT("20.01.2013");
                Assert.IsTrue(dtx1.Date.Equals(dt), "dtx1.DateTime.Equals(dt)");

                dtx1.ParseString("1716/"); // potentially incorrect value
                Assert.AreEqual("1716", dtx1.StringValue);

                dtx1.ParseString("1716/1717");
                Assert.AreEqual("1716/1717", dtx1.StringValue);

                dtx1.ParseString("1716/20");
                Assert.AreEqual("1716/20", dtx1.StringValue);

                dtx1.ParseString("3 MAY 1835/1838");
                Assert.AreEqual("03 MAY 1835/1838", dtx1.StringValue);

                dtx1.ParseString("ABT 1844/1845");
                Assert.AreEqual("ABT 1844/1845", dtx1.StringValue);

                dtx1.ParseString("FEB 1746/1747");
                Assert.AreEqual("FEB 1746/1747", dtx1.StringValue);

                dtx1.ParseString("INT 20 JAN 2013 (today)");
                Assert.IsTrue(dtx1.Date.Equals(dt), "dtx1.DateTime.Equals(dt)");
                Assert.AreEqual("today", (dtx1.Value as GEDCOMDateInterpreted).DatePhrase);

                (dtx1.Value as GEDCOMDateInterpreted).DatePhrase = "now";
                Assert.AreEqual("INT 20 JAN 2013 (now)", dtx1.StringValue);

                (dtx1.Value as GEDCOMDateInterpreted).DatePhrase = "(yesterday)";
                Assert.AreEqual("INT 20 JAN 2013 (yesterday)", dtx1.StringValue);

                dtx1.ParseString("INT 20 JAN 2013 (yesterday)");
                Assert.AreEqual("INT 20 JAN 2013 (yesterday)", dtx1.StringValue);

                string st;

                st = "ABT 20 JAN 2013";
                dtx1.ParseString(st);
                Assert.IsTrue(dtx1.Date.Equals(dt));
                Assert.AreEqual(st, dtx1.StringValue);
                Assert.AreEqual(GEDCOMApproximated.daAbout, ((GEDCOMDate)dtx1.Value).Approximated);
                
                st = "CAL 20 JAN 2013";
                dtx1.ParseString(st);
                Assert.AreEqual(dtx1.Date, dt);
                Assert.AreEqual(st, dtx1.StringValue);
                Assert.AreEqual(GEDCOMApproximated.daCalculated, ((GEDCOMDate)dtx1.Value).Approximated);
                
                st = "EST 20 DEC 2013";
                dtx1.ParseString(st);
                Assert.AreEqual(dtx1.Date, ParseDT("20.12.2013"));
                Assert.AreEqual(st, dtx1.StringValue);
                Assert.AreEqual(GEDCOMApproximated.daEstimated, ((GEDCOMDate)dtx1.Value).Approximated);

                ((GEDCOMDate)dtx1.Value).Approximated = GEDCOMApproximated.daCalculated;
                Assert.AreEqual("CAL 20 DEC 2013", dtx1.StringValue);

                ((GEDCOMDate)dtx1.Value).Approximated = GEDCOMApproximated.daExact;
                Assert.AreEqual("20 DEC 2013", dtx1.StringValue);

                using (GEDCOMDateValue dtx2 = new GEDCOMDateValue(null, null, "DATE", "19 JAN 2013"))
                {
                    int res = dtx1.CompareTo(dtx2);
                    Assert.AreEqual(1, res);
                }
                
                int res1 = dtx1.CompareTo(null);
                Assert.AreEqual(-1, res1);

                //
                
                dtx1.ParseString("FROM 04 JAN 2013 TO 23 JAN 2013");
                Assert.IsFalse(dtx1.IsEmpty());
                Assert.AreEqual("FROM 04 JAN 2013 TO 23 JAN 2013", dtx1.StringValue);
                Assert.AreEqual("04 JAN 2013", (dtx1.Value as GEDCOMDatePeriod).DateFrom.StringValue);
                Assert.AreEqual("23 JAN 2013", (dtx1.Value as GEDCOMDatePeriod).DateTo.StringValue);
                dtx1.Clear();
                Assert.IsTrue(dtx1.IsEmpty());

                dtx1.ParseString("BEF 20 JAN 2013");
                Assert.IsFalse(dtx1.IsEmpty());
                Assert.AreEqual(ParseDT("20.01.2013"), dtx1.Date);
                Assert.AreEqual("BEF 20 JAN 2013", dtx1.StringValue);

                dtx1.ParseString("AFT 20 JAN 2013");
                Assert.IsFalse(dtx1.IsEmpty());
                Assert.AreEqual(ParseDT("20.01.2013"), dtx1.Date);
                Assert.AreEqual("AFT 20 JAN 2013", dtx1.StringValue);

                dtx1.ParseString("BET 04 JAN 2013 AND 25 JAN 2013");
                Assert.IsFalse(dtx1.IsEmpty());
                Assert.AreEqual("BET 04 JAN 2013 AND 25 JAN 2013", dtx1.StringValue);
                Assert.AreEqual("04 JAN 2013", (dtx1.Value as GEDCOMDateRange).After.StringValue);
                Assert.AreEqual("25 JAN 2013", (dtx1.Value as GEDCOMDateRange).Before.StringValue);
                dtx1.Clear();
                Assert.IsTrue(dtx1.IsEmpty());

                GEDCOMTree otherTree = new GEDCOMTree();
                dtx1.ResetOwner(otherTree);
                Assert.AreEqual(otherTree, dtx1.Owner);
            }
        }

        [Test]
        public void GEDCOMAddress_Tests()
        {
            using (GEDCOMAddress addr = GEDCOMAddress.Create(null, null, GEDCOMTagType.ADDR, "") as GEDCOMAddress)
            {
                Assert.IsNotNull(addr, "addr != null");

                addr.SetAddressText("test");
                Assert.AreEqual("test", addr.Address.Text.Trim());

                addr.Address = new StringList("This\r\naddress\r\ntest");
                Assert.AreEqual("This\r\naddress\r\ntest", addr.Address.Text.Trim());
                Assert.AreEqual("This", addr.Address[0]);
                Assert.AreEqual("address", addr.Address[1]);
                Assert.AreEqual("test", addr.Address[2]);

                addr.AddTag("PHON", "8 911 101 99 99", null);
                Assert.AreEqual("8 911 101 99 99", addr.PhoneNumbers[0].StringValue);

                addr.AddTag("EMAIL", "test@mail.com", null);
                Assert.AreEqual("test@mail.com", addr.EmailAddresses[0].StringValue);

                addr.AddTag("FAX", "abrakadabra", null);
                Assert.AreEqual("abrakadabra", addr.FaxNumbers[0].StringValue);

                addr.AddTag("WWW", "http://test.com", null);
                Assert.AreEqual("http://test.com", addr.WebPages[0].StringValue);

                // stream test
                string buf = TagStreamTest(addr);
                Assert.AreEqual(buf, "0 ADDR This\r\n"+"1 CONT address\r\n"+"1 CONT test\r\n"
                                +"0 PHON 8 911 101 99 99\r\n"
                                +"0 EMAIL test@mail.com\r\n"
                                +"0 FAX abrakadabra\r\n"
                                +"0 WWW http://test.com\r\n");

                addr.AddPhoneNumber("8 911 101 33 33");
                Assert.AreEqual("8 911 101 33 33", addr.PhoneNumbers[1].StringValue);

                addr.AddEmailAddress("test@mail.ru");
                Assert.AreEqual("test@mail.ru", addr.EmailAddresses[1].StringValue);

                addr.AddFaxNumber("abrakadabra");
                Assert.AreEqual("abrakadabra", addr.FaxNumbers[1].StringValue);

                addr.AddWebPage("http://test.ru");
                Assert.AreEqual("http://test.ru", addr.WebPages[1].StringValue);

                //

                addr.AddressLine1 = "test1";
                Assert.AreEqual("test1", addr.AddressLine1);

                addr.AddressLine2 = "test2";
                Assert.AreEqual("test2", addr.AddressLine2);

                addr.AddressLine3 = "test3";
                Assert.AreEqual("test3", addr.AddressLine3);

                addr.AddressCity = "test4";
                Assert.AreEqual("test4", addr.AddressCity);

                addr.AddressState = "test5";
                Assert.AreEqual("test5", addr.AddressState);

                addr.AddressCountry = "test6";
                Assert.AreEqual("test6", addr.AddressCountry);

                addr.AddressPostalCode = "test7";
                Assert.AreEqual("test7", addr.AddressPostalCode);

                using (GEDCOMAddress addr2 = GEDCOMAddress.Create(null, null, GEDCOMTagType.ADDR, "") as GEDCOMAddress)
                {
                    Assert.Throws(typeof(ArgumentException), () => { addr2.Assign(null); });

                    addr2.Assign(addr);

                    Assert.AreEqual("This\r\naddress\r\ntest", addr2.Address.Text.Trim());
                    Assert.AreEqual("8 911 101 99 99", addr2.PhoneNumbers[0].StringValue);
                    Assert.AreEqual("test@mail.com", addr2.EmailAddresses[0].StringValue);
                    Assert.AreEqual("abrakadabra", addr2.FaxNumbers[0].StringValue);
                    Assert.AreEqual("http://test.com", addr2.WebPages[0].StringValue);
                    Assert.AreEqual("8 911 101 33 33", addr2.PhoneNumbers[1].StringValue);
                    Assert.AreEqual("test@mail.ru", addr2.EmailAddresses[1].StringValue);
                    Assert.AreEqual("abrakadabra", addr2.FaxNumbers[1].StringValue);
                    Assert.AreEqual("http://test.ru", addr2.WebPages[1].StringValue);
                    Assert.AreEqual("test1", addr2.AddressLine1);
                    Assert.AreEqual("test2", addr2.AddressLine2);
                    Assert.AreEqual("test3", addr2.AddressLine3);
                    Assert.AreEqual("test4", addr2.AddressCity);
                    Assert.AreEqual("test5", addr2.AddressState);
                    Assert.AreEqual("test6", addr2.AddressCountry);
                    Assert.AreEqual("test7", addr2.AddressPostalCode);
                }

                addr.SetAddressArray(new string[] {"test11", "test21", "test31"});
                Assert.AreEqual("test11", addr.Address[0]);
                Assert.AreEqual("test21", addr.Address[1]);
                Assert.AreEqual("test31", addr.Address[2]);

                Assert.IsFalse(addr.IsEmpty());
                addr.Clear();
                Assert.IsTrue(addr.IsEmpty());

                GEDCOMTree otherTree = new GEDCOMTree();
                addr.ResetOwner(otherTree);
                Assert.AreEqual(otherTree, addr.Owner);
            }
        }

        [Test]
        public void GEDCOMAlias_Tests()
        {
            using (GEDCOMAlias alias = GEDCOMAlias.Create(null, null, GEDCOMTagType.ALIA, "") as GEDCOMAlias)
            {
                Assert.IsNotNull(alias, "alias != null");
            }
        }

        [Test]
        public void GEDCOMAssociation_Tests()
        {
            using (GEDCOMAssociation association = GEDCOMAssociation.Create(null, null, GEDCOMTagType.ASSO, "") as GEDCOMAssociation) {
                Assert.IsNotNull(association);

                Assert.IsNotNull(association.SourceCitations);
                
                Assert.IsNotNull(association.Notes); // for GEDCOMPointerWithNotes
                
                association.Relation = "This is test relation";
                Assert.AreEqual("This is test relation", association.Relation);

                association.Individual = null;
                Assert.IsNull(association.Individual);

                GEDCOMTag tag = association.AddTag(GEDCOMTagType.SOUR, "xxx", null);
                Assert.IsNotNull(tag);
                Assert.IsTrue(tag is GEDCOMSourceCitation);

                Assert.IsFalse(association.IsEmpty());
                association.Clear();
                Assert.IsTrue(association.IsEmpty());

                GEDCOMTree otherTree = new GEDCOMTree();
                association.ResetOwner(otherTree);
                Assert.AreEqual(otherTree, association.Owner);
            }
        }

        [Test]
        public void GEDCOMUserRef_Tests()
        {
            using (GEDCOMUserReference userRef = GEDCOMUserReference.Create(null, null, GEDCOMTagType.REFN, "") as GEDCOMUserReference)
            {
                Assert.IsNotNull(userRef);

                userRef.ReferenceType = "test";
                Assert.AreEqual("test", userRef.ReferenceType);
            }
        }

        private void OnTreeChange(object sender, EventArgs e) {}
        private void OnTreeChanging(object sender, EventArgs e) {}
        private void OnTreeProgress(object sender, int progress) {}

        [Test]
        public void GEDCOMTree_Tests()
        {
            GEDCOMTree tree = new GEDCOMTree();
            Assert.IsNotNull(tree);


            // Tests of event handlers
            tree.OnChange += OnTreeChange;
            //Assert.AreEqual(OnTreeChange, tree.OnChange);
            tree.OnChange -= OnTreeChange;
            //Assert.AreEqual(null, tree.OnChange);
            tree.OnChanging += OnTreeChanging;
            //Assert.AreEqual(OnTreeChanging, tree.OnChanging);
            tree.OnChanging -= OnTreeChanging;
            //Assert.AreEqual(null, tree.OnChanging);
            tree.OnProgress += OnTreeProgress;
            //Assert.AreEqual(OnTreeProgress, tree.OnProgress);
            tree.OnProgress -= OnTreeProgress;
            //Assert.AreEqual(null, tree.OnProgress);


            //

            Assert.IsNotNull(tree.GetSubmitter());

            GEDCOMRecord rec;

            GEDCOMIndividualRecord iRec = tree.CreateIndividual();
            Assert.IsNotNull(iRec, "CreateIndividual() != null");

            string xref = iRec.XRef;
            rec = tree.XRefIndex_Find(xref);
            Assert.IsNotNull(rec);
            Assert.AreEqual(xref, rec.XRef);

            string uid = iRec.UID;
            rec = tree.FindUID(uid);
            Assert.IsNotNull(rec);
            Assert.AreEqual(uid, rec.UID);
            Assert.IsNull(tree.FindUID(""));

            //
            GEDCOMFamilyRecord famRec = tree.CreateFamily();
            Assert.IsNotNull(famRec, "CreateFamily() != null");
            GEDCOMFamilyRecordTest(famRec, iRec);

            //
            GEDCOMNoteRecord noteRec = tree.CreateNote();
            Assert.IsNotNull(noteRec, "CreateNote() != null");
            GEDCOMNoteRecordTest(noteRec, iRec);

            //
            GEDCOMRepositoryRecord repRec = tree.CreateRepository();
            Assert.IsNotNull(repRec, "CreateRepository() != null");

            //
            GEDCOMSourceRecord srcRec = tree.CreateSource();
            Assert.IsNotNull(srcRec, "CreateSource() != null");
            GEDCOMSourceRecordTest(srcRec, iRec, repRec);

            //
            GEDCOMMultimediaRecord mmRec = tree.CreateMultimedia();
            Assert.IsNotNull(mmRec, "CreateMultimedia() != null");
            GEDCOMMultimediaRecordTest(mmRec, iRec);
            
            //

            GEDCOMRecord sbmrRec = tree.AddRecord(GEDCOMSubmitterRecord.Create(tree, tree, "", "") as GEDCOMRecord);
            Assert.IsNotNull(sbmrRec, "sbmrRec != null");
            sbmrRec.InitNew();
            string submXRef = sbmrRec.XRef;

            //

            GEDCOMSubmissionRecord submRec = tree.AddRecord(GEDCOMSubmissionRecord.Create(tree, tree, "", "") as GEDCOMRecord) as GEDCOMSubmissionRecord;
            Assert.IsNotNull(submRec, "rec1 != null");
            submRec.InitNew();
            GEDCOMSubmissionRecordTest(submRec, submXRef);

            //
            GEDCOMGroupRecord groupRec = tree.CreateGroup();
            Assert.IsNotNull(groupRec, "CreateGroup() != null");

            //
            GEDCOMTaskRecord taskRec = tree.CreateTask();
            Assert.IsNotNull(taskRec, "CreateTask() != null");

            //
            GEDCOMCommunicationRecord commRec = tree.CreateCommunication();
            Assert.IsNotNull(commRec, "CreateCommunication() != null");

            //
            GEDCOMResearchRecord resRec = tree.CreateResearch();
            Assert.IsNotNull(resRec, "CreateResearch() != null");
            GEDCOMResearchRecordTest(resRec, commRec, taskRec, groupRec);

            //
            GEDCOMLocationRecord locRec = tree.CreateLocation();
            Assert.IsNotNull(locRec, "CreateLocation() != null");


            tree.Pack();


            int size = 0;
            var enum1 = tree.GetEnumerator(GEDCOMRecordType.rtNone);
            GEDCOMRecord rec1;
            while (enum1.MoveNext(out rec1)) {
                size++;
            }
            Assert.AreEqual(14, size);

            for (int i = 0; i < tree.RecordsCount; i++) {
                GEDCOMRecord rec2 = tree[i];
                Assert.IsNotNull(rec2);

                string xref2 = rec2.XRef;
                GEDCOMRecord rec3 = tree.XRefIndex_Find(xref2);
                Assert.IsNotNull(rec3);
                Assert.AreEqual(rec2, rec3);

                /*string uid = rec2.UID;
				GEDCOMRecord rec4 = tree.FindUID(uid);
				Assert.IsNotNull(rec4);
				Assert.AreEqual(rec2, rec4);*/

                int idx = tree.IndexOf(rec2);
                Assert.AreEqual(i, idx);
            }
            
            Assert.IsFalse(tree.IsEmpty);

            Assert.IsFalse(tree.DeleteFamilyRecord(null));
            Assert.IsTrue(tree.DeleteFamilyRecord(famRec));

            Assert.IsFalse(tree.DeleteNoteRecord(null));
            Assert.IsTrue(tree.DeleteNoteRecord(noteRec));

            Assert.IsFalse(tree.DeleteSourceRecord(null));
            Assert.IsTrue(tree.DeleteSourceRecord(srcRec));

            Assert.IsFalse(tree.DeleteGroupRecord(null));
            Assert.IsTrue(tree.DeleteGroupRecord(groupRec));

            Assert.IsFalse(tree.DeleteLocationRecord(null));
            Assert.IsTrue(tree.DeleteLocationRecord(locRec));

            Assert.IsFalse(tree.DeleteResearchRecord(null));
            Assert.IsTrue(tree.DeleteResearchRecord(resRec));

            Assert.IsFalse(tree.DeleteCommunicationRecord(null));
            Assert.IsTrue(tree.DeleteCommunicationRecord(commRec));

            Assert.IsFalse(tree.DeleteTaskRecord(null));
            Assert.IsTrue(tree.DeleteTaskRecord(taskRec));

            Assert.IsFalse(tree.DeleteMediaRecord(null));
            Assert.IsTrue(tree.DeleteMediaRecord(mmRec));

            Assert.IsFalse(tree.DeleteIndividualRecord(null));
            Assert.IsTrue(tree.DeleteIndividualRecord(iRec));

            Assert.IsFalse(tree.DeleteRepositoryRecord(null));
            Assert.IsTrue(tree.DeleteRepositoryRecord(repRec));

            tree.Clear();
            Assert.AreEqual(0, tree.RecordsCount);
            Assert.IsTrue(tree.IsEmpty);

            tree.State = GEDCOMState.osReady;
            Assert.AreEqual(GEDCOMState.osReady, tree.State);


            // Tests of GEDCOMTree.Extract()
            using (GEDCOMTree tree2 = new GEDCOMTree()) {
                GEDCOMIndividualRecord iRec2 = tree.AddRecord(GEDCOMIndividualRecord.Create(tree2, tree2, "", "") as GEDCOMRecord) as GEDCOMIndividualRecord;
                Assert.IsNotNull(iRec2);
                iRec2.InitNew();

                tree2.AddRecord(iRec2);
                int rIdx = tree2.IndexOf(iRec2);
                Assert.IsTrue(rIdx >= 0);
                GEDCOMRecord extractedRec = tree2.Extract(rIdx);
                Assert.AreEqual(iRec2, extractedRec);
                Assert.IsTrue(tree2.IndexOf(iRec2) < 0);
            }
        }

        [Test]
        public void GEDCOMHeader_Tests()
        {
            GEDCOMHeader headRec = fContext.Tree.Header;

            headRec.Notes = new StringList("This notes test");
            Assert.AreEqual("This notes test", headRec.Notes[0]);

            headRec.CharacterSet = GEDCOMCharacterSet.csASCII;
            Assert.AreEqual(GEDCOMCharacterSet.csASCII, headRec.CharacterSet);

            headRec.CharacterSetVersion = "1x";
            Assert.AreEqual("1x", headRec.CharacterSetVersion);

            headRec.Copyright = "copyright";
            Assert.AreEqual("copyright", headRec.Copyright);

            headRec.Source = "GEDKeeper";
            Assert.AreEqual("GEDKeeper", headRec.Source);

            headRec.ReceivingSystemName = "GEDKeeper";
            Assert.AreEqual("GEDKeeper", headRec.ReceivingSystemName);

            headRec.Language.Value = GEDCOMLanguageID.Russian;
            Assert.AreEqual("Russian", headRec.Language.StringValue);

            headRec.GEDCOMVersion = "5.5";
            Assert.AreEqual("5.5", headRec.GEDCOMVersion);

            headRec.GEDCOMForm = "LINEAGE-LINKED";
            Assert.AreEqual("LINEAGE-LINKED", headRec.GEDCOMForm);

            headRec.FileName = "testfile.ged";
            Assert.AreEqual("testfile.ged", headRec.FileName);

            DateTime dtx = DateTime.Now;
            dtx = dtx.AddTicks(-dtx.Ticks % 10000000);
            headRec.TransmissionDateTime = dtx;
            Assert.AreEqual(dtx, headRec.TransmissionDateTime);

            headRec.FileRevision = 113;
            Assert.AreEqual(113, headRec.FileRevision);

            headRec.PlaceHierarchy = "test11";
            Assert.AreEqual("test11", headRec.PlaceHierarchy);

            Assert.IsNotNull(headRec.SourceBusinessAddress);

            headRec.SourceBusinessName = "test23";
            Assert.AreEqual("test23", headRec.SourceBusinessName);

            headRec.SourceProductName = "test33";
            Assert.AreEqual("test33", headRec.SourceProductName);

            headRec.SourceVersion = "test44";
            Assert.AreEqual("test44", headRec.SourceVersion);

            Assert.IsNotNull(headRec.Submission);

            Assert.IsFalse(headRec.IsEmpty());
            headRec.Clear();
            Assert.IsTrue(headRec.IsEmpty());
        }

        [Test]
        public void GEDCOMMap_Tests()
        {
            using (GEDCOMMap map = GEDCOMMap.Create(null, null, "", "") as GEDCOMMap) {
                map.Lati = 5.11111;
                Assert.AreEqual(5.11111, map.Lati);

                map.Long = 7.99999;
                Assert.AreEqual(7.99999, map.Long);
            }
        }

        [Test]
        public void GEDCOMAux_Tests()
        {
            GEDCOMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I1") as GEDCOMIndividualRecord;
            GEDCOMCustomEvent evt, evtd;

            evt = iRec.FindEvent("BIRT");
            Assert.IsNotNull(evt);

            evtd = iRec.FindEvent("DEAT");
            Assert.IsNotNull(evtd);

            GEDCOMCustomEventTest(evt, "28.12.1990");
            Assert.IsNotNull(evt.Address);
        }

        [Test]
        public void GEDCOMIndividualRecord_Tests()
        {
            GEDCOMIndividualRecord ind3 = fContext.Tree.XRefIndex_Find("I3") as GEDCOMIndividualRecord;
            Assert.IsNotNull(ind3.GetParentsFamily());

            GEDCOMIndividualRecord ind2 = fContext.Tree.XRefIndex_Find("I2") as GEDCOMIndividualRecord;
            Assert.IsNotNull(ind2.GetMarriageFamily());

            //
            GEDCOMIndividualRecord indiRec = fContext.Tree.XRefIndex_Find("I4") as GEDCOMIndividualRecord;
            Assert.IsNull(indiRec.GetMarriageFamily());
            Assert.IsNotNull(indiRec.GetMarriageFamily(true));

            GEDCOMRecordTest(indiRec);

            Assert.IsNotNull(indiRec.Aliases);
            Assert.IsNotNull(indiRec.AncestorsInterest);
            Assert.IsNotNull(indiRec.Associations);
            Assert.IsNotNull(indiRec.DescendantsInterest);
            Assert.IsNotNull(indiRec.IndividualOrdinances);
            Assert.IsNotNull(indiRec.Submittors);
            Assert.IsNotNull(indiRec.UserReferences); // for GEDCOMRecord

            Assert.Throws(typeof(ArgumentException), () => { indiRec.AddEvent(GEDCOMFamilyEvent.Create(null, null, "", "") as GEDCOMCustomEvent); });

            GEDCOMIndividualRecord father, mother;
            GEDCOMFamilyRecord fam = indiRec.GetParentsFamily();
            if (fam == null) {
                father = null;
                mother = null;
            } else {
                father = fam.GetHusband();
                mother = fam.GetWife();
            }

            Assert.IsNull(father);
            Assert.IsNull(mother);

            indiRec.Sex = GEDCOMSex.svMale;
            Assert.AreEqual(GEDCOMSex.svMale, indiRec.Sex);

            indiRec.Restriction = GEDCOMRestriction.rnLocked;
            Assert.AreEqual(GEDCOMRestriction.rnLocked, indiRec.Restriction);

            indiRec.Patriarch = true;
            Assert.AreEqual(true, indiRec.Patriarch);
            indiRec.Patriarch = false;
            Assert.AreEqual(false, indiRec.Patriarch);

            indiRec.Bookmark = true;
            Assert.AreEqual(true, indiRec.Bookmark);
            indiRec.Bookmark = false;
            Assert.AreEqual(false, indiRec.Bookmark);

            indiRec.AncestralFileNumber = "test11";
            Assert.AreEqual("test11", indiRec.AncestralFileNumber);

            indiRec.PermanentRecordFileNumber = "test22";
            Assert.AreEqual("test22", indiRec.PermanentRecordFileNumber);

            Assert.Throws(typeof(ArgumentException), () => { indiRec.MoveTo(null, false); });

            using (GEDCOMIndividualRecord copyIndi = new GEDCOMIndividualRecord(null, null, "", "")) {
                Assert.IsNotNull(copyIndi);

                Assert.Throws(typeof(ArgumentException), () => { copyIndi.Assign(null); });

                copyIndi.Assign(indiRec);
                Assert.AreEqual(100.0f, indiRec.IsMatch(copyIndi, new MatchParams()));
            }


            Assert.IsFalse(indiRec.IsEmpty());
            indiRec.Clear();
            Assert.IsTrue(indiRec.IsEmpty());

            float ca = indiRec.GetCertaintyAssessment();
            Assert.AreEqual(0.0f, ca);


            Assert.IsNull(indiRec.GetPrimaryMultimediaLink());
            GEDCOMMultimediaLink mmLink = indiRec.SetPrimaryMultimediaLink(null);
            Assert.IsNull(mmLink);
            GEDCOMMultimediaRecord mmRec = fContext.Tree.CreateMultimedia();
            mmLink = indiRec.SetPrimaryMultimediaLink(mmRec);
            Assert.IsNotNull(mmLink);
            mmLink = indiRec.GetPrimaryMultimediaLink();
            Assert.AreEqual(mmRec, mmLink.Value);


            Assert.AreEqual(-1, indiRec.IndexOfGroup(null));
            Assert.AreEqual(-1, indiRec.IndexOfSpouse(null));


            GEDCOMIndividualRecord indi2 = fContext.Tree.XRefIndex_Find("I2") as GEDCOMIndividualRecord;
            GEDCOMAssociation asso = indiRec.AddAssociation("test", indi2);
            Assert.IsNotNull(asso);

            using (GEDCOMIndividualRecord indi = new GEDCOMIndividualRecord(fContext.Tree, fContext.Tree, "", "")) {
                Assert.IsNotNull(indi);

                var parts = GKUtils.GetNameParts(indi); // test with empty PersonalNames
                Assert.AreEqual("", parts.Surname);
                Assert.AreEqual("", parts.Name);
                Assert.AreEqual("", parts.Patronymic);

                indi.AddPersonalName(new GEDCOMPersonalName(fContext.Tree, indi, "", "")); // test with empty Name
                parts = GKUtils.GetNameParts(indi);
                Assert.AreEqual("", parts.Surname);
                Assert.AreEqual("", parts.Name);
                Assert.AreEqual("", parts.Patronymic);
                indi.PersonalNames.Clear();

                string st;
                Assert.AreEqual("", GKUtils.GetNameString(indi, true, false));
                Assert.AreEqual("", GKUtils.GetNickString(indi));

                GEDCOMPersonalName pName = new GEDCOMPersonalName(fContext.Tree, indi, "", "");
                indi.AddPersonalName(pName);
                pName.Pieces.Nickname = "BigHead";
                pName.SetNameParts("Ivan", "Petrov", "");

                st = GKUtils.GetNameString(indi, true, true);
                Assert.AreEqual("Petrov Ivan [BigHead]", st);
                st = GKUtils.GetNameString(indi, false, true);
                Assert.AreEqual("Ivan Petrov [BigHead]", st);
                Assert.AreEqual("BigHead", GKUtils.GetNickString(indi));

                Assert.IsNull(indi.GetParentsFamily());
                Assert.IsNotNull(indi.GetParentsFamily(true));

                // MoveTo test
                GEDCOMIndividualRecord ind = fContext.Tree.XRefIndex_Find("I2") as GEDCOMIndividualRecord;

                indi.AddAssociation("test", ind);
                indi.Aliases.Add(new GEDCOMAlias(fContext.Tree, indi, "", ""));
                indi.IndividualOrdinances.Add(new GEDCOMIndividualOrdinance(fContext.Tree, indi, "", ""));
                indi.AncestorsInterest.Add(new GEDCOMPointer(fContext.Tree, indi, "", ""));
                indi.DescendantsInterest.Add(new GEDCOMPointer(fContext.Tree, indi, "", ""));
                indi.Submittors.Add(new GEDCOMPointer(fContext.Tree, indi, "", ""));

                using (GEDCOMIndividualRecord indi3 = new GEDCOMIndividualRecord(fContext.Tree, fContext.Tree, "", "")) {
                    indi.MoveTo(indi3, false);

                    st = GKUtils.GetNameString(indi3, true, true);
                    Assert.AreEqual("Petrov Ivan [BigHead]", st);
                }

                indi.ResetOwner(fContext.Tree);
                Assert.AreEqual(fContext.Tree, indi.Owner);
            }
        }

        [Test]
        public void GEDCOMPersonalName_Tests()
        {
            GEDCOMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I1") as GEDCOMIndividualRecord;

            GEDCOMPersonalName pName = iRec.PersonalNames[0];
            Assert.AreEqual("Ivanov", pName.Surname);
            Assert.AreEqual("Ivan Ivanovich", pName.FirstPart);

            pName.SetNameParts("Ivan Ivanovich", "Ivanov", "testLastPart");
            Assert.AreEqual("Ivanov", pName.Surname);
            Assert.AreEqual("Ivan Ivanovich", pName.FirstPart);
            Assert.AreEqual("testLastPart", pName.LastPart);

//			GEDCOMPersonalNamePieces pieces = pName.Pieces;
//			Assert.AreEqual(pieces.Surname, "surname");
//			Assert.AreEqual(pieces.Name, "name");
//			Assert.AreEqual(pieces.PatronymicName, "patr");

            var parts = GKUtils.GetNameParts(iRec);
            Assert.AreEqual("Ivanov", parts.Surname);
            Assert.AreEqual("Ivan", parts.Name);
            Assert.AreEqual("Ivanovich", parts.Patronymic);


            GEDCOMPersonalName persName = GEDCOMPersonalName.Create(iRec.Owner, iRec, "", "") as GEDCOMPersonalName;
            iRec.AddPersonalName(persName);

            persName = iRec.PersonalNames[0];
            persName.NameType = GEDCOMNameType.ntBirth;
            Assert.AreEqual(GEDCOMNameType.ntBirth, persName.NameType);

            //

            persName.SetNameParts("Petr", "Ivanov", "Fedoroff");

            //persName.Surname = "Ivanov";
            Assert.AreEqual("Petr", persName.FirstPart);
            Assert.AreEqual("Ivanov", persName.Surname);
            Assert.AreEqual("Fedoroff", persName.LastPart);

            Assert.AreEqual("Petr Ivanov Fedoroff", persName.FullName);

            persName.FirstPart = "Petr";
            Assert.AreEqual("Petr", persName.FirstPart);

            persName.Surname = "Test";
            Assert.AreEqual("Test", persName.Surname);

            persName.LastPart = "Fedoroff";
            Assert.AreEqual("Fedoroff", persName.LastPart);

            //

            GEDCOMPersonalNamePieces pnPieces = persName.Pieces;
            
            pnPieces.Prefix = "Prefix";
            Assert.AreEqual("Prefix", pnPieces.Prefix);

            pnPieces.Given = "Given";
            Assert.AreEqual("Given", pnPieces.Given);

            pnPieces.Nickname = "Nickname";
            Assert.AreEqual("Nickname", pnPieces.Nickname);

            pnPieces.SurnamePrefix = "SurnamePrefix";
            Assert.AreEqual("SurnamePrefix", pnPieces.SurnamePrefix);

            pnPieces.Surname = "Surname";
            Assert.AreEqual("Surname", pnPieces.Surname);

            pnPieces.Suffix = "Suffix";
            Assert.AreEqual("Suffix", pnPieces.Suffix);

            pnPieces.PatronymicName = "PatronymicName";
            Assert.AreEqual("PatronymicName", pnPieces.PatronymicName);

            pnPieces.MarriedName = "MarriedName";
            Assert.AreEqual("MarriedName", pnPieces.MarriedName);

            pnPieces.ReligiousName = "ReligiousName";
            Assert.AreEqual("ReligiousName", pnPieces.ReligiousName);

            pnPieces.CensusName = "CensusName";
            Assert.AreEqual("CensusName", pnPieces.CensusName);

            persName.Pack();

            //

            Assert.AreEqual(GEDCOMLanguageID.Unknown, persName.Language.Value);
            persName.Language.Value = GEDCOMLanguageID.English;
            Assert.AreEqual(GEDCOMLanguageID.English, persName.Language.Value);
            persName.Language.Value = GEDCOMLanguageID.Unknown;
            Assert.AreEqual(GEDCOMLanguageID.Unknown, persName.Language.Value);
            persName.Language.Value = GEDCOMLanguageID.Polish;
            Assert.AreEqual(GEDCOMLanguageID.Polish, persName.Language.Value);

            //

            string buf = TagStreamTest(persName);
            Assert.AreEqual("1 NAME Petr /Test/ Fedoroff\r\n"+
                            "2 TYPE birth\r\n"+
                            "2 _LANG Polish\r\n"+ // extension
                            "2 SURN Surname\r\n"+
                            "2 GIVN Given\r\n"+
                            "2 _PATN PatronymicName\r\n"+
                            "2 NPFX Prefix\r\n"+
                            "2 NICK Nickname\r\n"+
                            "2 SPFX SurnamePrefix\r\n"+
                            "2 NSFX Suffix\r\n"+
                            "2 _MARN MarriedName\r\n"+
                            "2 _RELN ReligiousName\r\n"+
                            "2 _CENN CensusName\r\n", buf);

            persName.Language.Value = GEDCOMLanguageID.Unknown;
            persName.Pack();

            using (GEDCOMPersonalName nameCopy = new GEDCOMPersonalName(iRec.Owner, iRec, "", "")) {
                Assert.Throws(typeof(ArgumentException), () => { nameCopy.Assign(null); });

                iRec.AddPersonalName(nameCopy);
                nameCopy.Assign(persName);

                string buf2 = TagStreamTest(nameCopy);
                Assert.AreEqual("1 NAME Petr /Test/ Fedoroff\r\n"+
                                "2 TYPE birth\r\n"+
                                "2 SURN Surname\r\n"+
                                "2 GIVN Given\r\n"+
                                "2 _PATN PatronymicName\r\n"+
                                "2 NPFX Prefix\r\n"+
                                "2 NICK Nickname\r\n"+
                                "2 SPFX SurnamePrefix\r\n"+
                                "2 NSFX Suffix\r\n"+
                                "2 _MARN MarriedName\r\n"+
                                "2 _RELN ReligiousName\r\n"+
                                "2 _CENN CensusName\r\n", buf2);

                iRec.PersonalNames.Delete(nameCopy);
            }

            using (GEDCOMPersonalName name1 = new GEDCOMPersonalName(null, null, "", "")) {
                Assert.AreEqual("", name1.FirstPart);
                Assert.AreEqual("", name1.Surname);

                Assert.AreEqual(0.0f, name1.IsMatch(null, false));

                using (GEDCOMPersonalName name2 = new GEDCOMPersonalName(null, null, "", "")) {
                    Assert.AreEqual(0.0f, name1.IsMatch(name2, false));

                    name1.SetNameParts("Ivan", "Dub", "");
                    name2.SetNameParts("Ivan", "Dub", "");
                    Assert.AreEqual(100.0f, name1.IsMatch(name2, false));

                    name1.SetNameParts("Ivan", "Dub", "");
                    name2.SetNameParts("Ivan", "Dub2", "");
                    Assert.AreEqual(12.5f, name1.IsMatch(name2, false));

                    name1.SetNameParts("Ivan", "Dub", "");
                    name2.SetNameParts("Ivan2", "Dub", "");
                    Assert.AreEqual(50.0f, name1.IsMatch(name2, false));
                }
            }

            persName.ResetOwner(fContext.Tree);
            Assert.AreEqual(fContext.Tree, persName.Owner);

            persName.Clear();
            Assert.IsTrue(persName.IsEmpty());
        }

        [Test]
        public void GEDCOMFileReference_Tests()
        {
            using (GEDCOMFileReference fileRef = new GEDCOMFileReference(null, null, "", "")) {
                fileRef.MediaType = GEDCOMMediaType.mtAudio;
                Assert.AreEqual(GEDCOMMediaType.mtAudio, fileRef.MediaType);
            }

            Assert.AreEqual(GEDCOMMultimediaFormat.mfUnknown, GEDCOMFileReference.RecognizeFormat(""));
            Assert.AreEqual(GEDCOMMultimediaFormat.mfUnknown, GEDCOMFileReference.RecognizeFormat("sample.xxx"));
            Assert.AreEqual(GEDCOMMultimediaFormat.mfBMP, GEDCOMFileReference.RecognizeFormat("sample.BMP"));
            Assert.AreEqual(GEDCOMMultimediaFormat.mfGIF, GEDCOMFileReference.RecognizeFormat("sample.Gif"));
            Assert.AreEqual(GEDCOMMultimediaFormat.mfJPG, GEDCOMFileReference.RecognizeFormat("sample.jpg"));
            Assert.AreEqual(GEDCOMMultimediaFormat.mfJPG, GEDCOMFileReference.RecognizeFormat("sample.Jpeg"));
            Assert.AreEqual(GEDCOMMultimediaFormat.mfOLE, GEDCOMFileReference.RecognizeFormat("sample.ole"));
            Assert.AreEqual(GEDCOMMultimediaFormat.mfPCX, GEDCOMFileReference.RecognizeFormat("sample.pCx"));
            Assert.AreEqual(GEDCOMMultimediaFormat.mfTIF, GEDCOMFileReference.RecognizeFormat("sample.TiF"));
            Assert.AreEqual(GEDCOMMultimediaFormat.mfTIF, GEDCOMFileReference.RecognizeFormat("sample.tiff"));
            Assert.AreEqual(GEDCOMMultimediaFormat.mfWAV, GEDCOMFileReference.RecognizeFormat("sample.wav"));
            Assert.AreEqual(GEDCOMMultimediaFormat.mfTXT, GEDCOMFileReference.RecognizeFormat("sample.txt"));
            Assert.AreEqual(GEDCOMMultimediaFormat.mfRTF, GEDCOMFileReference.RecognizeFormat("sample.rtf"));
            Assert.AreEqual(GEDCOMMultimediaFormat.mfAVI, GEDCOMFileReference.RecognizeFormat("sample.AvI"));
            Assert.AreEqual(GEDCOMMultimediaFormat.mfTGA, GEDCOMFileReference.RecognizeFormat("sample.TGA"));
            Assert.AreEqual(GEDCOMMultimediaFormat.mfPNG, GEDCOMFileReference.RecognizeFormat("sample.png"));
            Assert.AreEqual(GEDCOMMultimediaFormat.mfMPG, GEDCOMFileReference.RecognizeFormat("sample.mpg"));
            Assert.AreEqual(GEDCOMMultimediaFormat.mfMPG, GEDCOMFileReference.RecognizeFormat("sample.mpeg"));
            Assert.AreEqual(GEDCOMMultimediaFormat.mfHTM, GEDCOMFileReference.RecognizeFormat("sample.htm"));
            Assert.AreEqual(GEDCOMMultimediaFormat.mfHTM, GEDCOMFileReference.RecognizeFormat("sample.html"));
        }

        [Test]
        public void GEDCOMLanguage_Tests()
        {
            using (GEDCOMLanguage langTag = GEDCOMLanguage.Create(null, null, "", "") as GEDCOMLanguage) {
                Assert.IsTrue(langTag.IsEmpty());

                langTag.Value = GEDCOMLanguageID.AngloSaxon;
                Assert.AreEqual(GEDCOMLanguageID.AngloSaxon, langTag.Value);

                langTag.ParseString("Spanish");
                Assert.AreEqual("Spanish", langTag.StringValue);

                using (GEDCOMLanguage langTag2 = GEDCOMLanguage.Create(null, null, "", "") as GEDCOMLanguage) {
                    Assert.IsTrue(langTag2.IsEmpty());

                    langTag2.Assign(null);

                    langTag2.Assign(langTag);
                    Assert.AreEqual("Spanish", langTag2.StringValue);
                }

                langTag.Clear();
                Assert.IsTrue(langTag.IsEmpty());
            }
        }

        [Test]
        public void GEDCOMGroupRecord_Tests2()
        {
            using (GEDCOMGroupRecord grpRec = GEDCOMGroupRecord.Create(null, null, "", "") as GEDCOMGroupRecord)
            {
                Assert.IsNotNull(grpRec);

                grpRec.ResetOwner(fContext.Tree);
                Assert.AreEqual(fContext.Tree, grpRec.Owner);
            }
        }

        [Test]
        public void GEDCOMGroupRecord_Tests()
        {
            using (GEDCOMGroupRecord groupRec = fContext.Tree.CreateGroup()) {
                GEDCOMIndividualRecord member = fContext.Tree.XRefIndex_Find("I1") as GEDCOMIndividualRecord;

                groupRec.GroupName = "Test Group";
                Assert.AreEqual("Test Group", groupRec.GroupName);

                groupRec.DeleteTag(GEDCOMTagType._UID);
                groupRec.DeleteTag(GEDCOMTagType.CHAN);
                string buf = TagStreamTest(groupRec);
                Assert.AreEqual("0 @G2@ _GROUP\r\n1 NAME Test Group\r\n", buf);

                bool res = groupRec.AddMember(null);
                Assert.IsFalse(res);

                res = groupRec.RemoveMember(null);
                Assert.IsFalse(res);

                Assert.AreEqual(-1, groupRec.IndexOfMember(null));

                groupRec.AddMember(member);
                Assert.AreEqual(0, groupRec.IndexOfMember(member));

                groupRec.RemoveMember(member);
                Assert.AreEqual(-1, groupRec.IndexOfMember(member));

                Assert.IsFalse(groupRec.IsEmpty());
                groupRec.Clear();
                Assert.IsTrue(groupRec.IsEmpty());
            }
        }

        [Test]
        public void GEDCOMList_Tests()
        {
            GEDCOMObject obj1 = new GEDCOMObject();
            GEDCOMObject obj2 = new GEDCOMObject();

            using (GEDCOMList<GEDCOMObject> list = new GEDCOMList<GEDCOMObject>(null)) {
                Assert.IsNull(list.Owner);

                // internal list is null (all routines instant returned)
                list.Delete(null);
                list.Exchange(0, 1);
                Assert.IsNull(list.Extract(0));
                Assert.IsNull(list.Extract(null));

                // normal checks
                list.Add(obj1);
                list.Add(obj2);
                Assert.AreEqual(0, list.IndexOf(obj1));
                Assert.AreEqual(1, list.IndexOf(obj2));

                list.Delete(obj1);
                Assert.AreEqual(-1, list.IndexOf(obj1));
                Assert.AreEqual(0, list.IndexOf(obj2));

                list.Add(obj1);
                Assert.AreEqual(1, list.IndexOf(obj1));
                Assert.AreEqual(0, list.IndexOf(obj2));
                list.Exchange(0, 1);
                Assert.AreEqual(0, list.IndexOf(obj1));
                Assert.AreEqual(1, list.IndexOf(obj2));

                Assert.AreEqual(null, list.Extract(null));
                list.Add(obj1);
                Assert.AreEqual(obj1, list.Extract(obj1));

                foreach (GEDCOMObject obj in list) {
                }
            }
        }

        #endregion

        #region Partial Tests

        [Test]
        public void GEDCOMCustomEvent_Tests()
        {
            using (GEDCOMIndividualAttribute customEvent = GEDCOMIndividualAttribute.Create(null, null, "", "") as GEDCOMIndividualAttribute)
            {
                Assert.IsNotNull(customEvent);

                StringList strs = new StringList("test");
                customEvent.PhysicalDescription = strs;
                Assert.AreEqual(strs.Text, customEvent.PhysicalDescription.Text);

                customEvent.AddTag("EMAIL", "email", null);
                Assert.AreEqual("email", customEvent.Address.EmailAddresses[0].StringValue);

                customEvent.Pack();

                GEDCOMTree otherTree = new GEDCOMTree();
                customEvent.ResetOwner(otherTree);
                Assert.AreEqual(otherTree, customEvent.Owner);
            }

            using (GEDCOMIndividualEvent customEvent = GEDCOMIndividualEvent.Create(null, null, "", "") as GEDCOMIndividualEvent)
            {
                Assert.IsNotNull(customEvent);

                // stream test
                customEvent.SetName("BIRT");
                customEvent.Date.ParseString("20 SEP 1970");
                customEvent.Place.StringValue = "test place";
                string buf = TagStreamTest(customEvent);
                Assert.AreEqual("0 BIRT\r\n"+
                                "1 DATE 20 SEP 1970\r\n"+
                                "1 PLAC test place\r\n", buf);

                using (GEDCOMIndividualEvent copyEvent = GEDCOMIndividualEvent.Create(null, null, "", "") as GEDCOMIndividualEvent)
                {
                    Assert.IsNotNull(copyEvent);
                    copyEvent.Assign(customEvent);

                    string buf1 = TagStreamTest(copyEvent);
                    Assert.AreEqual("0 BIRT\r\n"+
                                    "1 DATE 20 SEP 1970\r\n"+
                                    "1 PLAC test place\r\n", buf1);
                }

                customEvent.AddTag("EMAIL", "email", null);
                Assert.AreEqual("email", customEvent.Address.EmailAddresses[0].StringValue);

                customEvent.Pack();

                GEDCOMTree otherTree = new GEDCOMTree();
                customEvent.ResetOwner(otherTree);
                Assert.AreEqual(otherTree, customEvent.Owner);
            }

            using (GEDCOMFamilyEvent customEvent = GEDCOMFamilyEvent.Create(null, null, "", "") as GEDCOMFamilyEvent)
            {
                Assert.IsNotNull(customEvent);

                customEvent.AddTag("EMAIL", "email", null);
                Assert.AreEqual("email", customEvent.Address.EmailAddresses[0].StringValue);

                customEvent.Pack();

                GEDCOMTree otherTree = new GEDCOMTree();
                customEvent.ResetOwner(otherTree);
                Assert.AreEqual(otherTree, customEvent.Owner);
            }
        }

        public static void GEDCOMCustomEventTest(GEDCOMCustomEvent evt, string dateTest)
        {
            GEDCOMEventDetailTest(evt, dateTest);

            Assert.AreEqual(evt.Date.GetDateTime(), ParseDT(dateTest));
        }

        [Test]
        public void GEDCOMPlaceTest()
        {
            using (GEDCOMPlace place = GEDCOMPlace.Create(null, null, "", "") as GEDCOMPlace) {
                place.Form = "abrakadabra";
                Assert.AreEqual("abrakadabra", place.Form);

                Assert.IsNotNull(place.Map);
                Assert.IsNotNull(place.Location);
            }
        }

        private static void GEDCOMEventDetailTest(GEDCOMCustomEvent detail, string dateTest)
        {
            Assert.AreEqual(ParseDT(dateTest), detail.Date.Date);
            Assert.AreEqual("Ivanovo", detail.Place.StringValue);

            Assert.IsNotNull(detail.Place);

            detail.Agency = "test agency";
            Assert.AreEqual("test agency", detail.Agency);

            detail.Classification = "test type";
            Assert.AreEqual("test type", detail.Classification);

            detail.Cause = "test cause";
            Assert.AreEqual("test cause", detail.Cause);

            detail.ReligiousAffilation = "test aff";
            Assert.AreEqual("test aff", detail.ReligiousAffilation);

            detail.Restriction = GEDCOMRestriction.rnLocked;
            Assert.AreEqual(GEDCOMRestriction.rnLocked, detail.Restriction);
        }

        [Test]
        public void GEDCOMTag_Test()
        {
            using (GEDCOMTag tag = GEDCOMTag.Create(null, null, "", "")) {
                Assert.AreEqual(-1, tag.IndexOfTag(null));
            }
        }

        private static void GEDCOMRecordTest(GEDCOMRecord rec)
        {
            Assert.Throws(typeof(ArgumentException), () => { rec.Assign(null); });

            rec.AutomatedRecordID = "test11";
            Assert.AreEqual("test11", rec.AutomatedRecordID);

            Assert.AreEqual(GEDCOMRecordType.rtIndividual, rec.RecordType);

            Assert.AreEqual(4, rec.GetId());
            Assert.AreEqual("4", rec.GetXRefNum());

            Assert.AreEqual(-1, rec.IndexOfSource(null));

            rec.AddUserRef("test userref");
            Assert.AreEqual("test userref", rec.UserReferences[0].StringValue);
        }

        [Test]
        public void GEDCOMFamilyRecord_Tests()
        {
            using (GEDCOMFamilyRecord famRec = GEDCOMFamilyRecord.Create(fContext.Tree, fContext.Tree, "", "") as GEDCOMFamilyRecord)
            {
                Assert.IsNotNull(famRec);

                GEDCOMIndividualRecord unkInd = new GEDCOMIndividualRecord(null, null, "", "");
                unkInd.Sex = GEDCOMSex.svUndetermined;
                Assert.IsFalse(famRec.AddSpouse(unkInd));

                GEDCOMIndividualRecord child1 = fContext.Tree.CreateIndividual(); // for pointer need a proper object
                Assert.IsTrue(famRec.AddChild(child1));

                GEDCOMIndividualRecord child2 = fContext.Tree.CreateIndividual(); // for pointer need a proper object
                Assert.IsTrue(famRec.AddChild(child2));
                Assert.AreEqual(1, famRec.IndexOfChild(child2));

                famRec.DeleteChild(child1);
                Assert.AreEqual(-1, famRec.IndexOfChild(child1));

                string str = GKUtils.GetFamilyString(famRec, null, null);
                Assert.AreEqual("? - ?", str);

                str = GKUtils.GetFamilyString(famRec, "x", "x");
                Assert.AreEqual("x - x", str);

                Assert.AreEqual(0.0f, famRec.IsMatch(null, new MatchParams()));
                Assert.AreEqual(100.0f, famRec.IsMatch(famRec, new MatchParams()));

                // MoveTo test
                Assert.Throws(typeof(ArgumentException), () => { famRec.MoveTo(null, false); });

                GEDCOMCustomEvent evt = famRec.AddEvent(new GEDCOMFamilyEvent(fContext.Tree, famRec, "MARR", "01 SEP 1981"));
                Assert.AreEqual(1, famRec.Events.Count);
                Assert.AreEqual(evt, famRec.FindEvent("MARR"));

                GEDCOMSpouseSealing sps = famRec.SpouseSealings.Add(new GEDCOMSpouseSealing(fContext.Tree, fContext.Tree, "", ""));
                Assert.AreEqual(1, famRec.SpouseSealings.Count);
                Assert.AreEqual(sps, famRec.SpouseSealings[0]);

                using (GEDCOMFamilyRecord famRec2 = GEDCOMFamilyRecord.Create(fContext.Tree, fContext.Tree, "", "") as GEDCOMFamilyRecord)
                {
                    Assert.AreEqual(0, famRec2.Events.Count);
                    Assert.AreEqual(null, famRec2.FindEvent("MARR"));

                    Assert.AreEqual(0, famRec2.SpouseSealings.Count);
                    Assert.AreEqual(null, famRec2.SpouseSealings[0]);

                    famRec.MoveTo(famRec2, false);

                    Assert.AreEqual(1, famRec2.Events.Count);
                    Assert.AreEqual(evt, famRec2.FindEvent("MARR"));

                    Assert.AreEqual(1, famRec2.SpouseSealings.Count);
                    Assert.AreEqual(sps, famRec2.SpouseSealings[0]);
                }

                famRec.ResetOwner(fContext.Tree);
                Assert.AreEqual(fContext.Tree, famRec.Owner);
            }
        }

        private static void GEDCOMFamilyRecordTest(GEDCOMFamilyRecord famRec, GEDCOMIndividualRecord indiv)
        {
            Assert.IsNotNull(famRec.Submitter);
            Assert.IsNotNull(famRec.SpouseSealings);

            famRec.Restriction = GEDCOMRestriction.rnLocked;
            Assert.AreEqual(GEDCOMRestriction.rnLocked, famRec.Restriction);

            famRec.AddChild(indiv);
            Assert.AreEqual(0, famRec.IndexOfChild(indiv));

            // stream test
            famRec.DeleteTag(GEDCOMTagType._UID);
            famRec.DeleteTag(GEDCOMTagType.CHAN);
            string buf = TagStreamTest(famRec);
            Assert.AreEqual("0 @F1@ FAM\r\n"+
                            "1 SUBM\r\n"+
                            "1 RESN locked\r\n"+
                            "1 CHIL @I1@\r\n", buf);

            // Integrity test
            GEDCOMChildToFamilyLink childLink = indiv.ChildToFamilyLinks[0];
            Assert.IsNotNull(childLink.Family);

            famRec.RemoveChild(indiv);
            Assert.AreEqual(-1, famRec.IndexOfChild(indiv));

            //

            Assert.Throws(typeof(ArgumentException), () => { famRec.AddEvent(GEDCOMIndividualEvent.Create(null, null, "", "") as GEDCOMCustomEvent); });

            //

            famRec.Husband.Value = indiv;
            Assert.AreEqual(indiv, famRec.GetHusband());
            famRec.Husband.Value = null;

            //

            famRec.Wife.Value = indiv;
            Assert.AreEqual(indiv, famRec.GetWife());
            famRec.Wife.Value = null;

            //

            indiv.Sex = GEDCOMSex.svMale;
            famRec.AddSpouse(indiv);
            Assert.AreEqual(0, indiv.IndexOfSpouse(famRec));
            GEDCOMSpouseToFamilyLinkTest(indiv.SpouseToFamilyLinks[0]);
            Assert.IsNull(famRec.GetSpouseBy(indiv));
            famRec.RemoveSpouse(indiv);

            indiv.Sex = GEDCOMSex.svFemale;
            famRec.AddSpouse(indiv);
            Assert.AreEqual(0, indiv.IndexOfSpouse(famRec));
            GEDCOMSpouseToFamilyLinkTest(indiv.SpouseToFamilyLinks[0]);
            Assert.IsNull(famRec.GetSpouseBy(indiv));
            famRec.RemoveSpouse(indiv);

            //

            famRec.SortChilds();

            //

            famRec.AddChild(null);
            famRec.RemoveChild(null);
            famRec.AddSpouse(null);
            famRec.RemoveSpouse(null);

            Assert.IsFalse(famRec.IsEmpty());
            famRec.Clear();
            Assert.IsTrue(famRec.IsEmpty());
        }

        [Test]
        public void GEDCOMChildToFamilyLink_Tests()
        {
            using (GEDCOMChildToFamilyLink childLink = GEDCOMChildToFamilyLink.Create(null, null, "", "") as GEDCOMChildToFamilyLink)
            {
                Assert.IsNotNull(childLink);

                childLink.ChildLinkageStatus = GEDCOMChildLinkageStatus.clChallenged;
                Assert.AreEqual(GEDCOMChildLinkageStatus.clChallenged, childLink.ChildLinkageStatus);

                childLink.PedigreeLinkageType = GEDCOMPedigreeLinkageType.plFoster;
                Assert.AreEqual(GEDCOMPedigreeLinkageType.plFoster, childLink.PedigreeLinkageType);
            }
        }

        private static void GEDCOMSpouseToFamilyLinkTest(GEDCOMSpouseToFamilyLink spouseLink)
        {
            Assert.IsNotNull(spouseLink.Family);
            
            using (spouseLink = GEDCOMSpouseToFamilyLink.Create(null, null, "", "") as GEDCOMSpouseToFamilyLink)
            {
                Assert.IsNotNull(spouseLink);
            }
        }

        [Test]
        public void GEDCOMSourceRecord_Tests()
        {
            GEDCOMTree tree = new GEDCOMTree();

            // check match
            using (GEDCOMSourceRecord src1 = GEDCOMSourceRecord.Create(tree, tree, "", "") as GEDCOMSourceRecord)
            {
                Assert.IsNotNull(src1, "src1 != null");

                Assert.Throws(typeof(ArgumentNullException), () => { src1.RemoveRepository(null); });

                using (GEDCOMSourceRecord src2 = new GEDCOMSourceRecord(tree, tree, "", ""))
                {
                    Assert.IsNotNull(src2, "src2 != null");

                    Assert.AreEqual(0.0f, src1.IsMatch(null, new MatchParams()));

                    // empty records
                    Assert.AreEqual(100.0f, src1.IsMatch(src2, new MatchParams()));

                    // filled records
                    src1.FiledByEntry = "test source";
                    src2.FiledByEntry = "test source";
                    Assert.AreEqual(100.0f, src1.IsMatch(src2, new MatchParams()));
                }

                src1.ResetOwner(fContext.Tree);
                Assert.AreEqual(fContext.Tree, src1.Owner);
            }

            // check move
            using (GEDCOMSourceRecord src1 = GEDCOMSourceRecord.Create(tree, tree, "", "") as GEDCOMSourceRecord)
            {
                Assert.Throws(typeof(ArgumentException), () => { src1.MoveTo(null, false); });

                // fill the record
                src1.FiledByEntry = "test source";
                src1.Title = new StringList("test title");
                src1.Originator = new StringList("test author");
                src1.Publication = new StringList("test publ");
                src1.Text = new StringList("test text");

                Assert.AreEqual("test source", src1.FiledByEntry);
                Assert.AreEqual("test title", src1.Title.Text);
                Assert.AreEqual("test author", src1.Originator.Text);
                Assert.AreEqual("test publ", src1.Publication.Text);
                Assert.AreEqual("test text", src1.Text.Text);

                GEDCOMRepositoryRecord repRec = tree.CreateRepository();
                repRec.RepositoryName = "test repository";
                src1.AddRepository(repRec);
                Assert.AreEqual(1, src1.RepositoryCitations.Count);

                using (GEDCOMSourceRecord src2 = new GEDCOMSourceRecord(tree, tree, "", ""))
                {
                    src2.FiledByEntry = "test source 2"; // title isn't replaced

                    Assert.AreEqual(0, src2.RepositoryCitations.Count);

                    src1.MoveTo(src2, false);

                    Assert.AreEqual("test source 2", src2.FiledByEntry);

                    Assert.AreEqual("test title", src2.Title.Text);
                    Assert.AreEqual("test author", src2.Originator.Text);
                    Assert.AreEqual("test publ", src2.Publication.Text);
                    Assert.AreEqual("test text", src2.Text.Text);

                    Assert.AreEqual(1, src2.RepositoryCitations.Count);
                }
            }
        }

        private static void GEDCOMSourceRecordTest(GEDCOMSourceRecord sourRec, GEDCOMIndividualRecord indiv, GEDCOMRepositoryRecord repRec)
        {
            Assert.IsNotNull(sourRec.Data);
            
            sourRec.FiledByEntry = "This is test source";
            Assert.AreEqual("This is test source", sourRec.FiledByEntry);

            //
            sourRec.Originator = new StringList("author");
            Assert.AreEqual("author", sourRec.Originator.Text.Trim());
            
            sourRec.Title = new StringList("title");
            Assert.AreEqual("title", sourRec.Title.Text.Trim());
            
            sourRec.Publication = new StringList("publication");
            Assert.AreEqual("publication", sourRec.Publication.Text.Trim());
            
            sourRec.Text = new StringList("sample");
            Assert.AreEqual("sample", sourRec.Text.Text.Trim());

            //
            sourRec.SetOriginatorArray(new string[] {"author"});
            Assert.AreEqual("author", sourRec.Originator.Text.Trim());
            
            sourRec.SetTitleArray(new string[] {"title"});
            Assert.AreEqual("title", sourRec.Title.Text.Trim());
            
            sourRec.SetPublicationArray(new string[] {"publication"});
            Assert.AreEqual("publication", sourRec.Publication.Text.Trim());
            
            sourRec.SetTextArray(new string[] {"sample"});
            Assert.AreEqual("sample", sourRec.Text.Text.Trim());
            
            //
            GEDCOMSourceCitationTest(sourRec, indiv);
            GEDCOMRepositoryCitationTest(sourRec, repRec);

            sourRec.DeleteTag(GEDCOMTagType._UID);
            sourRec.DeleteTag(GEDCOMTagType.CHAN);
            string buf = TagStreamTest(sourRec);
            Assert.AreEqual("0 @S1@ SOUR\r\n"+
                            "1 DATA\r\n"+
                            "1 ABBR This is test source\r\n"+
                            "1 AUTH author\r\n"+
                            "1 TITL title\r\n"+
                            "1 PUBL publication\r\n"+
                            "1 TEXT sample\r\n"+
                            "1 REPO @R1@\r\n", buf);

            //
            Assert.IsFalse(sourRec.IsEmpty());
            sourRec.Clear();
            Assert.IsTrue(sourRec.IsEmpty());
        }

        [Test]
        public void GEDCOMSourceCitation_Tests()
        {
            using (GEDCOMSourceCitation srcCit = GEDCOMSourceCitation.Create(null, null, "", "") as GEDCOMSourceCitation) {
                Assert.IsNotNull(srcCit);
            }
        }

        private static void GEDCOMSourceCitationTest(GEDCOMSourceRecord sourRec, GEDCOMIndividualRecord indiv)
        {
            GEDCOMSourceCitation srcCit = indiv.AddSource(sourRec, "p2", 3);

            int idx = indiv.IndexOfSource(sourRec);
            Assert.AreEqual(0, idx);

            Assert.AreEqual("p2", srcCit.Page);
            Assert.AreEqual(3, srcCit.CertaintyAssessment);

            Assert.IsTrue(srcCit.IsPointer, "srcCit.IsPointer");

            Assert.IsFalse(srcCit.IsEmpty(), "srcCit.IsEmpty()"); // its pointer

            srcCit.Clear();
            srcCit.Value = null;

            Assert.IsTrue(srcCit.IsEmpty(), "srcCit.IsEmpty()"); // its pointer
            
            StringList strs = new StringList("test");
            srcCit.Description = strs;
            
            strs = srcCit.Description;
            Assert.AreEqual("test", strs.Text);
        }

        private static void GEDCOMRepositoryCitationTest(GEDCOMSourceRecord sourRec, GEDCOMRepositoryRecord repRec)
        {
            GEDCOMRepositoryCitation repCit = sourRec.AddRepository(repRec);

            Assert.IsFalse(repCit.IsEmpty(), "repCit.IsEmpty()"); // its pointer
        }

        [Test]
        public void GEDCOMResearchRecord_Tests()
        {
            using (GEDCOMResearchRecord resRec = GEDCOMResearchRecord.Create(null, null, "", "") as GEDCOMResearchRecord) {

                resRec.ResetOwner(fContext.Tree);
                Assert.AreEqual(fContext.Tree, resRec.Owner);
            }
        }

        private static void GEDCOMResearchRecordTest(GEDCOMResearchRecord resRec, GEDCOMCommunicationRecord commRec, GEDCOMTaskRecord taskRec, GEDCOMGroupRecord groupRec)
        {
            Assert.IsNotNull(resRec.Communications);
            Assert.IsNotNull(resRec.Groups);
            Assert.IsNotNull(resRec.Tasks);
            
            resRec.ResearchName = "Test Research";
            Assert.AreEqual("Test Research", resRec.ResearchName);
            
            resRec.Priority = GKResearchPriority.rpNormal;
            Assert.AreEqual(GKResearchPriority.rpNormal, resRec.Priority);
            
            resRec.Status = GKResearchStatus.rsOnHold;
            Assert.AreEqual(GKResearchStatus.rsOnHold, resRec.Status);
            
            resRec.StartDate.Date = ParseDT("20.01.2013");
            Assert.AreEqual(ParseDT("20.01.2013"), resRec.StartDate.Date);
            
            resRec.StopDate.Date = ParseDT("21.01.2013");
            Assert.AreEqual(ParseDT("21.01.2013"), resRec.StopDate.Date);
            
            resRec.Percent = 33;
            Assert.AreEqual(33, resRec.Percent);

            resRec.DeleteTag(GEDCOMTagType._UID);
            resRec.DeleteTag(GEDCOMTagType.CHAN);
            string buf = TagStreamTest(resRec);
            Assert.AreEqual("0 @RS1@ _RESEARCH\r\n"+
                            "1 NAME Test Research\r\n"+
                            "1 _PRIORITY normal\r\n"+
                            "1 _STATUS onhold\r\n"+
                            "1 _STARTDATE 20 JAN 2013\r\n"+
                            "1 _STOPDATE 21 JAN 2013\r\n"+
                            "1 _PERCENT 33\r\n", buf);

            Assert.AreEqual(-1, resRec.IndexOfCommunication(null));
            resRec.AddCommunication(commRec);
            resRec.RemoveCommunication(commRec);
            resRec.RemoveCommunication(null);

            Assert.AreEqual(-1, resRec.IndexOfTask(null));
            resRec.AddTask(taskRec);
            resRec.RemoveTask(taskRec);
            resRec.RemoveTask(null);

            Assert.AreEqual(-1, resRec.IndexOfGroup(null));
            resRec.AddGroup(groupRec);
            resRec.RemoveGroup(groupRec);
            resRec.RemoveGroup(null);

            Assert.IsFalse(resRec.IsEmpty());
            resRec.Clear();
            Assert.IsTrue(resRec.IsEmpty());
        }

        [Test]
        public void GEDCOMRepositoryRecord_Tests()
        {
            using (GEDCOMRepositoryRecord repoRec = GEDCOMRepositoryRecord.Create(fContext.Tree, fContext.Tree, "", "") as GEDCOMRepositoryRecord)
            {
                Assert.IsNotNull(repoRec);

                repoRec.InitNew();
                repoRec.RepositoryName = "Test Repository";
                Assert.AreEqual("Test Repository", repoRec.RepositoryName);

                Assert.IsNotNull(repoRec.Address);

                repoRec.DeleteTag(GEDCOMTagType._UID);
                repoRec.DeleteTag(GEDCOMTagType.CHAN);
                string buf = TagStreamTest(repoRec);
                Assert.AreEqual("0 @R2@ REPO\r\n"+
                                "1 NAME Test Repository\r\n"+
                                "1 ADDR\r\n", buf);

                Assert.IsFalse(repoRec.IsEmpty());
                repoRec.Clear();
                Assert.IsTrue(repoRec.IsEmpty());
            }
        }

        [Test]
        public void GEDCOMMultimediaRecord_Tests()
        {
            using (GEDCOMMultimediaRecord mmRec = GEDCOMMultimediaRecord.Create(null, null, "", "") as GEDCOMMultimediaRecord)
            {
                Assert.IsNotNull(mmRec);

                mmRec.ResetOwner(fContext.Tree);
                Assert.AreEqual(fContext.Tree, mmRec.Owner);
            }
        }

        private static void GEDCOMMultimediaRecordTest(GEDCOMMultimediaRecord mediaRec, GEDCOMIndividualRecord indiv)
        {
            Assert.AreEqual("", mediaRec.GetFileTitle());

            mediaRec.AddTag(GEDCOMTagType.FILE, "", null);
            GEDCOMFileReferenceWithTitle fileRef = mediaRec.FileReferences[0];
            Assert.IsNotNull(fileRef);

            fileRef.Title = "File Title 2";
            Assert.AreEqual("File Title 2", fileRef.Title);

            fileRef.LinkFile("sample.png");
            fileRef.MediaType = GEDCOMMediaType.mtManuscript;
            Assert.AreEqual("sample.png", fileRef.StringValue);
            Assert.AreEqual(GEDCOMMultimediaFormat.mfPNG, fileRef.MultimediaFormat);
            Assert.AreEqual(GEDCOMMediaType.mtManuscript, fileRef.MediaType);

            string title = mediaRec.GetFileTitle();
            Assert.AreEqual("File Title 2", title);

            mediaRec.DeleteTag(GEDCOMTagType._UID);
            mediaRec.DeleteTag(GEDCOMTagType.CHAN);
            string buf = TagStreamTest(mediaRec);
            Assert.AreEqual("0 @O1@ OBJE\r\n"+
                            "1 FILE sample.png\r\n"+
                            "2 TITL File Title 2\r\n"+
                            "2 FORM png\r\n"+
                            "3 TYPE manuscript\r\n", buf);
            
            GEDCOMMultimediaLinkTest(mediaRec, indiv);
            
            Assert.IsFalse(mediaRec.IsEmpty());
            mediaRec.Clear();
            Assert.IsTrue(mediaRec.IsEmpty());
        }

        [Test]
        public void GEDCOMMultimediaLink_Tests()
        {
            using (GEDCOMMultimediaLink mmLink = GEDCOMMultimediaLink.Create(fContext.Tree, null, "", "") as GEDCOMMultimediaLink) {
                Assert.IsNotNull(mmLink);
                Assert.IsTrue(mmLink.IsEmpty());

                // extensions
                Assert.IsFalse(mmLink.IsPrimaryCutout);
                mmLink.IsPrimaryCutout = true;
                Assert.IsTrue(mmLink.IsPrimaryCutout);

                mmLink.CutoutPosition.Value = ExtRect.Create(10, 15, 500, 600);
                ExtRect rt = mmLink.CutoutPosition.Value;
                Assert.AreEqual(10, rt.Left);
                Assert.AreEqual(15, rt.Top);
                Assert.AreEqual(500, rt.Right);
                Assert.AreEqual(600, rt.Bottom);

                Assert.AreEqual(10, mmLink.CutoutPosition.X1);
                Assert.AreEqual(15, mmLink.CutoutPosition.Y1);
                Assert.AreEqual(500, mmLink.CutoutPosition.X2);
                Assert.AreEqual(600, mmLink.CutoutPosition.Y2);

                mmLink.CutoutPosition.X1 = 10;
                mmLink.CutoutPosition.Y1 = 10;
                mmLink.CutoutPosition.X2 = 300;
                mmLink.CutoutPosition.Y2 = 400;
                Assert.AreEqual(10, mmLink.CutoutPosition.X1);
                Assert.AreEqual(10, mmLink.CutoutPosition.Y1);
                Assert.AreEqual(300, mmLink.CutoutPosition.X2);
                Assert.AreEqual(400, mmLink.CutoutPosition.Y2);

                mmLink.CutoutPosition.ParseString("11 15 576 611");
                Assert.IsFalse(mmLink.CutoutPosition.IsEmpty());
                Assert.AreEqual("11 15 576 611", mmLink.CutoutPosition.StringValue);

                using (var mmRec = (GEDCOMMultimediaRecord)GEDCOMMultimediaRecord.Create(fContext.Tree, fContext.Tree, "", "")) {
                    Assert.IsNull(mmLink.GetUID());

                    mmLink.Value = mmRec;

                    Assert.IsNotNull(mmLink.GetUID());
                }

                mmLink.CutoutPosition.Clear();
                Assert.IsTrue(mmLink.CutoutPosition.IsEmpty());
                Assert.AreEqual("", mmLink.CutoutPosition.StringValue);

                mmLink.ResetOwner(fContext.Tree);
                Assert.AreEqual(fContext.Tree, mmLink.Owner);
            }
        }

        private static void GEDCOMMultimediaLinkTest(GEDCOMMultimediaRecord mediaRec, GEDCOMIndividualRecord indiv)
        {
            GEDCOMMultimediaLink mmLink = indiv.AddMultimedia(mediaRec);

            Assert.IsNotNull(mmLink.FileReferences);

            mmLink.Title = "Title1";
            Assert.AreEqual("Title1", mmLink.Title);

            string buf = TagStreamTest(mmLink);
            Assert.AreEqual("1 OBJE @O1@\r\n"+
                            "2 TITL Title1\r\n", buf);

            Assert.IsTrue(mmLink.IsPointer, "mmLink.IsPointer");

            mmLink.IsPrimary = true;
            Assert.IsTrue(mmLink.IsPrimary, "mmLink.IsPrimary");

            Assert.IsFalse(mmLink.IsEmpty(), "mmLink.IsEmpty()"); // its pointer

            mmLink.Clear();
        }

        private static void GEDCOMSubmissionRecordTest(GEDCOMSubmissionRecord submRec, string submitterXRef)
        {
            submRec.FamilyFileName = "FamilyFileName";
            Assert.AreEqual("FamilyFileName", submRec.FamilyFileName);

            submRec.TempleCode = "TempleCode";
            Assert.AreEqual("TempleCode", submRec.TempleCode);

            submRec.GenerationsOfAncestors = 11;
            Assert.AreEqual(11, submRec.GenerationsOfAncestors);

            submRec.GenerationsOfDescendants = 77;
            Assert.AreEqual(77, submRec.GenerationsOfDescendants);

            submRec.OrdinanceProcessFlag = GEDCOMOrdinanceProcessFlag.opYes;
            Assert.AreEqual(GEDCOMOrdinanceProcessFlag.opYes, submRec.OrdinanceProcessFlag);
            
            submRec.AddTag(GEDCOMTagType.SUBM, GEDCOMUtils.EncloseXRef(submitterXRef), null);
            GEDCOMSubmitterRecord subr = submRec.Submitter.Value as GEDCOMSubmitterRecord;
            Assert.IsNotNull(subr);
            
            
            Assert.IsFalse(submRec.IsEmpty());
            submRec.Clear();
            Assert.IsTrue(submRec.IsEmpty());
        }

        [Test]
        public void GEDCOMSubmitterRecord_Tests()
        {
            using (GEDCOMSubmitterRecord subrRec = GEDCOMSubmitterRecord.Create(null, null, "", "") as GEDCOMSubmitterRecord) {
                subrRec.Name.StringValue = "Test Submitter";
                Assert.AreEqual("Test Submitter", subrRec.Name.StringValue);

                subrRec.RegisteredReference = "regref";
                Assert.AreEqual("regref", subrRec.RegisteredReference);

                subrRec.AddTag(GEDCOMTagType.LANG, "Russian", null);
                Assert.AreEqual("Russian", subrRec.Languages[0].StringValue);

                subrRec.SetLanguage(0, "nothing"); // return without exceptions

                subrRec.SetLanguage(1, "English");
                Assert.AreEqual("English", subrRec.Languages[1].StringValue);

                Assert.IsNotNull(subrRec.Address);


                Assert.IsFalse(subrRec.IsEmpty());
                subrRec.Clear();
                Assert.IsTrue(subrRec.IsEmpty());


                subrRec.ResetOwner(fContext.Tree);
                Assert.AreEqual(fContext.Tree, subrRec.Owner);
            }
        }

        [Test]
        public void GEDCOMCommunicationRecord_Test()
        {
            GEDCOMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I1") as GEDCOMIndividualRecord;
            Assert.IsNotNull(iRec);

            using (GEDCOMCommunicationRecord comRec = GEDCOMCommunicationRecord.Create(fContext.Tree, fContext.Tree, "", "") as GEDCOMCommunicationRecord) {
                comRec.CommName = "Test Communication";
                Assert.AreEqual("Test Communication", comRec.CommName);

                comRec.CommunicationType = GKCommunicationType.ctFax;
                Assert.AreEqual(GKCommunicationType.ctFax, comRec.CommunicationType);

                comRec.Date.Date = ParseDT("23.01.2013");
                Assert.AreEqual(ParseDT("23.01.2013"), comRec.Date.Date);

                comRec.SetCorresponder(GKCommunicationDir.cdFrom, iRec);

                var corr = comRec.GetCorresponder();
                Assert.AreEqual(GKCommunicationDir.cdFrom, corr.CommDir);
                Assert.AreEqual(iRec, corr.Corresponder);

                comRec.SetCorresponder(GKCommunicationDir.cdTo, iRec);
                corr = comRec.GetCorresponder();
                Assert.AreEqual(GKCommunicationDir.cdTo, corr.CommDir);
                Assert.AreEqual(iRec, corr.Corresponder);

                Assert.IsFalse(comRec.IsEmpty());
                comRec.Clear();
                Assert.IsTrue(comRec.IsEmpty());
            }
        }

        [Test]
        public void GEDCOMLocationRecord_Test()
        {
            using (GEDCOMLocationRecord locRec = GEDCOMLocationRecord.Create(null, null, "", "") as GEDCOMLocationRecord) {
                locRec.LocationName = "Test Location";
                Assert.AreEqual("Test Location", locRec.LocationName);

                Assert.IsNotNull(locRec.Map);

                Assert.IsFalse(locRec.IsEmpty());
                locRec.Clear();
                Assert.IsTrue(locRec.IsEmpty());
            }
        }

        private static DateTime ParseDT(string dtx)
        {
            return DateTime.ParseExact(dtx, "dd.MM.yyyy", CultureInfo.InvariantCulture);
        }

        [Test]
        public void GEDCOMTaskRecord_Tests()
        {
            GEDCOMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I1") as GEDCOMIndividualRecord;
            Assert.IsNotNull(iRec);

            GEDCOMFamilyRecord famRec = fContext.Tree.XRefIndex_Find("F1") as GEDCOMFamilyRecord;
            Assert.IsNotNull(famRec);

            GEDCOMSourceRecord srcRec = fContext.Tree.XRefIndex_Find("S1") as GEDCOMSourceRecord;
            Assert.IsNotNull(srcRec);

            using (GEDCOMTaskRecord taskRec = GEDCOMTaskRecord.Create(fContext.Tree, fContext.Tree, "", "") as GEDCOMTaskRecord)
            {
                Assert.IsNotNull(taskRec);

                taskRec.Priority = GKResearchPriority.rpNormal;
                Assert.AreEqual(GKResearchPriority.rpNormal, taskRec.Priority);

                taskRec.StartDate.Date = ParseDT("20.01.2013");
                Assert.AreEqual(ParseDT("20.01.2013"), taskRec.StartDate.Date);

                taskRec.StopDate.Date = ParseDT("21.01.2013");
                Assert.AreEqual(ParseDT("21.01.2013"), taskRec.StopDate.Date);

                taskRec.Goal = "Test Goal";
                Assert.AreEqual("Test Goal", taskRec.Goal);
                var goal = taskRec.GetTaskGoal();
                Assert.AreEqual(GKGoalType.gtOther, goal.GoalType);
                Assert.AreEqual(null, goal.GoalRec);

                taskRec.Goal = iRec.XRef;
                goal = taskRec.GetTaskGoal();
                Assert.AreEqual(GKGoalType.gtIndividual, goal.GoalType);
                Assert.AreEqual(iRec, goal.GoalRec);

                taskRec.Goal = famRec.XRef;
                goal = taskRec.GetTaskGoal();
                Assert.AreEqual(GKGoalType.gtFamily, goal.GoalType);
                Assert.AreEqual(famRec, goal.GoalRec);

                taskRec.Goal = srcRec.XRef;
                goal = taskRec.GetTaskGoal();
                Assert.AreEqual(GKGoalType.gtSource, goal.GoalType);
                Assert.AreEqual(srcRec, goal.GoalRec);

                Assert.IsFalse(taskRec.IsEmpty());
                taskRec.Clear();
                Assert.IsTrue(taskRec.IsEmpty());
            }
        }

        [Test]
        public void GEDCOMNotes_Tests()
        {
            using (GEDCOMNotes notes = GEDCOMNotes.Create(null, null, "", "") as GEDCOMNotes) {
                Assert.IsTrue(notes.IsEmpty());
                notes.Notes = new StringList("Test note");
                Assert.IsFalse(notes.IsEmpty());
                Assert.AreEqual("Test note", notes.Notes.Text);
            }
        }

        [Test]
        public void GEDCOMNoteRecord_Tests()
        {
            using (GEDCOMNoteRecord noteRec = GEDCOMNoteRecord.Create(null, null, "", "") as GEDCOMNoteRecord) {
                noteRec.AddNoteText("text");
                Assert.AreEqual("text", noteRec.Note.Text.Trim());

                Assert.Throws(typeof(ArgumentNullException), () => { noteRec.SetNoteText(null); });

                noteRec.SetNoteText("Test text");
                Assert.AreEqual("Test text", noteRec.Note.Text.Trim());

                using (GEDCOMNoteRecord noteRec2 = GEDCOMNoteRecord.Create(null, null, "", "") as GEDCOMNoteRecord) {
                    noteRec2.SetNoteText("Test text");
                    Assert.AreEqual("Test text", noteRec2.Note.Text.Trim());

                    Assert.AreEqual(100.0f, noteRec.IsMatch(noteRec2, new MatchParams()));

                    Assert.IsFalse(noteRec2.IsEmpty());
                    noteRec2.Clear();
                    Assert.IsTrue(noteRec2.IsEmpty());

                    Assert.AreEqual(0.0f, noteRec.IsMatch(noteRec2, new MatchParams()));

                    Assert.AreEqual(0.0f, noteRec.IsMatch(null, new MatchParams()));
                }

                Assert.Throws(typeof(ArgumentException), () => { noteRec.MoveTo(null, false); });

                using (GEDCOMNoteRecord noteRec3 = GEDCOMNoteRecord.Create(null, null, "", "") as GEDCOMNoteRecord) {
                    noteRec3.SetNoteText("Test text 3");
                    Assert.AreEqual("Test text 3", noteRec3.Note.Text.Trim());

                    noteRec.MoveTo(noteRec3, false);

                    Assert.AreEqual("Test text 3", noteRec3.Note.Text.Trim());
                }
            }
        }

        private static void GEDCOMNoteRecordTest(GEDCOMNoteRecord noteRec, GEDCOMIndividualRecord indiv)
        {
            noteRec.SetNotesArray(new string[] { "This", "notes", "test" });
            
            string ctx = GKUtils.MergeStrings(noteRec.Note);
            Assert.AreEqual("This notes test", ctx);

            noteRec.Note = new StringList("This\r\nnotes2\r\ntest2");
            Assert.AreEqual("This", noteRec.Note[0]);
            Assert.AreEqual("notes2", noteRec.Note[1]);
            Assert.AreEqual("test2", noteRec.Note[2]);
            
            Assert.Throws(typeof(ArgumentNullException), () => { GKUtils.MergeStrings(null); });
            
            ctx = GKUtils.MergeStrings(noteRec.Note);
            Assert.AreEqual("This notes2 test2", ctx);
            
            noteRec.Clear();
            noteRec.AddNoteText("Test text");
            Assert.AreEqual("Test text", noteRec.Note.Text.Trim());
            
            GEDCOMNotesTest(noteRec, indiv);

            Assert.IsFalse(noteRec.IsEmpty());
            noteRec.Clear();
            Assert.IsTrue(noteRec.IsEmpty());
        }

        private static void GEDCOMNotesTest(GEDCOMNoteRecord noteRec, GEDCOMIndividualRecord indiv)
        {
            GEDCOMNotes notes = indiv.AddNote(noteRec);
            
            Assert.AreEqual(notes.Notes.Text, noteRec.Note.Text);
            
            Assert.IsTrue(notes.IsPointer, "notes.IsPointer");
            
            Assert.IsFalse(notes.IsEmpty()); // its pointer
            
            notes.Clear();
        }
        
        #endregion
        
        #region Private Aux functions
        
        private static string TagStreamTest(GEDCOMTag tag)
        {
            string result;
            
            using (MemoryStream stm = new MemoryStream()) {
                using (StreamWriter fs = new StreamWriter(stm)) {
                    tag.SaveToStream(fs);
                    
                    fs.Flush();
                    
                    result = Encoding.ASCII.GetString(stm.ToArray());
                }
            }
            
            return result;
        }
        
        #endregion

        [Test]
        public void Standart_Tests()
        {
            Assembly assembly = typeof(CoreTests).Assembly;
            using (Stream inStream = assembly.GetManifestResourceStream("GKTests.Resources.TGC55CLF.GED")) {
                using (GEDCOMTree tree = new GEDCOMTree()) {
                    var gedcomProvider = new GEDCOMProvider(tree);
                    gedcomProvider.LoadFromStreamExt(inStream, inStream);

                    using (MemoryStream outStream = new MemoryStream()) {
                        gedcomProvider = new GEDCOMProvider(tree);
                        gedcomProvider.SaveToStreamExt(outStream, GEDCOMCharacterSet.csASCII);
                    }
                }
            }
        }

        #region GEDCOM Enums test

        private class GEDCOMMediaTypeEnum : GEDCOMEnumHelper<GEDCOMMediaType>
        {
            private static string[] XMediaTypeArr = new string[] {
                "", "audio", "book", "card", "electronic", "fiche", "film", "magazine",
                "manuscript", "map", "newspaper", "photo", "tombstone", "video", "-1" };

            protected GEDCOMMediaTypeEnum() : base(XMediaTypeArr, GEDCOMMediaType.mtUnknown, false)
            {
            }

            public static readonly GEDCOMMediaTypeEnum Instance = new GEDCOMMediaTypeEnum();
        }

        [Test]
        public void GEDCOMEnumParse_Tests()
        {
            Assert.IsNotNull(GEDCOMMediaTypeEnum.Instance);

            string strVal3 = GEDCOMMediaTypeEnum.Instance.GetStrValue((GEDCOMMediaType) 15);
            Assert.AreEqual("", strVal3);

            strVal3 = GEDCOMMediaTypeEnum.Instance.GetStrValue(GEDCOMMediaType.mtMagazine);
            Assert.AreEqual("magazine", strVal3);

            GEDCOMMediaType mt3 = GEDCOMMediaTypeEnum.Instance.GetEnumValue(strVal3);
            Assert.AreEqual(GEDCOMMediaType.mtMagazine, mt3);

            mt3 = GEDCOMMediaTypeEnum.Instance.GetEnumValue("test");
            Assert.AreEqual(GEDCOMMediaType.mtUnknown, mt3);

            // performance test
            /*Random rnd = new Random();
            for (int k = 0; k < 100000; k++) {
                string strVal1, strVal2, strVal3;

                int i = rnd.Next(1, 13);

                GEDCOMMediaType mt = (GEDCOMMediaType)i;
                strVal1 = GEDCOMUtils.GetMediaTypeStr(mt);
                strVal2 = Enum2Str(mt, MediaTypeArr); // slower for 1.2 ms
                strVal3 = mediaEnumHelper.GetStrValue(mt); // slower for 1.4 ms
                Assert.AreEqual(strVal1, strVal2);
                Assert.AreEqual(strVal2, strVal3);

                strVal1 = MediaTypeArr[i];
                GEDCOMMediaType mt1 = GEDCOMUtils.GetMediaTypeVal(strVal1);
                GEDCOMMediaType mt2 = (GEDCOMMediaType)Str2Enum(strVal1, MediaTypeArr, (int)GEDCOMMediaType.mtUnknown); // slower for 23 ms
                GEDCOMMediaType mt3 = (GEDCOMMediaType)mediaEnumHelper.GetEnumValue(strVal1); // faster for 114 ms
                Assert.AreEqual(mt1, mt2);
                Assert.AreEqual(mt2, mt3);
            }*/
        }

        #region Methods only for the test

        public static int Str2Enum(string val, string[] values, int defVal)
        {
            if (string.IsNullOrEmpty(val)) return defVal;

            val = val.Trim().ToLower(CultureInfo.InvariantCulture);
            for (int i = 0; i < values.Length; i++) {
                if (string.Equals(values[i], val)) {
                    return i;
                }
            }

            return defVal;
        }

        public static string Enum2Str(IConvertible elem, string[] values)
        {
            int idx = (int)elem;
            if (idx < 0 || idx >= values.Length) {
                return string.Empty;
            } else {
                return values[idx];
            }
        }

        #endregion

        #endregion
    }
}
