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

using GKCommon.GEDCOM;
using NUnit.Framework;

namespace GKCommon.GEDCOM
{
    [TestFixture]
    public class GEDCOMProviderTests
    {
        [Test]
        public void Test_GetGEDCOMFormat()
        {
            GEDCOMTree tree = new GEDCOMTree();

            // Tests of determine GEDCOM-format
            Assert.AreEqual(GEDCOMFormat.gf_Unknown, GEDCOMProvider.GetGEDCOMFormat(tree));
            tree.Header.Source = "GENBOX";
            Assert.AreEqual(GEDCOMFormat.gf_GENBOX, GEDCOMProvider.GetGEDCOMFormat(tree));
        }

        [Test]
        public void Test_GetTagProps()
        {
            TagProperties props = GEDCOMProvider.GetTagProps(GEDCOMTagType.ADDR);
            Assert.IsNotNull(props);
            Assert.IsTrue(props.SkipEmpty);

            props = GEDCOMProvider.GetTagProps("test");
            Assert.IsNull(props);
        }

        [Test]
        public void Test_GEDCOMPointer()
        {
            using (var ptr = new GEDCOMPointer(null, null, "", "")) {
                string remainder = ptr.ParseString("  @I1111@ test");
                Assert.AreEqual("I1111", ptr.XRef);
                Assert.AreEqual(" test", remainder);

                remainder = ptr.ParseString("  @#I1111@ test21");
                Assert.AreEqual("", ptr.XRef);
                Assert.AreEqual("@#I1111@ test21", remainder);

                remainder = ptr.ParseString("    test2");
                Assert.AreEqual("", ptr.XRef);
                Assert.AreEqual("test2", remainder);

                remainder = ptr.ParseString("    ");
                Assert.AreEqual("", ptr.XRef);
                Assert.AreEqual("", remainder);

                remainder = ptr.ParseString("");
                Assert.AreEqual("", ptr.XRef);
                Assert.AreEqual("", remainder);
            }
        }

        [Test]
        public void Test_GEDCOMTime()
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
        public void Test_ParseCutoutPosition_Perf()
        {
            string str = "150 150 1000 1000";
            int x1, y1, x2, y2;
            for (int i = 0; i < 10000; i++) {
                GEDCOMUtils.ParseCutoutPosition(str, out x1, out y1, out x2, out y2); // x4 faster!
                GEDCOMUtils.ParseCutoutPosition_old(str, out x1, out y1, out x2, out y2);
            }
        }*/

        [Test]
        public void Test_ParseTag()
        {
            string str;
            int tagLevel2, res2;
            string tagXRef2, tagName2, tagValue2;

            str = "0 HEAD";
            res2 = GEDCOMUtils.ParseTag(str, out tagLevel2, out tagXRef2, out tagName2, out tagValue2);
            Assert.AreEqual(0, tagLevel2);
            Assert.AreEqual("", tagXRef2);
            Assert.AreEqual("HEAD", tagName2);
            Assert.AreEqual("", tagValue2);
            Assert.AreEqual(2, res2);

            str = "0 @SUB1@ SUBM";
            res2 = GEDCOMUtils.ParseTag(str, out tagLevel2, out tagXRef2, out tagName2, out tagValue2);
            Assert.AreEqual(0, tagLevel2);
            Assert.AreEqual("SUB1", tagXRef2);
            Assert.AreEqual("SUBM", tagName2);
            Assert.AreEqual("", tagValue2);
            Assert.AreEqual(3, res2);

            str = "0 @SUB1@ SUBM testVal";
            res2 = GEDCOMUtils.ParseTag(str, out tagLevel2, out tagXRef2, out tagName2, out tagValue2);
            Assert.AreEqual(0, tagLevel2);
            Assert.AreEqual("SUB1", tagXRef2);
            Assert.AreEqual("SUBM", tagName2);
            Assert.AreEqual("testVal", tagValue2);
            Assert.AreEqual(4, res2);

            str = "1 SUBM @SUB1@";
            res2 = GEDCOMUtils.ParseTag(str, out tagLevel2, out tagXRef2, out tagName2, out tagValue2);
            Assert.AreEqual(1, tagLevel2);
            Assert.AreEqual("", tagXRef2);
            Assert.AreEqual("SUBM", tagName2);
            Assert.AreEqual("@SUB1@", tagValue2);
            Assert.AreEqual(3, res2);

            str = "    1 SUBM @SUB1@";
            res2 = GEDCOMUtils.ParseTag(str, out tagLevel2, out tagXRef2, out tagName2, out tagValue2);
            Assert.AreEqual(1, tagLevel2);
            Assert.AreEqual("", tagXRef2);
            Assert.AreEqual("SUBM", tagName2);
            Assert.AreEqual("@SUB1@", tagValue2);
            Assert.AreEqual(3, res2);

            str = "2 DATE FROM 20 JAN 1979 TO 15 MAY 2012";
            res2 = GEDCOMUtils.ParseTag(str, out tagLevel2, out tagXRef2, out tagName2, out tagValue2);
            Assert.AreEqual(2, tagLevel2);
            Assert.AreEqual("", tagXRef2);
            Assert.AreEqual("DATE", tagName2);
            Assert.AreEqual("FROM 20 JAN 1979 TO 15 MAY 2012", tagValue2);
            Assert.AreEqual(3, res2);


            str = "    test test test (FTB line with error)";
            res2 = GEDCOMUtils.ParseTag(str, out tagLevel2, out tagXRef2, out tagName2, out tagValue2);
            Assert.AreEqual("    test test test (FTB line with error)", tagValue2);
            Assert.AreEqual(-1, res2);

            str = "        ";
            res2 = GEDCOMUtils.ParseTag(str, out tagLevel2, out tagXRef2, out tagName2, out tagValue2);
            Assert.AreEqual(0, tagLevel2);
            Assert.AreEqual("", tagXRef2);
            Assert.AreEqual("", tagName2);
            Assert.AreEqual("", tagValue2);
            Assert.AreEqual(-2, res2);

            str = "";
            res2 = GEDCOMUtils.ParseTag(str, out tagLevel2, out tagXRef2, out tagName2, out tagValue2);
            Assert.AreEqual(-2, res2);
        }

        [Test]
        public void Test_CtorDyn()
        {
            var uref = GEDCOMFactory.CreateTagEx<GEDCOMUserReference>(null, null, "", "test 12345");
            Assert.IsNotNull(uref);
            Assert.AreEqual("test 12345", uref.StringValue);
        }
    }
}
