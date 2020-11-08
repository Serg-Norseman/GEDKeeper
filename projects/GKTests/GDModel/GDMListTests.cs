/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2020 by Sergey V. Zhdanovskih.
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

using GDModel;
using GKCore;
using GKTests;
using GKUI;
using NUnit.Framework;

namespace GDModel
{
    [TestFixture]
    public class GDMListTests
    {
        private BaseContext fContext;

        [TestFixtureSetUp]
        public void SetUp()
        {
            WFAppHost.ConfigureBootstrap(false);

            LangMan.DefInit();

            fContext = TestUtils.CreateContext();
            TestUtils.FillContext(fContext);
        }

        [TestFixtureTearDown]
        public void TearDown()
        {
        }

        [Test]
        public void Test_Common()
        {
            GDMTag obj1 = new GDMTag(null);
            GDMTag obj2 = new GDMTag(null);

            using (GDMList<GDMTag> list = new GDMList<GDMTag>(null)) {
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

                foreach (GDMObject obj in list) {
                }
            }
        }

        [Test]
        public void Test_PerfCommon()
        {
            GDMIndividualRecord iRec = fContext.Tree.XRefIndex_Find("I1") as GDMIndividualRecord;
            Assert.IsNotNull(iRec);

            for (int k = 0; k < REP_COUNT; k++) {
                GEDCOMListTest11(iRec);
                GEDCOMListTest12(iRec);
                GEDCOMListTest21(iRec);
                GEDCOMListTest22(iRec);
                GEDCOMListTest23(iRec);
            }
        }

        private const int REP_COUNT = 1000; // 1000000; // for profile tests

        private static void GEDCOMListTest11(GDMIndividualRecord iRec)
        {
            int hash;
            foreach (GDMCustomEvent evt1 in iRec.Events) {
                hash = evt1.GetHashCode();
            }
        }

        private static void GEDCOMListTest12(GDMIndividualRecord iRec)
        {
            int hash;
            IGEDCOMListEnumerator<GDMCustomEvent> enumer = iRec.Events.GetEnumerator();
            enumer.Reset();
            while (enumer.MoveNext()) {
                GDMCustomEvent evt1 = enumer.Current;
                hash = evt1.GetHashCode();
            }
        }

        private static void GEDCOMListTest21(GDMIndividualRecord iRec)
        {
            int hash;
            for (int i = 0; i < iRec.Events.Count; i++) {
                GDMCustomEvent evt1 = iRec.Events[i];
                hash = evt1.GetHashCode();
            }
        }

        private static void GEDCOMListTest22(GDMIndividualRecord iRec)
        {
            int hash;
            for (int i = 0, num = iRec.Events.Count; i < num; i++) {
                GDMCustomEvent evt1 = iRec.Events[i];
                hash = evt1.GetHashCode();
            }
        }

        private static void GEDCOMListTest23(GDMIndividualRecord iRec)
        {
            int hash;
            GDMList<GDMCustomEvent> events = iRec.Events;
            for (int i = 0, num = events.Count; i < num; i++) {
                GDMCustomEvent evt1 = events[i];
                hash = evt1.GetHashCode();
            }
        }
    }
}
