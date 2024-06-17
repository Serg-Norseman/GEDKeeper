/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
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

using GKCore;
using GKTests;
using NUnit.Framework;

namespace GDModel
{
    [TestFixture]
    public class GDMListTests
    {
        private BaseContext fContext;

        public GDMListTests()
        {
            TestUtils.InitGEDCOMProviderTest();
            fContext = TestUtils.CreateContext();
            TestUtils.FillContext(fContext);
        }

        [Test]
        public void Test_Common()
        {
            GDMTag obj1 = new GDMTag();
            GDMTag obj2 = new GDMTag();

            using (var list = new GDMList<GDMTag>()) {
                // internal list is null (all routines instant returned)
                list.Remove(null);
                list.Exchange(0, 1);
                Assert.IsNull(list.Extract(0));
                Assert.IsNull(list.Extract(null));

                // normal checks
                list.Add(obj1);
                list.Add(obj2);
                Assert.AreEqual(0, list.IndexOf(obj1));
                Assert.AreEqual(1, list.IndexOf(obj2));

                using (var list2 = new GDMList<GDMTag>()) {
                    list2.AddRange(list);
                    Assert.AreEqual(2, list2.Count);
                }

                list.Remove(obj1);
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
            }
        }

        [Test]
        public void Test_ForeachEnum()
        {
            var iRec = fContext.Tree.XRefIndex_Find("I1") as GDMIndividualRecord;
            Assert.IsNotNull(iRec);

            int hash;
            foreach (GDMCustomEvent evt1 in iRec.Events) {
                hash = evt1.GetHashCode();
                SysUtils.DoNotInline(hash);
            }
        }

        [Test]
        public void Test_WhileEnum()
        {
            var iRec = fContext.Tree.XRefIndex_Find("I1") as GDMIndividualRecord;
            Assert.IsNotNull(iRec);

            int hash;
            IGDMListEnumerator<GDMCustomEvent> enumer = iRec.Events.GetEnumerator();
            enumer.Reset();
            while (enumer.MoveNext()) {
                GDMCustomEvent evt1 = enumer.Current;
                hash = evt1.GetHashCode();
                SysUtils.DoNotInline(hash);
            }
        }

        [Test]
        public void Test_For()
        {
            var iRec = fContext.Tree.XRefIndex_Find("I1") as GDMIndividualRecord;
            Assert.IsNotNull(iRec);

            int hash;
            for (int i = 0; i < iRec.Events.Count; i++) {
                GDMCustomEvent evt1 = iRec.Events[i];
                hash = evt1.GetHashCode();
                SysUtils.DoNotInline(hash);
            }
        }
    }
}
