/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore;
using GKCore.Utilities;
using GKTests;
using NUnit.Framework;

namespace GDModel
{
    [TestFixture]
    public class GDMListTests
    {
        private readonly BaseContext fContext;

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
            var iRec = fContext.Tree.FindXRef<GDMIndividualRecord>("I1");
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
            var iRec = fContext.Tree.FindXRef<GDMIndividualRecord>("I1");
            Assert.IsNotNull(iRec);

            int hash;
            var enumer = iRec.Events.GetEnumerator();
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
            var iRec = fContext.Tree.FindXRef<GDMIndividualRecord>("I1");
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
