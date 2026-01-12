/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore;
using GKTests;
using NUnit.Framework;

namespace GDModel
{
    [TestFixture]
    public class GDMSubmitterRecordTests
    {
        private readonly BaseContext fContext;

        public GDMSubmitterRecordTests()
        {
            TestUtils.InitGEDCOMProviderTest();

            fContext = TestUtils.CreateContext();
            TestUtils.FillContext(fContext);
        }

        [Test]
        public void Test_Common()
        {
            using (GDMSubmitterRecord subrRec = new GDMSubmitterRecord(fContext.Tree)) {
                subrRec.Name = "Test Submitter";
                Assert.AreEqual("Test Submitter", subrRec.Name);

                subrRec.RegisteredReference = "regref";
                Assert.AreEqual("regref", subrRec.RegisteredReference);

                var lang = new GDMLanguage();
                lang.ParseString("Russian");
                subrRec.Languages.Add(lang);
                Assert.AreEqual("Russian", subrRec.Languages[0].StringValue);

                subrRec.SetLanguage(0, "nothing"); // return without exceptions

                subrRec.SetLanguage(1, "English");
                Assert.AreEqual("English", subrRec.Languages[1].StringValue);

                Assert.IsNotNull(subrRec.Address);

                subrRec.ReplaceXRefs(new GDMXRefReplacer());


                Assert.IsFalse(subrRec.IsEmpty());
                subrRec.Clear();
                Assert.IsTrue(subrRec.IsEmpty());


                subrRec.ResetTree(fContext.Tree);
            }
        }

        [Test]
        public void Test_SetLanguage()
        {
            var inst = new GDMSubmitterRecord(fContext.Tree);

            inst.SetLanguage(-1, "nothing test");
            Assert.AreEqual(0, inst.Languages.Count);

            inst.SetLanguage(0, "test");
            Assert.AreEqual(1, inst.Languages.Count);
        }
    }
}
