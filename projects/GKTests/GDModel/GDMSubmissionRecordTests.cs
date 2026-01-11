// Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
// This file is part of GEDKeeper, licensed under the GNU GPL v3.
// See LICENSE file in the project root for full license information.

using GKCore;
using GKTests;
using NUnit.Framework;

namespace GDModel
{
    [TestFixture]
    public class GDMSubmissionRecordTests
    {
        private readonly BaseContext fContext;

        public GDMSubmissionRecordTests()
        {
            TestUtils.InitGEDCOMProviderTest();

            fContext = TestUtils.CreateContext();
            TestUtils.FillContext(fContext);
        }

        [Test]
        public void Test_Common()
        {
            GDMSubmissionRecord submRec = fContext.Tree.AddRecord(new GDMSubmissionRecord(fContext.Tree)) as GDMSubmissionRecord;
            GDMRecord sbmrRec = fContext.Tree.AddRecord(new GDMSubmitterRecord(fContext.Tree));
            fContext.Tree.NewXRef(sbmrRec);
            string submitterXRef = sbmrRec.XRef;

            submRec.FamilyFileName = "FamilyFileName";
            Assert.AreEqual("FamilyFileName", submRec.FamilyFileName);

            submRec.TempleCode = "TempleCode";
            Assert.AreEqual("TempleCode", submRec.TempleCode);

            submRec.GenerationsOfAncestors = 11;
            Assert.AreEqual(11, submRec.GenerationsOfAncestors);

            submRec.GenerationsOfDescendants = 77;
            Assert.AreEqual(77, submRec.GenerationsOfDescendants);

            submRec.OrdinanceProcessFlag = GDMOrdinanceProcessFlag.opYes;
            Assert.AreEqual(GDMOrdinanceProcessFlag.opYes, submRec.OrdinanceProcessFlag);

            submRec.Submitter.XRef = submitterXRef;
            GDMSubmitterRecord subr = fContext.Tree.GetPtrValue<GDMSubmitterRecord>(submRec.Submitter);
            Assert.IsNotNull(subr);


            submRec.ReplaceXRefs(new GDMXRefReplacer());

            Assert.IsFalse(submRec.IsEmpty());
            submRec.Clear();
            Assert.IsTrue(submRec.IsEmpty());
        }
    }
}
