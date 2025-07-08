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
