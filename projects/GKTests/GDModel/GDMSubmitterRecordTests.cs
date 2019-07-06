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
using GDModel;
using GKCore;
using GKTests;
using NUnit.Framework;

namespace GDModel
{
    [TestFixture]
    public class GDMSubmitterRecordTests
    {
        private BaseContext fContext;

        [TestFixtureSetUp]
        public void SetUp()
        {
            fContext = TestUtils.CreateContext();
            TestUtils.FillContext(fContext);
        }

        [Test]
        public void Test_Common()
        {
            using (GDMSubmitterRecord subrRec = new GDMSubmitterRecord(fContext.Tree)) {
                subrRec.Name.StringValue = "Test Submitter";
                Assert.AreEqual("Test Submitter", subrRec.Name.StringValue);

                subrRec.RegisteredReference = "regref";
                Assert.AreEqual("regref", subrRec.RegisteredReference);

                var lang = new GDMLanguage(subrRec);
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


                subrRec.ResetOwner(fContext.Tree);
                Assert.AreEqual(fContext.Tree, subrRec.GetTree());
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
