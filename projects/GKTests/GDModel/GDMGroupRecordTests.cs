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
    public class GDMGroupRecordTests
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
            using (GDMGroupRecord grpRec = new GDMGroupRecord(fContext.Tree)) {
                Assert.IsNotNull(grpRec);

                grpRec.ResetOwner(fContext.Tree);
                Assert.AreEqual(fContext.Tree, grpRec.GetTree());
            }

            using (GDMGroupRecord groupRec = fContext.Tree.CreateGroup()) {
                GDMIndividualRecord member = fContext.Tree.XRefIndex_Find("I1") as GDMIndividualRecord;

                groupRec.GroupName = "Test Group";
                Assert.AreEqual("Test Group", groupRec.GroupName);

                string buf = TestUtils.GetTagStreamText(groupRec, 0);
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
    }
}
