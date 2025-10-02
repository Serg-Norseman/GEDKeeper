/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
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
using GDModel.Providers.GEDCOM;
using GKCore;
using GKTests;
using NUnit.Framework;

namespace GDModel
{
    [TestFixture]
    public class GDMGroupRecordTests
    {
        private readonly BaseContext fContext;

        public GDMGroupRecordTests()
        {
            TestUtils.InitGEDCOMProviderTest();
            fContext = TestUtils.CreateContext();
            TestUtils.FillContext(fContext);
        }

        [Test]
        public void Test_Common()
        {
            using (GDMGroupRecord grpRec = new GDMGroupRecord(fContext.Tree)) {
                Assert.IsNotNull(grpRec);

                grpRec.ResetTree(fContext.Tree);
            }

            using (GDMGroupRecord groupRec = fContext.Tree.CreateGroup()) {
                GDMIndividualRecord member = fContext.Tree.FindXRef<GDMIndividualRecord>("I1");

                groupRec.GroupName = "Test Group";
                Assert.AreEqual("Test Group", groupRec.GroupName);

                using (GDMGroupRecord group3 = fContext.Tree.CreateGroup()) {
                    var matchParams = new MatchParams();
                    matchParams.IndistinctThreshold = 100.0f;

                    Assert.AreEqual(0.0f, groupRec.IsMatch(null, matchParams));

                    group3.GroupName = "Test group";
                    Assert.AreEqual(100.0f, groupRec.IsMatch(group3, matchParams));

                    group3.GroupName = "test";
                    Assert.AreEqual(0.0f, groupRec.IsMatch(group3, matchParams));
                }

                bool res = groupRec.AddMember(null);
                Assert.IsFalse(res);

                res = groupRec.RemoveMember(null);
                Assert.IsFalse(res);

                Assert.AreEqual(-1, groupRec.IndexOfMember(null));

                groupRec.AddMember(member);
                Assert.AreEqual(0, groupRec.IndexOfMember(member));

                using (var group2 = fContext.Tree.CreateGroup()) {
                    Assert.Throws(typeof(ArgumentException), () => {
                        group2.Assign(null);
                    });

                    group2.Assign(groupRec);

                    string buf = GEDCOMProvider.GetTagStreamText(group2, 0);
                    Assert.AreEqual("0 @G4@ _GROUP\r\n" +
                                    "1 NAME Test Group\r\n" +
                                    "1 _MEMBER @I1@\r\n", buf);
                }

                groupRec.RemoveMember(member);
                Assert.AreEqual(-1, groupRec.IndexOfMember(member));

                groupRec.ReplaceXRefs(new GDMXRefReplacer());

                Assert.IsFalse(groupRec.IsEmpty());
                groupRec.Clear();
                Assert.IsTrue(groupRec.IsEmpty());
            }
        }
    }
}
