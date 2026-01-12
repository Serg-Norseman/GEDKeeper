/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
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
