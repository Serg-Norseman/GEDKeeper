// Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
// This file is part of GEDKeeper, licensed under the GNU GPL v3.
// See LICENSE file in the project root for full license information.

using System;
using GDModel.Providers.GEDCOM;
using NUnit.Framework;

namespace GDModel
{
    [TestFixture]
    public class GDMRepositoryRecordTests
    {
        [Test]
        public void Test_Common()
        {
            var tree = new GDMTree();

            using (GDMRepositoryRecord repoRec = new GDMRepositoryRecord(tree)) {
                Assert.IsNotNull(repoRec);

                tree.NewXRef(repoRec);
                repoRec.RepositoryName = "Test Repository";
                Assert.AreEqual("Test Repository", repoRec.RepositoryName);

                Assert.IsNotNull(repoRec.Address);
                repoRec.Address.AddressLine1 = "AdrLine1";

                using (GDMRepositoryRecord repo2 = new GDMRepositoryRecord(tree)) {
                    tree.NewXRef(repo2);

                    Assert.Throws(typeof(ArgumentException), () => {
                        repo2.Assign(null);
                    });

                    repo2.Assign(repoRec);

                    string buf = GEDCOMProvider.GetTagStreamText(repo2, 0);
                    Assert.AreEqual("0 @R2@ REPO\r\n" +
                                    "1 NAME Test Repository\r\n" +
                                    "1 ADDR\r\n" +
                                    "2 ADR1 AdrLine1\r\n", buf);
                }

                using (GDMRepositoryRecord repo3 = new GDMRepositoryRecord(tree)) {
                    tree.NewXRef(repo3);

                    var matchParams = new MatchParams();
                    matchParams.IndistinctThreshold = 100.0f;

                    Assert.AreEqual(0.0f, repoRec.IsMatch(null, matchParams));

                    repo3.RepositoryName = "Test Repository";
                    Assert.AreEqual(100.0f, repoRec.IsMatch(repo3, matchParams));

                    repo3.RepositoryName = "test";
                    Assert.AreEqual(0.0f, repoRec.IsMatch(repo3, matchParams));
                }

                repoRec.ReplaceXRefs(new GDMXRefReplacer());

                Assert.IsFalse(repoRec.IsEmpty());
                repoRec.Clear();
                Assert.IsTrue(repoRec.IsEmpty());
            }
        }
    }
}
