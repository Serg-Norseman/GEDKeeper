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

using GDModel;
using GKTests;
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

                repoRec.InitNew();
                repoRec.RepositoryName = "Test Repository";
                Assert.AreEqual("Test Repository", repoRec.RepositoryName);

                Assert.IsNotNull(repoRec.Address);
                repoRec.Address.AddressLine1 = "AdrLine1";

                string buf = TestUtils.GetTagStreamText(repoRec, 0);
                Assert.AreEqual("0 @R1@ REPO\r\n" +
                                "1 NAME Test Repository\r\n" +
                                "1 ADDR\r\n" +
                                "2 ADR1 AdrLine1\r\n", buf);

                Assert.IsFalse(repoRec.IsEmpty());
                repoRec.Clear();
                Assert.IsTrue(repoRec.IsEmpty());
            }
        }
    }
}
