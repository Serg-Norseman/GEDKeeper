/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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
using GKCommon;
using NUnit.Framework;

namespace GKTests.GKCommon
{
    [TestFixture]
    public class IndistinctMatchingTests
    {
        [Test]
        public void Test_Common()
        {
            int res1, res2;

            res1 = IndistinctMatching.LevenshteinDistance("", "");
            Assert.AreEqual(0, res1);
            res1 = IndistinctMatching.LevenshteinDistance("Ivanov", "");
            Assert.AreEqual(6, res1);
            res1 = IndistinctMatching.LevenshteinDistance("", "Petroff");
            Assert.AreEqual(7, res1);
            res1 = IndistinctMatching.LevenshteinDistance("Ivanov", "Ivanov");
            Assert.AreEqual(0, res1);
            res1 = IndistinctMatching.LevenshteinDistance("Ivanov", "IvanovTest");
            Assert.AreEqual(4, res1);
            res1 = IndistinctMatching.LevenshteinDistance("Ivanvo", "Ivanov");
            Assert.AreEqual(2, res1);
            res1 = IndistinctMatching.LevenshteinDistance("Petroff", "Pterov");
            Assert.AreEqual(4, res1); // permutation -fail

            res1 = IndistinctMatching.DamerauLevenshteinDistance("", "");
            Assert.AreEqual(0, res1);
            res1 = IndistinctMatching.DamerauLevenshteinDistance("Ivanov", "");
            Assert.AreEqual(6, res1);
            res1 = IndistinctMatching.DamerauLevenshteinDistance("", "Petroff");
            Assert.AreEqual(7, res1);
            res2 = IndistinctMatching.DamerauLevenshteinDistance("Ivanov", "Ivanov");
            Assert.AreEqual(0, res2);
            res1 = IndistinctMatching.DamerauLevenshteinDistance("Ivanov", "IvanovTest");
            Assert.AreEqual(4, res1);
            res2 = IndistinctMatching.DamerauLevenshteinDistance("Ivanvo", "Ivanov");
            Assert.AreEqual(1, res2);
            res1 = IndistinctMatching.DamerauLevenshteinDistance("Petroff", "Pterov");
            Assert.AreEqual(3, res1); // permutation -ok

            Assert.Throws(typeof(ArgumentNullException), () => { IndistinctMatching.GetSimilarity("Ivanvo", null); });
            Assert.Throws(typeof(ArgumentNullException), () => { IndistinctMatching.GetSimilarity(null, "Ivanov"); });

            Assert.GreaterOrEqual(IndistinctMatching.GetSimilarity("Ivanov", "Ivanov"), 1.0f);
            Assert.GreaterOrEqual(IndistinctMatching.GetSimilarity("Ivanvo", "Ivanov"), 0.833f);
        }

        [Test]
        public void Test_Perf1()
        {
            for (int i = 1; i < 10000; i++) {
                IndistinctMatching.LevenshteinDistance("Ivan", "Ivanov");
                IndistinctMatching.DamerauLevenshteinDistance("Ivan", "Ivanov");
            }
        }

        [Test]
        public void Test_Perf2()
        {
            for (int i = 1; i < 10000; i++) {
                IndistinctMatching.LevenshteinDistance("Ivanvo", "Ivanov");
                IndistinctMatching.DamerauLevenshteinDistance("Ivanvo", "Ivanov");
            }
        }
    }
}
