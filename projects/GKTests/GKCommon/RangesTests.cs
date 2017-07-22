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
    public class RangesTests
    {
        [Test]
        public void Test_Common()
        {
            Assert.Throws(typeof(ArgumentException), () => { new Range<int>((2), (1)); });

            Assert.IsTrue(new Range<int>((1), (2)).IsOverlapped(new Range<int>(1, 2)), "chk1"); // true
            Assert.IsTrue(new Range<int>((1), (3)).IsOverlapped(new Range<int>((2), (4))), "chk2"); // true
            Assert.IsTrue(new Range<int>((2), (4)).IsOverlapped(new Range<int>((1), (3))), "chk3"); // true
            Assert.IsFalse(new Range<int>((3), (4)).IsOverlapped(new Range<int>((1), (2))), "chk4"); // false
            Assert.IsFalse(new Range<int>((1), (2)).IsOverlapped(new Range<int>((3), (4))), "chk5"); // false
            Assert.IsTrue(new Range<int>((2), (3)).IsOverlapped(new Range<int>((1), (4))), "chk6"); // true
            Assert.IsTrue(new Range<int>((1), (4)).IsOverlapped(new Range<int>((2), (3))), "chk7"); // true

            Assert.IsTrue(new Range<int>((1), (2)).IsOverlapped(new Range<int>((1), (4))), "chk8"); // true
            Assert.IsTrue(new Range<int>((1), (4)).IsOverlapped(new Range<int>((1), (2))), "chk9"); // true
            Assert.IsTrue(new Range<int>((1), (4)).IsOverlapped(new Range<int>((3), (4))), "chk10"); // true
            Assert.IsTrue(new Range<int>((3), (4)).IsOverlapped(new Range<int>((1), (4))), "chk11"); // true
        }
    }
}
