/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2018 by Sergey V. Zhdanovskih.
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

using NUnit.Framework;

namespace GKUI.Components
{
    [TestFixture]
    public class GKListItemTests
    {
        [Test]
        public void Test_Common()
        {
            var item1 = new GKListItem(10, null);
            var item2 = new GKListItem(20, null);
            Assert.AreEqual(-1, item1.CompareTo(item2));

            item1 = new GKListItem(10, null);
            item2 = new GKListItem(null, null);
            Assert.AreEqual(-1, item1.CompareTo(item2));

            item1 = new GKListItem(null, null);
            item2 = new GKListItem(20, null);
            Assert.AreEqual(1, item1.CompareTo(item2));

            Assert.AreEqual(0, item1.CompareTo(item1));
            Assert.AreEqual(-1, item1.CompareTo(null));


            var subitem1 = new GKListSubItem(10);
            var subitem2 = new GKListSubItem(20);
            Assert.AreEqual(-1, subitem1.CompareTo(subitem2));

            subitem1 = new GKListSubItem(10);
            subitem2 = new GKListSubItem(null);
            Assert.AreEqual(-1, subitem1.CompareTo(subitem2));

            subitem1 = new GKListSubItem(null);
            subitem2 = new GKListSubItem(20);
            Assert.AreEqual(1, subitem1.CompareTo(subitem2));

            Assert.AreEqual(0, subitem1.CompareTo(subitem1));
            Assert.AreEqual(-1, subitem1.CompareTo(null));
        }
    }
}
