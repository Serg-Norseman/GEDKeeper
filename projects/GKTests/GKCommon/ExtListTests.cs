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

using GKCommon;
using GKTests.Mocks;
using NUnit.Framework;

namespace GKTests.GKCommon
{
    [TestFixture]
    public class ExtListTests
    {
        [Test]
        public void Test_Common()
        {
            using (ExtList<object> list = new ExtList<object>(true))
            {
                Assert.IsNotNull(list);
            }

            using (ExtList<object> list = new ExtList<object>())
            {
                Assert.IsNotNull(list);
                Assert.AreEqual(0, list.Count);

                Assert.Throws(typeof(ListException), () => { list[-1] = null; });

                object obj = new object();
                object obj1 = new object();

                list.Add(obj);
                Assert.AreEqual(1, list.Count);
                Assert.AreEqual(obj, list[0]);
                Assert.AreEqual(0, list.IndexOf(obj));

                list.Delete(0);
                Assert.AreEqual(0, list.Count);

                list.Add(obj);
                Assert.AreEqual(obj, list.Extract(obj));

                list.Insert(0, obj);

                list[0] = obj;
                Assert.AreEqual(obj, list[0]);

                list.Add(null);
                Assert.AreEqual(2, list.Count);
                Assert.AreEqual(null, list[1]);
                list[1] = obj1;
                Assert.AreEqual(obj1, list[1]);

                list[1] = null;
                Assert.AreEqual(2, list.Count);
                list.Pack();
                Assert.AreEqual(1, list.Count);

                list.Remove(obj);
                Assert.AreEqual(0, list.Count);

                Assert.AreEqual(false, list.OwnsObjects);

                list.OwnsObjects = true;
                Assert.AreEqual(true, list.OwnsObjects);

                list.Clear();
                list.Add(obj);
                list.Add(obj1);
                Assert.AreEqual(obj, list[0]);
                Assert.AreEqual(obj1, list[1]);
                list.Exchange(0, 1);
                Assert.AreEqual(obj, list[1]);
                Assert.AreEqual(obj1, list[0]);
            }

            using (ExtList<ValItem> list = new ExtList<ValItem>())
            {
                Assert.IsNotNull(list);

                list.Add(new ValItem(5));
                list.Add(new ValItem(1));
                list.Add(new ValItem(17));
                list.Add(new ValItem(4));

                list.QuickSort(CompareItems);

                list.MergeSort(CompareItems);
            }
        }

        private static int CompareItems(ValItem item1, ValItem item2)
        {
            return item1.Value.CompareTo(item2.Value);
        }
    }
}
