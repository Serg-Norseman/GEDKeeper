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

using BSLib;
using NUnit.Framework;

namespace GKTests.GKCommon
{
    [TestFixture]
    public class StringListTests
    {
        [Test]
        public void Test_Common()
        {
            string[] list = new string[] { "The", "string", "list", "test" };

            using (var strList0 = new StringList()) {
                strList0.Text = "The string list test";
                Assert.AreEqual("The string list test", strList0.Text);
            }

            StringList strList = new StringList(list);

            strList.OnChange += SLChange;
            strList.OnChanging += SLChanging;

            Assert.AreEqual("The", strList[0]);
            Assert.AreEqual("string", strList[1]);
            Assert.AreEqual("list", strList[2]);
            Assert.AreEqual("test", strList[3]);

            Assert.AreEqual("The\r\nstring\r\nlist\r\ntest", strList.Text);

            StringList strList2 = new StringList();
            strList2.Assign(null);
            strList2.Assign(strList);
            Assert.AreEqual("The", strList2[0]);
            Assert.AreEqual("string", strList2[1]);
            Assert.AreEqual("list", strList2[2]);
            Assert.AreEqual("test", strList2[3]);
            strList2.Clear();
            strList2.AddStrings(null);
            strList2.AddStrings(strList);
            Assert.AreEqual("The", strList2[0]);
            Assert.AreEqual("string", strList2[1]);
            Assert.AreEqual("list", strList2[2]);
            Assert.AreEqual("test", strList2[3]);
            Assert.Throws(typeof(StringListException), () => { strList2.Delete(-1); });
            Assert.Throws(typeof(StringListException), () => { strList2.Exchange(-1, 0); });
            Assert.Throws(typeof(StringListException), () => { strList2.Exchange(0, -1); });

            Assert.Throws(typeof(StringListException), () => { object item = strList2[-1]; });
            Assert.Throws(typeof(StringListException), () => { strList2[-1] = null; });
            Assert.Throws(typeof(StringListException), () => { object item = strList2.GetObject(-1); });
            Assert.Throws(typeof(StringListException), () => { strList2.SetObject(-1, null); });

            string[] listVals = strList.ToArray();
            Assert.AreEqual("The", listVals[0]);
            Assert.AreEqual("string", listVals[1]);
            Assert.AreEqual("list", listVals[2]);
            Assert.AreEqual("test", listVals[3]);

            strList.Exchange(1, 2);
            Assert.AreEqual("string", strList[2]);
            Assert.AreEqual("list", strList[1]);

            strList[2] = "string2";
            Assert.AreEqual("string2", strList[2]);

            object obj = new object();
            strList.SetObject(2, obj);
            Assert.AreEqual(obj, strList.GetObject(2));
            Assert.AreEqual(2, strList.IndexOfObject(obj));
            Assert.AreEqual(-1, strList.IndexOfObject(new object()));

            strList.CaseSensitive = true;
            Assert.IsTrue(strList.CaseSensitive);

            strList.DuplicateSolve = DuplicateSolve.Accept;
            Assert.AreEqual(DuplicateSolve.Accept, strList.DuplicateSolve);

            strList.DuplicateSolve = DuplicateSolve.Error;
            strList.Sorted = true;
            Assert.AreEqual(true, strList.Sorted);
            Assert.Throws(typeof(StringListException), () => { strList.Add("The"); });
            Assert.Throws(typeof(StringListException), () => { strList[0] = "The"; }); // property Sorted blocks

            Assert.Throws(typeof(StringListException), () => { strList.Insert(0, "insert test"); }); // Operation not allowed on sorted list
            strList.Sorted = false;
            strList.Insert(0, "insert test");
            Assert.AreEqual("insert test", strList[0]);
            Assert.Throws(typeof(StringListException), () => { strList.Insert(-1, "insert test2"); }); // List index out of bounds

            strList.Clear();
            Assert.IsTrue(strList.IsEmpty());


            strList = new StringList(list);
            Assert.AreEqual(0, strList.IndexOf("The"));
            Assert.AreEqual(1, strList.IndexOf("string"));
            Assert.AreEqual(2, strList.IndexOf("list"));
            Assert.AreEqual(3, strList.IndexOf("test"));
            Assert.AreEqual(-1, strList.IndexOf("abrakadabra"));

            strList.DuplicateSolve = DuplicateSolve.Accept;
            strList.Add("string");
            strList.Sorted = true;
            Assert.AreEqual(0, strList.IndexOf("list"));
            Assert.AreEqual(1, strList.IndexOf("string"));
            Assert.AreEqual(3, strList.IndexOf("test"));
            Assert.AreEqual(4, strList.IndexOf("The"));
            Assert.AreEqual(-1, strList.IndexOf("abrakadabra"));

            strList.OnChange -= SLChange;
            strList.OnChanging -= SLChanging;
        }

        private void SLChanging(object sender)
        {
        }

        private void SLChange(object sender)
        {
        }
    }
}
