/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2024 by Sergey V. Zhdanovskih.
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
using NUnit.Framework;

namespace GDModel
{
    [TestFixture]
    public class GDMLinesTests
    {
        [Test]
        public void Test_Common()
        {
            string[] list = new string[] { "The", "string", "list", "test" };

            var strList0 = new GDMLines();
            strList0.Text = "The string list test";
            Assert.AreEqual("The string list test", strList0.Text);

            GDMLines strList = new GDMLines(list);
            Assert.AreEqual("The", strList[0]);
            Assert.AreEqual("string", strList[1]);
            Assert.AreEqual("list", strList[2]);
            Assert.AreEqual("test", strList[3]);

            Assert.AreEqual(0, strList.IndexOf("The"));
            Assert.AreEqual(1, strList.IndexOf("string"));
            Assert.AreEqual(2, strList.IndexOf("list"));
            Assert.AreEqual(3, strList.IndexOf("test"));
            Assert.AreEqual(-1, strList.IndexOf("abrakadabra"));

            Assert.AreEqual("The\r\nstring\r\nlist\r\ntest", strList.Text);

            GDMLines strList2 = new GDMLines();
            strList2.Assign(null);
            strList2.Assign(strList);
            Assert.AreEqual("The", strList2[0]);
            Assert.AreEqual("string", strList2[1]);
            Assert.AreEqual("list", strList2[2]);
            Assert.AreEqual("test", strList2[3]);
            strList2.Clear();

            GDMLines otherList = null;
            Assert.Throws(typeof(ArgumentNullException), () => { strList2.AddRange(otherList); });

            strList2.AddRange(strList);
            Assert.AreEqual("The", strList2[0]);
            Assert.AreEqual("string", strList2[1]);
            Assert.AreEqual("list", strList2[2]);
            Assert.AreEqual("test", strList2[3]);
            Assert.Throws(typeof(ArgumentOutOfRangeException), () => { strList2.RemoveAt(-1); });

            Assert.Throws(typeof(ArgumentOutOfRangeException), () => { object item = strList2[-1]; });
            Assert.Throws(typeof(ArgumentOutOfRangeException), () => { strList2[-1] = null; });

            string[] listVals = strList.ToArray();
            Assert.AreEqual("The", listVals[0]);
            Assert.AreEqual("string", listVals[1]);
            Assert.AreEqual("list", listVals[2]);
            Assert.AreEqual("test", listVals[3]);

            strList[2] = "string2";
            Assert.AreEqual("string2", strList[2]);

            strList.Insert(0, "insert test");
            Assert.AreEqual("insert test", strList[0]);
            Assert.Throws(typeof(ArgumentOutOfRangeException), () => { strList.Insert(-1, "insert test2"); }); // List index out of bounds

            strList.Clear();
            Assert.IsTrue(strList.IsEmpty());

            string[] strArr = null;
            Assert.Throws(typeof(ArgumentNullException), () => { strList.AddRange(strArr); });
        }

        [Test]
        public void Test_CRLF()
        {
            var strList1 = new GDMLines("The\r\nstring\r\n\r\nlist\r\ntest");
            Assert.AreEqual("The", strList1[0]);
            Assert.AreEqual("string", strList1[1]);
            Assert.AreEqual("", strList1[2]);
            Assert.AreEqual("list", strList1[3]);
            Assert.AreEqual("test", strList1[4]);
        }

        [Test]
        public void Test_LFOnly()
        {
            var strList1 = new GDMLines("The1\nstring1\n\nlist1\ntest1");
            Assert.AreEqual("The1", strList1[0]);
            Assert.AreEqual("string1", strList1[1]);
            Assert.AreEqual("", strList1[2]);
            Assert.AreEqual("list1", strList1[3]);
            Assert.AreEqual("test1", strList1[4]);
        }

        [Test]
        public void Test_EmptyLines()
        {
            var strList = new GDMLines();
            strList.Text = "The string\r\n\r\n list test";
            Assert.AreEqual("The string\r\n\r\n list test", strList.Text);

            Assert.AreEqual("The string", strList[0]);
            Assert.AreEqual("", strList[1]);
            Assert.AreEqual(" list test", strList[2]);
        }
    }
}
