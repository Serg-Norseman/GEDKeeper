/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
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
using System.Collections.Generic;
using Externals.CSV;
using NUnit.Framework;

namespace GKTests
{
    [TestFixture]
    public class ExtTests
    {

        /*[Test]
		public void Sort_PerfTest()
		{
			Random rnd = new Random();

			List<ValItem> listQS = new List<ValItem>();
			List<ValItem> listMS = new List<ValItem>();
			List<ValItem> listTS = new List<ValItem>();
			List<ValItem> listCS = new List<ValItem>();

			for (int i = 0; i < 1000000; i++)
			{
				double val = rnd.NextDouble();

				listTS.Add(new ValItem(val));
				listQS.Add(new ValItem(val));
				listMS.Add(new ValItem(val));
				listCS.Add(new ValItem(val));
			}

			listCS.Sort(CompareItems);

			SortHelper.QuickSort(listQS, CompareItems);

			SortHelper.MergeSort(listMS, CompareItems);

			ExtUtils.ListTimSort<ValItem>.Sort(listTS, CompareItems);
		}

		private class ValItem
		{
			public double Value;

			public ValItem(double value)
			{
				this.Value = value;
			}
		}

		private int CompareItems(ValItem item1, ValItem item2)
		{
			return item1.Value.CompareTo(item2.Value);
		}*/


        /*[Test]
		public void Calendar_PerfTest()
		{
			int y = 1990, m = 10, d = 10;
			double jd;

			for (int i = 0; i < 1000000; i++) {
				jd = CalendarConverter.gregorian_to_jd(y, m, d);
				jd = CalendarConverter.gregorian_to_jd2(y, m, d);
				jd = CalendarConverter.gregorian_to_jd3(y, m, d);
			}
		}*/

//		public void MyTestFunc1(
        //            [Values(1, 2, 5)]int x,
        //            [Values("hello", "buy")]string s)
//		{
//			Assert.IsTrue(x < 10);
//		}
//
//		[Test]
        //        public void MyTestFunc2(
        //            [Range(1, 100, 2)]int x,
        //            [Values("hello", "buy")]string s)
        //        {
        //            Assert.IsTrue(x < 50);
        //        }
//
//		public void MyTestFunc3(
        //            [Random(100)]int x,
        //            [Values("hello", "buy")]string s)
//		{
//
//		}


        [Test]
        public void CSV_Tests()
        {
            string data =
                "test,test2,test3,test4\r\n"+
                "12,\"alpha\",12.5,15.4\r\n"+
                "15,\"beta\",15.4,3.7\r\n"+
                "21,\"gamma delta\",21.5,1.02\r\n";

            using (CSVReader csv = new CSVReader(data)) {
                List<object> row;

                row = csv.ReadRow(); // header

                row = csv.ReadRow();
                Assert.AreEqual(12, row[0]);
                Assert.AreEqual("alpha", row[1]);
                Assert.AreEqual(12.5f, row[2]);
                Assert.AreEqual(15.4f, row[3]);

                row = csv.ReadRow();
                Assert.AreEqual(15, row[0]);
                Assert.AreEqual("beta", row[1]);
                Assert.AreEqual(15.4f, row[2]);
                Assert.AreEqual(3.7f, row[3]);

                row = csv.ReadRow();
                Assert.AreEqual(21, row[0]);
                Assert.AreEqual("gamma delta", row[1]);
                Assert.AreEqual(21.5f, row[2]);
                Assert.AreEqual(1.02f, row[3]);

                row = csv.ReadRow();
                Assert.IsNull(row);
            }
        }
    }
}
