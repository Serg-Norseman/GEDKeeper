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
using System.Data;
using System.IO;
using System.Text;

using Externals;
using GKCommon;
using GKTests.Mocks;
using NUnit.Framework;

namespace GKTests.GKCommon
{
    [TestFixture]
    public class ExtTests
    {
        [Test]
        public void Sort_Tests()
        {
            Assert.Throws(typeof(ArgumentNullException), () => { SysUtils.QuickSort<ValItem>(null, null); });
            Assert.Throws(typeof(ArgumentNullException), () => { SysUtils.MergeSort<ValItem>(null, null); });
            Assert.Throws(typeof(ArgumentNullException), () => { ListTimSort<int>.Sort(null, null); });

            Random rnd = new Random();

            List<ValItem> listQS = new List<ValItem>();
            List<ValItem> listMS = new List<ValItem>();
            List<ValItem> listTS = new List<ValItem>();
            List<ValItem> listCS = new List<ValItem>();

            //const int MaxCount = 1000000; // for performance test
            const int MaxCount = 1000; // for common test

            for (int i = 0; i < MaxCount; i++)
            {
                double val = rnd.NextDouble();

                listTS.Add(new ValItem(val));
                listQS.Add(new ValItem(val));
                listMS.Add(new ValItem(val));
                listCS.Add(new ValItem(val));
            }

            listCS.Sort(CompareItems);

            SysUtils.QuickSort(listQS, CompareItems);

            SysUtils.MergeSort(listMS, CompareItems);

            ListTimSort<ValItem>.Sort(listTS, CompareItems);

            // test for sort valid
            //(only for numbers, because some methods is with the permutations, and part - no)
            for (int i = 0; i < MaxCount; i++)
            {
                Assert.AreEqual(listTS[i].Value, listQS[i].Value);
                Assert.AreEqual(listQS[i].Value, listMS[i].Value);
                Assert.AreEqual(listMS[i].Value, listCS[i].Value);
            }
        }

        private static int CompareItems(ValItem item1, ValItem item2)
        {
            return item1.Value.CompareTo(item2.Value);
        }


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
        public void ZipStorer_Tests()
        {
            using (MemoryStream stream = new MemoryStream()) {
                using (ZipStorer zip = ZipStorer.Create(stream, "test")) {
                    using (MemoryStream csvStream = new MemoryStream(Encoding.ASCII.GetBytes(CSVData))) {
                        zip.AddStream(ZipStorer.Compression.Deflate, "csv_file.csv", csvStream, DateTime.Now, "");
                    }

                    //ZipStorer.ZipFileEntry? entry = zip.FindFile("invalid");
                    //Assert.IsNull(entry);

                    //zip.ExtractFile(
                }
            }
        }

        private static string CSVData =
            "test,test2,test3,test4\r\n"+
            "12,\"alpha\",12.5,15.4\r\n"+
            "15,\"beta\",15.4,3.7\r\n"+
            "2100,\"gamma delta\",21.5,1.02\r\n"+
            "91000,\"omega\",21.5,1.02\r\n";

        [Test]
        public void CSV_Tests()
        {
            using (CSVReader csv = new CSVReader(CSVData)) {
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
                Assert.AreEqual(2100, row[0]);
                Assert.AreEqual("gamma delta", row[1]);
                Assert.AreEqual(21.5f, row[2]);
                Assert.AreEqual(1.02f, row[3]);

                row = csv.ReadRow();
                Assert.AreEqual(91000, row[0]);
                Assert.AreEqual("omega", row[1]);
                Assert.AreEqual(21.5f, row[2]);
                Assert.AreEqual(1.02f, row[3]);

                row = csv.ReadRow();
                Assert.IsNull(row);
            }

            using (CSVReader csv = new CSVReader(CSVData)) {
                DataTable tbl = csv.CreateDataTable(true);
                Assert.AreEqual(4, tbl.Rows.Count);
                Assert.AreEqual(4, tbl.Columns.Count);

                DataRow row = tbl.Rows[0];
                Assert.AreEqual(12, row[0]);
                Assert.AreEqual("alpha", row[1]);
                Assert.AreEqual(12.5f, row[2]);
                Assert.AreEqual(15.4f, row[3]);

                row = tbl.Rows[1];
                Assert.AreEqual(15, row[0]);
                Assert.AreEqual("beta", row[1]);
                Assert.AreEqual(15.4f, row[2]);
                Assert.AreEqual(3.7f, row[3]);

                row = tbl.Rows[2];
                Assert.AreEqual(2100, row[0]);
                Assert.AreEqual("gamma delta", row[1]);
                Assert.AreEqual(21.5f, row[2]);
                Assert.AreEqual(1.02f, row[3]);

                row = tbl.Rows[3];
                Assert.AreEqual(91000, row[0]);
                Assert.AreEqual("omega", row[1]);
                Assert.AreEqual(21.5f, row[2]);
                Assert.AreEqual(1.02f, row[3]);
            }
        }

        [Test]
        public void IniFile_Tests()
        {
            using (IniFile iniFile = new IniFile()) {
                iniFile.WriteInteger("test", "int", 15);
                Assert.AreEqual(15, iniFile.ReadInteger("test", "int", 0));
                iniFile.WriteString("test", "int", "0x9F");
                Assert.AreEqual(159, iniFile.ReadInteger("test", "int", 0));

                iniFile.WriteBool("test", "bool", true);
                Assert.AreEqual(true, iniFile.ReadBool("test", "bool", false));

                iniFile.WriteFloat("test", "float", 0.6666d);
                Assert.AreEqual(0.6666d, iniFile.ReadFloat("test", "float", 0.3333d));

                iniFile.WriteString("test", "str", "alpha");
                Assert.AreEqual("alpha", iniFile.ReadString("test", "str", "beta"));

                DateTime dtx = new DateTime(2016, 08, 11);
                iniFile.WriteDateTime("test", "dtx", dtx);
                Assert.AreEqual(dtx, iniFile.ReadDateTime("test", "dtx", new DateTime()));


                iniFile.DeleteKey("test", "str");
                Assert.AreEqual("beta", iniFile.ReadString("test", "str", "beta"));

                //iniFile.DeleteSection("test"); // don't work!!!
                iniFile.DeleteKey("test", "int");
                Assert.AreEqual(0, iniFile.ReadInteger("test", "int", 0));
                iniFile.DeleteKey("test", "bool");
                Assert.AreEqual(false, iniFile.ReadBool("test", "bool", false));
                iniFile.DeleteKey("test", "float");
                Assert.AreEqual(0.3333d, iniFile.ReadFloat("test", "float", 0.3333d));
            }
        }
    }
}
