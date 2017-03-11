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
using System.Collections.Generic;
using System.Data;
using System.IO;
using System.Text;

using Externals;
using Externals.Linguistics;
using GKCommon;
using GKCore;
using NUnit.Framework;

namespace GKTests.GKCommon
{
    [TestFixture]
    public class ExtTests
    {
        [Test]
        public void ZipStorer_Tests()
        {
            Assert.Throws(typeof(ArgumentNullException), () => { ZipStorer.Open("", FileAccess.Read); });

            string fileName = TestStubs.GetTempFilePath("test.zip");

            using (ZipStorer zip = ZipStorer.Create(fileName, "test")) {
                using (MemoryStream csvStream = new MemoryStream(Encoding.ASCII.GetBytes(CSVData))) {
                    zip.AddStream(ZipStorer.Compression.Deflate, "csv_file.csv", csvStream, DateTime.Now, "");
                }

                Assert.Throws(typeof(InvalidOperationException), () => { zip.ReadCentralDir(); });

                ZipStorer xzip = null;
                Assert.Throws(typeof(ArgumentNullException), () => { ZipStorer.RemoveEntries(ref xzip, null); });
                xzip = zip;
                Assert.Throws(typeof(ArgumentNullException), () => { ZipStorer.RemoveEntries(ref xzip, null); });
            }

            using (ZipStorer zip = ZipStorer.Open(fileName, FileAccess.Read)) {
                Assert.Throws(typeof(ArgumentNullException), () => { zip.FindFile(null); });

                ZipStorer.ZipFileEntry? entry = zip.FindFile("invalid");
                Assert.IsNull(entry);

                entry = zip.FindFile("csv_file.csv");
                Assert.IsNotNull(entry);

                using (MemoryStream csvStream = new MemoryStream()) {
                    Assert.Throws(typeof(ArgumentNullException), () => { zip.ExtractStream(entry.Value, null); });

                    zip.ExtractStream(entry.Value, csvStream);

                    csvStream.Seek(0, SeekOrigin.Begin);
                    using (var reader = new StreamReader(csvStream, Encoding.ASCII)) {
                        string text = reader.ReadToEnd();
                        Assert.AreEqual(CSVData, text);
                    }
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
            #if !__MonoCS__
            string fileName = GKUtils.GetTempDir() + "test.ini";
            #else
            string fileName = GKUtils.GetHomePath() + "test.ini";
            #endif

            if (File.Exists(fileName)) File.Delete(fileName); // for local tests!

            using (IniFile iniFile = new IniFile(fileName)) {
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
                Assert.AreEqual(dtx, iniFile.ReadDateTime("test", "dtx", new DateTime())); // writed value

                dtx = new DateTime();
                Assert.AreEqual(dtx, iniFile.ReadDateTime("test", "dtx2", dtx)); // default value

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

        [Test]
        public void RusDeclension_Tests()
        {
            Assert.AreEqual("Иванова Ивана Ивановича", RusDeclension.GetDeclension("Иванов Иван Иванович", DeclensionCase.Genitive));

            Assert.AreEqual("Иванова-Петрова Ивана Ивановича", RusDeclension.GetDeclension("Иванов-Петров Иван Иванович", DeclensionCase.Genitive));

            //Assert.AreEqual("атому", RusDeclension.GetDeclension("атом", DeclensionCase.Dative));
            //Assert.AreEqual("лугу", RusDeclension.GetDeclension("луг", DeclensionCase.Dative));
        }
    }
}
