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
using System.IO;
using System.Text;

using Externals;
using NUnit.Framework;

namespace GKTests.GKCommon
{
    [TestFixture]
    public class ZipStorerTests
    {
        [Test]
        public void Test_Common()
        {
            Assert.Throws(typeof(ArgumentNullException), () => { ZipStorer.Open("", FileAccess.Read); });

            string fileName = TestStubs.GetTempFilePath("test.zip");

            using (ZipStorer zip = ZipStorer.Create(fileName, "test")) {
                using (MemoryStream csvStream = new MemoryStream(Encoding.ASCII.GetBytes(TestStubs.CSVData))) {
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

                ZipStorer.ZipFileEntry entry = zip.FindFile("invalid");
                Assert.IsNull(entry);

                entry = zip.FindFile("csv_file.csv");
                Assert.IsNotNull(entry);

                using (MemoryStream csvStream = new MemoryStream()) {
                    Assert.Throws(typeof(ArgumentNullException), () => { zip.ExtractStream(entry, null); });

                    zip.ExtractStream(entry, csvStream);

                    csvStream.Seek(0, SeekOrigin.Begin);
                    using (var reader = new StreamReader(csvStream, Encoding.ASCII)) {
                        string text = reader.ReadToEnd();
                        Assert.AreEqual(TestStubs.CSVData, text);
                    }
                }
            }
        }
    }
}
