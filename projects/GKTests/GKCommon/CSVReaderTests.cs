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

using System.Collections.Generic;
using System.Data;

using Externals;
using NUnit.Framework;

namespace GKTests.GKCommon
{
    [TestFixture]
    public class CSVReaderTests
    {
        [Test]
        public void Test_Common()
        {
            using (CSVReader csv = new CSVReader(TestStubs.CSVData)) {
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

            using (CSVReader csv = new CSVReader(TestStubs.CSVData)) {
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
    }
}
