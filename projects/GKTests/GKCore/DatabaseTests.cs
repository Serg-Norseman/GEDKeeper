/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
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

using System.IO;
using GDModel;
using GKTests;
using NUnit.Framework;

namespace GKCore.Database
{
    /// <summary>
    /// 
    /// </summary>
    [TestFixture]
    public class DatabaseTests
    {
        public DatabaseTests()
        {
            TestUtils.InitUITest();
        }

        [Test]
        public void Test_Database()
        {
            var instance = new GKDatabase();
            Assert.IsNotNull(instance);

            CleanDB(instance);

            instance.Connect();
            Assert.Throws(typeof(GKDatabaseException), () => { instance.Connect(); }); // already connected

            instance.CleanSpace();

            instance.Disconnect();
            Assert.Throws(typeof(GKDatabaseException), () => { instance.Disconnect(); }); // already disconnected
        }

        [Test]
        public void Test_NamesTable()
        {
            var instance = new GKDatabase();
            Assert.IsNotNull(instance);

            CleanDB(instance);

            instance.Connect();

            var record = new GKDBNameEntry();
            record.Name = "Ivan";
            record.F_Patronymic = "Ivanovna";
            record.M_Patronymic = "Ivanovich";
            record.Sex = GDMSex.svMale;

            var result = instance.AddRecord(record);
            Assert.AreNotEqual(0, result);

            var list = instance.QueryNameEntries();
            Assert.IsNotNull(list);
            Assert.AreNotEqual(0, list.Count);
            Assert.AreEqual("Ivan", list[0].Name);
        }

        private static void CleanDB(GKDatabase db)
        {
            string databaseName = GKDatabase.GetBaseName();
            try {
                if (File.Exists(databaseName)) {
                    File.Delete(databaseName);
                }
            } catch {
            }
        }
    }
}
