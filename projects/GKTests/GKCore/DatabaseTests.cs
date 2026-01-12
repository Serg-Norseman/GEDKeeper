/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
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
