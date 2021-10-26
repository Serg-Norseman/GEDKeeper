/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2021 by Sergey V. Zhdanovskih.
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
using System.Diagnostics;
using System.IO;
using SQLite;

namespace GKCore.Database
{
    public class GKDatabaseException : Exception
    {
        public GKDatabaseException(string message) : base(message)
        {
        }
    }


    /// <summary>
    /// 
    /// </summary>
    public class GKDatabase
    {
        private SQLiteConnection fConnection;

        static GKDatabase()
        {
            SQLiteLoader.Load();
        }

        public GKDatabase()
        {
        }

        public void Connect()
        {
            if (fConnection != null)
                throw new GKDatabaseException("Database already connected");

            string databaseName = GetBaseName();

            Debug.WriteLine("DatabaseName: " + databaseName);

            fConnection = new SQLiteConnection(databaseName);

            CreateDatabase();
        }

        public void Disconnect()
        {
            if (fConnection == null)
                throw new GKDatabaseException("Database already disconnected");

            fConnection.Close();
            fConnection.Dispose();
            fConnection = null;
        }

        /// <summary>
        /// Cleaning waste space
        /// </summary>
        public void CleanSpace()
        {
            fConnection.Execute("VACUUM;");
        }

        public string GetBaseName()
        {
            return Path.Combine(AppHost.Instance.GetAppDataPath(), "GEDKeeper.db3");
        }

        private void CreateDatabase()
        {
            /*
                @"create table [NamesTable] (
                    [id] integer primary key autoincrement not null,
                    [name] char(100) not null,
                    [f_patronymic] char(100),
                    [m_patronymic] char(100),
                    [sex] char(1) not null);";
             */
            fConnection.CreateTable<GKDBNameEntry>();
        }

        #region Records

        public void Execute(string query, params object[] args)
        {
            fConnection.Execute(query, args);
        }

        public int AddRecord(object obj)
        {
            return fConnection.Insert(obj);
        }

        public void UpdateRecord(object obj)
        {
            fConnection.Update(obj);
        }

        public void DeleteRecord(object obj)
        {
            fConnection.Delete(obj);
        }

        public void DeleteRecord<T>(int objId)
        {
            fConnection.Delete<T>(objId);
        }

        public T GetRecord<T>(int objId) where T : new()
        {
            T result;
            if (objId <= 0) {
                result = default(T);
            } else {
                try {
                    result = fConnection.Get<T>(objId);
                } catch (InvalidOperationException) {
                    // record not exists
                    result = default(T);
                }
            }
            return result;
        }

        public IList<T> QueryRecords<T>(string query, params object[] args) where T : new()
        {
            return fConnection.Query<T>(query, args);
        }

        #endregion

        public IList<GKDBNameEntry> QueryNameEntries()
        {
            return fConnection.Query<GKDBNameEntry>("select * from NamesTable");
        }

        public IList<GKDBNameEntry> QueryNameEntries(string name)
        {
            return fConnection.Query<GKDBNameEntry>("select * from NamesTable where (Name = ?)", name);
        }
    }
}
