using System;
using System.Data;
using System.Data.SQLite;
using System.IO;
using System.Reflection;

namespace GKCommon.Database
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

        public GKDatabase()
        {
        }

        #region Private methods

        private static string GetAppPath()
        {
            Module[] mods = Assembly.GetExecutingAssembly().GetModules();
            string fn = mods[0].FullyQualifiedName;
            return Path.GetDirectoryName(fn) + Path.DirectorySeparatorChar;
        }

        private static string GetBaseName()
        {
            return GetAppPath() + "GEDKeeper.db3";
        }

        #endregion

        public void Connect()
        {
            if (fConnection != null)
                throw new GKDatabaseException("Database already connected");

            string baseName = GetBaseName();

            if (!File.Exists(baseName)) {
                CreateDatabase();
            }

            fConnection = (SQLiteConnection)SQLiteFactory.Instance.CreateConnection();
            fConnection.ConnectionString = "Data Source = " + baseName;
            fConnection.Open();
        }

        public void Disconnect()
        {
            if (fConnection == null)
                throw new GKDatabaseException("Database already disconnected");

            fConnection.Close();
            fConnection.Dispose();
            fConnection = null;
        }

        public static void CreateDatabase()
        {
            string baseName = GetBaseName();

            SQLiteConnection.CreateFile(baseName);

            using (SQLiteConnection connection = (SQLiteConnection)SQLiteFactory.Instance.CreateConnection())
            {
                connection.ConnectionString = "Data Source = " + baseName;
                connection.Open();

                using (SQLiteCommand command = new SQLiteCommand(connection))
                {
                    command.CommandText = @"create table [NamesTable] (
                    [id] integer primary key autoincrement not null,
                    [name] char(100) not null,
                    [f_patronymic] char(100),
                    [m_patronymic] char(100),
                    [sex] char(1) not null);";
                    command.CommandType = CommandType.Text;
                    command.ExecuteNonQuery();
                }
            }
        }
    }
}
