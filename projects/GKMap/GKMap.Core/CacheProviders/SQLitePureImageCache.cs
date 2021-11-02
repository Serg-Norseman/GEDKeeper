/*
 *  This file is part of the "GKMap".
 *  GKMap project borrowed from GMap.NET (by radioman).
 *
 *  Copyright (C) 2009-2018 by radioman (email@radioman.lt).
 *  This program is licensed under the FLAT EARTH License.
 */

#if MONO
#undef EMBED_LIBS
#endif

using System;
using System.Collections.Generic;
using System.Data.Common;
using System.Diagnostics;
using System.IO;
using System.IO.Compression;
using System.Reflection;
using System.Threading;
using GKMap.MapProviders;

namespace GKMap.CacheProviders
{
#if !MONO
    using System.Data.SQLite;
#else
    using SQLiteConnection = Mono.Data.Sqlite.SqliteConnection;
    using SQLiteTransaction = Mono.Data.Sqlite.SqliteTransaction;
    using SQLiteCommand = Mono.Data.Sqlite.SqliteCommand;
    using SQLiteDataReader = Mono.Data.Sqlite.SqliteDataReader;
    using SQLiteParameter = Mono.Data.Sqlite.SqliteParameter;
#endif

    /// <summary>
    /// Ultra fast SQLite cache system for tiles.
    /// </summary>
    public class SQLitePureImageCache : IPureImageCache
    {
        private static readonly string SingleSqlSelect = "SELECT Tile FROM main.TilesData WHERE id = (SELECT id FROM main.Tiles WHERE X={0} AND Y={1} AND Zoom={2} AND Type={3})";
        private static readonly string SingleSqlInsert = "INSERT INTO main.Tiles(X, Y, Zoom, Type, CacheTime) VALUES(@p1, @p2, @p3, @p4, @p5)";
        private static readonly string SingleSqlInsertLast = "INSERT INTO main.TilesData(id, Tile) VALUES((SELECT last_insert_rowid()), @p1)";

        private readonly List<string> fAttachedCaches = new List<string>();
        private string fFinalSqlSelect = SingleSqlSelect;
        private string fAttachSqlQuery = string.Empty;
        private string fDetachSqlQuery = string.Empty;

        private string fCache;
        private string fCachePath;
        private string fConnectionString;
        private bool fCreated;
        private string fDir;
        private string fDb;
        private int fPreAllocationPing;

#if EMBED_LIBS
        static SQLitePureImageCache()
        {
            AppDomain.CurrentDomain.AssemblyResolve += CurrentDomain_AssemblyResolve;
        }

        static Assembly CurrentDomain_AssemblyResolve(object sender, ResolveEventArgs args)
        {
            if (args.Name.StartsWith("System.Data.SQLite", StringComparison.OrdinalIgnoreCase)) {
                string appDataDir = Stuff.GetApplicationDataFolderPath();
                if (string.IsNullOrEmpty(appDataDir)) {
                    return null;
                }

                string dllDir = appDataDir + "DllCache" + Path.DirectorySeparatorChar;
                string dll = dllDir + "SQLite_v103_NET" + Environment.Version.Major + "_" + (IntPtr.Size == 8 ? "x64" : "x86") + Path.DirectorySeparatorChar + "System.Data.SQLite.DLL";
                if (!File.Exists(dll)) {
                    string dir = Path.GetDirectoryName(dll);
                    if (!Directory.Exists(dir)) {
                        Directory.CreateDirectory(dir);
                    }

                    Debug.WriteLine("Saving to DllCache: " + dll);

                    if (Environment.Version.Major == 2) {
                    } else if (Environment.Version.Major == 4) {
                        string gzName = IntPtr.Size == 8
                            ? "GKMap.Resources.System.Data.SQLite.x64.NET4.dll.gz"
                            : "GKMap.Resources.System.Data.SQLite.x86.NET4.dll.gz";

                        var resStream = Stuff.LoadResourceStream(gzName);
                        using (var gs = new GZipStream(resStream, CompressionMode.Decompress)) {
                            using (MemoryStream exctDll = new MemoryStream()) {
                                byte[] tmp = new byte[1024 * 256];
                                int r;
                                while ((r = gs.Read(tmp, 0, tmp.Length)) > 0) {
                                    exctDll.Write(tmp, 0, r);
                                }

                                File.WriteAllBytes(dll, exctDll.ToArray());
                            }
                        }
                    }
                }

                Debug.WriteLine("Assembly.LoadFile: " + dll);

                return Assembly.LoadFile(dll);
            }
            return null;
        }

        private static int fPing;
#endif

        /// <summary>
        /// triggers dynamic SQLite loading
        /// </summary>
        public static void Ping()
        {
#if EMBED_LIBS
            if (++fPing == 1) {
                Trace.WriteLine("SQLiteVersion: " + SQLiteConnection.SQLiteVersion + " | " + SQLiteConnection.SQLiteSourceId + " | " + SQLiteConnection.DefineConstants);
            }
#endif
        }

        /// <summary>
        /// local cache location
        /// </summary>
        public string CacheLocation
        {
            get {
                return fCache;
            }
            set {
                fCache = value;

                fCachePath = Path.Combine(fCache, "TileDBv5") + Path.DirectorySeparatorChar;

                fDir = fCachePath + GMapProvider.LanguageStr + Path.DirectorySeparatorChar;

                // recreate dir
                if (!Directory.Exists(fDir)) {
                    Directory.CreateDirectory(fDir);
                }

#if !MONO
                SQLiteConnection.ClearAllPools();
#endif
                // make empty db
                fDb = fDir + "Data.gmdb";
                fCreated = !File.Exists(fDb) ? CreateEmptyDB(fDb) : AlterDBAddTimeColumn(fDb);

                CheckPreAllocation();

#if !MONO
                fConnectionString = string.Format("Data Source=\"{0}\";Page Size=32768;Pooling=True", fDb); //;Journal Mode=Wal
#else
                fConnectionString = string.Format("Version=3,URI=file://{0},FailIfMissing=True,Page Size=32768,Pooling=True", fDb);
#endif

                // clear old attachments
                fAttachedCaches.Clear();
                RebuildFinalSelect();

                // attach all databases from main cache location
                var dbs = Directory.GetFiles(fDir, "*.gmdb", SearchOption.AllDirectories);
                foreach (var d in dbs) {
                    if (d != fDb) {
                        Attach(d);
                    }
                }
            }
        }

        /// <summary>
        /// pre-allocate 32MB free space 'ahead' if needed,
        /// decreases fragmentation
        /// </summary>
        private void CheckPreAllocation()
        {
            byte[] pageSizeBytes = new byte[2];
            byte[] freePagesBytes = new byte[4];

            lock (this) {
                using (var dbf = File.Open(fDb, FileMode.Open, FileAccess.Read, FileShare.ReadWrite)) {
                    dbf.Seek(16, SeekOrigin.Begin);

#if !MONO
                    dbf.Lock(16, 2);
                    dbf.Read(pageSizeBytes, 0, 2);
                    dbf.Unlock(16, 2);

                    dbf.Seek(36, SeekOrigin.Begin);

                    dbf.Lock(36, 4);
                    dbf.Read(freePagesBytes, 0, 4);
                    dbf.Unlock(36, 4);
#else
                    dbf.Read(pageSizeBytes, 0, 2);
                    dbf.Seek(36, SeekOrigin.Begin);
                    dbf.Read(freePagesBytes, 0, 4);
#endif

                    dbf.Close();
                }
            }

            if (BitConverter.IsLittleEndian) {
                Array.Reverse(pageSizeBytes);
                Array.Reverse(freePagesBytes);
            }

            ushort pageSize = BitConverter.ToUInt16(pageSizeBytes, 0);
            uint freePages = BitConverter.ToUInt32(freePagesBytes, 0);
            var freeMB = (pageSize * freePages) / (1024.0 * 1024.0);

            int addSizeMB = 32;
            int waitUntilMB = 4;

            Debug.WriteLine("FreePageSpace in cache: " + freeMB + "MB | " + freePages + " pages");

            if (freeMB <= waitUntilMB) {
                PreAllocateDB(fDb, addSizeMB);
            }
        }

        #region -- import / export --
        public static bool CreateEmptyDB(string file)
        {
            bool ret = true;

            try {
                string dir = Path.GetDirectoryName(file);
                if (!Directory.Exists(dir)) {
                    Directory.CreateDirectory(dir);
                }

                using (SQLiteConnection cn = new SQLiteConnection()) {
#if !MONO
                    cn.ConnectionString = string.Format("Data Source=\"{0}\";FailIfMissing=False;Page Size=32768", file);
#else
                    cn.ConnectionString = string.Format("Version=3,URI=file://{0},FailIfMissing=False,Page Size=32768", file);
#endif
                    cn.Open();
                    string strCreateTileDb = Stuff.LoadResourceString("GKMap.Resources.CreateTileDb.sql");

                    using (DbTransaction tr = cn.BeginTransaction()) {
                        try {
                            using (DbCommand cmd = cn.CreateCommand()) {
                                cmd.Transaction = tr;
                                cmd.CommandText = strCreateTileDb;
                                cmd.ExecuteNonQuery();
                            }

                            tr.Commit();
                        } catch (Exception exx) {
#if MONO
                            Console.WriteLine("CreateEmptyDB: " + exx.ToString());
#endif
                            Debug.WriteLine("CreateEmptyDB: " + exx);

                            tr.Rollback();
                            ret = false;
                        }
                    }

                    cn.Close();
                }
            } catch (Exception ex) {
#if MONO
                Console.WriteLine("CreateEmptyDB: " + ex.ToString());
#endif
                Debug.WriteLine("CreateEmptyDB: " + ex);
                ret = false;
            }
            return ret;
        }

        public static bool PreAllocateDB(string file, int addSizeInMBytes)
        {
            bool ret = true;

            try {
                Debug.WriteLine("PreAllocateDB: " + file + ", +" + addSizeInMBytes + "MB");

                using (SQLiteConnection cn = new SQLiteConnection()) {
#if !MONO
                    cn.ConnectionString = string.Format("Data Source=\"{0}\";FailIfMissing=False;Page Size=32768", file);
#else
                    cn.ConnectionString = string.Format("Version=3,URI=file://{0},FailIfMissing=False,Page Size=32768", file);
#endif
                    cn.Open();
                    {
                        using (DbTransaction tr = cn.BeginTransaction()) {
                            try {
                                using (DbCommand cmd = cn.CreateCommand()) {
                                    cmd.Transaction = tr;
                                    cmd.CommandText = string.Format("create table large (a); insert into large values (zeroblob({0})); drop table large;", addSizeInMBytes * 1024 * 1024);
                                    cmd.ExecuteNonQuery();
                                }
                                tr.Commit();
                            } catch (Exception exx) {
#if MONO
                                Console.WriteLine("PreAllocateDB: " + exx.ToString());
#endif
                                Debug.WriteLine("PreAllocateDB: " + exx);

                                tr.Rollback();
                                ret = false;
                            }
                        }
                        cn.Close();
                    }
                }
            } catch (Exception ex) {
#if MONO
                Console.WriteLine("PreAllocateDB: " + ex.ToString());
#endif
                Debug.WriteLine("PreAllocateDB: " + ex);
                ret = false;
            }
            return ret;
        }

        private static bool AlterDBAddTimeColumn(string file)
        {
            bool ret = true;

            try {
                if (File.Exists(file)) {
                    using (SQLiteConnection cn = new SQLiteConnection()) {
#if !MONO
                        cn.ConnectionString = string.Format("Data Source=\"{0}\";FailIfMissing=False;Page Size=32768;Pooling=True", file);
#else
                        cn.ConnectionString = string.Format("Version=3,URI=file://{0},FailIfMissing=False,Page Size=32768,Pooling=True", file);
#endif
                        cn.Open();
                        {
                            using (DbTransaction tr = cn.BeginTransaction()) {
                                bool? NoCacheTimeColumn;

                                try {
                                    using (DbCommand cmd = new SQLiteCommand("SELECT CacheTime FROM Tiles", cn)) {
                                        cmd.Transaction = tr;

                                        using (DbDataReader rd = cmd.ExecuteReader()) {
                                            rd.Close();
                                        }
                                        NoCacheTimeColumn = false;
                                    }
                                } catch (Exception ex) {
                                    if (ex.Message.Contains("no such column: CacheTime")) {
                                        NoCacheTimeColumn = true;
                                    } else {
                                        throw ex;
                                    }
                                }

                                try {
                                    if (NoCacheTimeColumn.HasValue && NoCacheTimeColumn.Value) {
                                        using (DbCommand cmd = cn.CreateCommand()) {
                                            cmd.Transaction = tr;

                                            cmd.CommandText = "ALTER TABLE Tiles ADD CacheTime DATETIME";

                                            cmd.ExecuteNonQuery();
                                        }
                                        tr.Commit();
                                        NoCacheTimeColumn = false;
                                    }
                                } catch (Exception exx) {
#if MONO
                                    Console.WriteLine("AlterDBAddTimeColumn: " + exx.ToString());
#endif
                                    Debug.WriteLine("AlterDBAddTimeColumn: " + exx);

                                    tr.Rollback();
                                    ret = false;
                                }
                            }
                            cn.Close();
                        }
                    }
                } else {
                    ret = false;
                }
            } catch (Exception ex) {
#if MONO
                Console.WriteLine("AlterDBAddTimeColumn: " + ex.ToString());
#endif
                Debug.WriteLine("AlterDBAddTimeColumn: " + ex);
                ret = false;
            }
            return ret;
        }

        public static bool VacuumDb(string file)
        {
            bool ret = true;

            try {
                using (SQLiteConnection cn = new SQLiteConnection()) {
#if !MONO
                    cn.ConnectionString = string.Format("Data Source=\"{0}\";FailIfMissing=True;Page Size=32768", file);
#else
                    cn.ConnectionString = string.Format("Version=3,URI=file://{0},FailIfMissing=True,Page Size=32768", file);
#endif
                    cn.Open();
                    using (DbCommand cmd = cn.CreateCommand()) {
                        cmd.CommandText = "vacuum;";
                        cmd.ExecuteNonQuery();
                    }
                    cn.Close();
                }
            } catch (Exception ex) {
                Debug.WriteLine("VacuumDb: " + ex);
                ret = false;
            }
            return ret;
        }
        #endregion

        private void RebuildFinalSelect()
        {
            fFinalSqlSelect = null;
            fFinalSqlSelect = SingleSqlSelect;

            fAttachSqlQuery = null;
            fAttachSqlQuery = string.Empty;

            fDetachSqlQuery = null;
            fDetachSqlQuery = string.Empty;

            int i = 1;
            foreach (var c in fAttachedCaches) {
                fFinalSqlSelect += string.Format("\nUNION SELECT Tile FROM db{0}.TilesData WHERE id = (SELECT id FROM db{0}.Tiles WHERE X={{0}} AND Y={{1}} AND Zoom={{2}} AND Type={{3}})", i);
                fAttachSqlQuery += string.Format("\nATTACH '{0}' as db{1};", c, i);
                fDetachSqlQuery += string.Format("\nDETACH DATABASE db{0};", i);

                i++;
            }
        }

        public void Attach(string db)
        {
            if (!fAttachedCaches.Contains(db)) {
                fAttachedCaches.Add(db);
                RebuildFinalSelect();
            }
        }

        public void Detach(string db)
        {
            if (fAttachedCaches.Contains(db)) {
                fAttachedCaches.Remove(db);
                RebuildFinalSelect();
            }
        }

        bool IPureImageCache.PutImageToCache(byte[] tile, int type, GPoint pos, int zoom)
        {
            bool ret = true;
            if (fCreated) {
                try {
                    using (SQLiteConnection cn = new SQLiteConnection()) {
                        cn.ConnectionString = fConnectionString;
                        cn.Open();
                        {
                            using (DbTransaction tr = cn.BeginTransaction()) {
                                try {
                                    using (DbCommand cmd = cn.CreateCommand()) {
                                        cmd.Transaction = tr;
                                        cmd.CommandText = SingleSqlInsert;

                                        cmd.Parameters.Add(new SQLiteParameter("@p1", pos.X));
                                        cmd.Parameters.Add(new SQLiteParameter("@p2", pos.Y));
                                        cmd.Parameters.Add(new SQLiteParameter("@p3", zoom));
                                        cmd.Parameters.Add(new SQLiteParameter("@p4", type));
                                        cmd.Parameters.Add(new SQLiteParameter("@p5", DateTime.Now));

                                        cmd.ExecuteNonQuery();
                                    }

                                    using (DbCommand cmd = cn.CreateCommand()) {
                                        cmd.Transaction = tr;

                                        cmd.CommandText = SingleSqlInsertLast;
                                        cmd.Parameters.Add(new SQLiteParameter("@p1", tile));

                                        cmd.ExecuteNonQuery();
                                    }
                                    tr.Commit();
                                } catch (Exception ex) {
#if MONO
                                    Console.WriteLine("PutImageToCache: " + ex.ToString());
#endif
                                    Debug.WriteLine("PutImageToCache: " + ex);

                                    tr.Rollback();
                                    ret = false;
                                }
                            }
                        }
                        cn.Close();
                    }

                    if (Interlocked.Increment(ref fPreAllocationPing) % 22 == 0) {
                        CheckPreAllocation();
                    }
                } catch (Exception ex) {
#if MONO
                    Console.WriteLine("PutImageToCache: " + ex.ToString());
#endif
                    Debug.WriteLine("PutImageToCache: " + ex);
                    ret = false;
                }
            }
            return ret;
        }

        PureImage IPureImageCache.GetImageFromCache(int type, GPoint pos, int zoom)
        {
            PureImage ret = null;
            try {
                using (SQLiteConnection cn = new SQLiteConnection()) {
                    cn.ConnectionString = fConnectionString;
                    cn.Open();

                    if (!string.IsNullOrEmpty(fAttachSqlQuery)) {
                        using (DbCommand com = cn.CreateCommand()) {
                            com.CommandText = fAttachSqlQuery;
                            int x = com.ExecuteNonQuery();
                            //Debug.WriteLine("Attach: " + x);
                        }
                    }

                    using (DbCommand com = cn.CreateCommand()) {
                        com.CommandText = string.Format(fFinalSqlSelect, pos.X, pos.Y, zoom, type);

                        using (DbDataReader rd = com.ExecuteReader(System.Data.CommandBehavior.SequentialAccess)) {
                            if (rd.Read()) {
                                long length = rd.GetBytes(0, 0, null, 0, 0);
                                byte[] tile = new byte[length];
                                rd.GetBytes(0, 0, tile, 0, tile.Length);
                                if (GMaps.TileImageProxy != null) {
                                    ret = GMaps.TileImageProxy.FromArray(tile);
                                }
                            }

                            rd.Close();
                        }
                    }

                    if (!string.IsNullOrEmpty(fDetachSqlQuery)) {
                        using (DbCommand com = cn.CreateCommand()) {
                            com.CommandText = fDetachSqlQuery;
                            int x = com.ExecuteNonQuery();
                            //Debug.WriteLine("Detach: " + x);
                        }
                    }

                    cn.Close();
                }
            } catch (Exception ex) {
#if MONO
                Console.WriteLine("GetImageFromCache: " + ex.ToString());
#endif
                Debug.WriteLine("GetImageFromCache: " + ex);
                ret = null;
            }

            return ret;
        }

        int IPureImageCache.DeleteOlderThan(DateTime date, int? type)
        {
            int affectedRows = 0;

            try {
                using (SQLiteConnection cn = new SQLiteConnection()) {
                    cn.ConnectionString = fConnectionString;
                    cn.Open();
                    using (DbCommand com = cn.CreateCommand()) {
                        com.CommandText = string.Format("DELETE FROM Tiles WHERE CacheTime is not NULL and CacheTime < datetime('{0}')", date.ToString("s"));
                        if (type.HasValue) {
                            com.CommandText += " and Type = " + type;
                        }

                        affectedRows = com.ExecuteNonQuery();
                    }
                }
            } catch (Exception ex) {
#if MONO
                Console.WriteLine("DeleteOlderThan: " + ex);
#endif
                Debug.WriteLine("DeleteOlderThan: " + ex);
            }

            return affectedRows;
        }
    }
}
