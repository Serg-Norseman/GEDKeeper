/*
 *  This file is part of the "GKMap".
 *  GKMap project borrowed from GMap.NET (by radioman).
 *
 *  Copyright (C) 2009-2018 by radioman (email@radioman.lt).
 *  This program is licensed under the FLAT EARTH License.
 */

#if !MOBILE

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Runtime.InteropServices;
using System.Threading;
using GKMap.MapProviders;
using SQLite;

namespace GKMap.CacheProviders
{
    /// <summary>
    /// SQLite cache system for tiles.
    /// </summary>
    public class SQLitePureImageCache : IPureImageCache
    {
        private const string SingleSqlSelect = "SELECT Id, Tile FROM main.TilesData WHERE id = (SELECT id FROM main.Tiles WHERE X={0} AND Y={1} AND Zoom={2} AND Type={3})";

        private readonly List<string> fAttachedCaches = new List<string>();
        private string fFinalSqlSelect = SingleSqlSelect;
        private string fAttachSqlQuery = string.Empty;
        private string fDetachSqlQuery = string.Empty;

        private string fCache;
        private string fCachePath;
        private bool fCreated;
        private string fDir;
        private string fDb;
        private int fPreAllocationPing;

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

                // make empty db
                fDb = fDir + "Data.gmdb";
                fCreated = !File.Exists(fDb) ? CreateEmptyDB(fDb) : AlterDBAddTimeColumn(fDb);

                CheckPreAllocation();

                // clear old attachments
                fAttachedCaches.Clear();

                // attach all databases from main cache location
                var dbs = Directory.GetFiles(fDir, "*.gmdb", SearchOption.AllDirectories);
                foreach (var d in dbs) {
                    if (d != fDb) {
                        Attach(d);
                    }
                }
            }
        }

        static SQLitePureImageCache() {
#if !NETCORE && !NETSTANDARD
            SQLiteLoader.Load();
#endif
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
                    Lock(dbf, 16, 2);
                    dbf.Read(pageSizeBytes, 0, 2);
                    Unlock(dbf, 16, 2);

                    dbf.Seek(36, SeekOrigin.Begin);
                    Lock(dbf, 36, 4);
                    dbf.Read(freePagesBytes, 0, 4);
                    Unlock(dbf, 36, 4);

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

        private static void Unlock(FileStream dbf, int position, int length)
        {
#if NETSTANDARD || NETCOREAPP || NET5_0_OR_GREATER || NET471_OR_GREATER
            if (!RuntimeInformation.IsOSPlatform(OSPlatform.OSX))
#endif
            {
                dbf.Unlock(position, length);
            }
        }

        private static void Lock(FileStream dbf, int position, int length)
        {
#if NETSTANDARD || NETCOREAPP || NET5_0_OR_GREATER || NET471_OR_GREATER
            if (!RuntimeInformation.IsOSPlatform(OSPlatform.OSX))
#endif
            {
                dbf.Lock(position, length);
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

                using (var db = new SQLiteConnection(file)) {
                    db.CreateTable<TileRecord>();
                    db.CreateTable<TileDataRecord>();
                }
            } catch (Exception ex) {
                WriteDebugLine("CreateEmptyDB: " + ex);
                ret = false;
            }

            return ret;
        }

        public static bool PreAllocateDB(string file, int addSizeInMBytes)
        {
            bool ret = true;

            try {
                Debug.WriteLine("PreAllocateDB: " + file + ", +" + addSizeInMBytes + "MB");

                using (var db = new SQLiteConnection(file)) {
                    string sql = string.Format("create table if not exists large (a); insert into large values (zeroblob({0})); drop table large;", addSizeInMBytes * 1024 * 1024);
                    db.Execute(sql);
                }
            } catch (Exception ex) {
                WriteDebugLine("PreAllocateDB: " + ex);
                ret = false;
            }

            return ret;
        }

        private static bool AlterDBAddTimeColumn(string file)
        {
            bool ret = true;

            try {
                if (File.Exists(file)) {
                    using (var db = new SQLiteConnection(file)) {
                        try {
                            db.Query<TileRecord>("SELECT CacheTime FROM Tiles LIMIT 1");
                        } catch (Exception ex) {
                            if (ex.Message.Contains("no such column: CacheTime")) {
                                db.Execute("ALTER TABLE Tiles ADD CacheTime DATETIME");
                            } else {
                                throw;
                            }
                        }
                    }
                } else {
                    ret = false;
                }
            } catch (Exception ex) {
                WriteDebugLine("AlterDBAddTimeColumn: " + ex);
                ret = false;
            }

            return ret;
        }

#endregion

        private void RebuildFinalSelect()
        {
            fFinalSqlSelect = SingleSqlSelect;
            fAttachSqlQuery = string.Empty;
            fDetachSqlQuery = string.Empty;

            int i = 1;
            foreach (var c in fAttachedCaches) {
                fFinalSqlSelect += string.Format("\nUNION SELECT Id, Tile FROM db{0}.TilesData WHERE id = (SELECT id FROM db{0}.Tiles WHERE X={{0}} AND Y={{1}} AND Zoom={{2}} AND Type={{3}})", i);
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
                    using (var db = new SQLiteConnection(fDb)) {
                        var r = new TileRecord {
                            X = pos.X,
                            Y = pos.Y,
                            Zoom = zoom,
                            Type = (uint)type,
                            CacheTime = DateTime.Now
                        };
                        db.Insert(r);
                        db.Insert(new TileDataRecord {
                            Id = r.Id,
                            Tile = tile
                        });
                    }

                    if (Interlocked.Increment(ref fPreAllocationPing) % 22 == 0) {
                        CheckPreAllocation();
                    }
                } catch (Exception ex) {
                    WriteDebugLine("PutImageToCache: " + ex);
                    ret = false;
                }
            }

            return ret;
        }

        PureImage IPureImageCache.GetImageFromCache(int type, GPoint pos, int zoom)
        {
            PureImage ret = null;
            try {
                using (var db = new SQLiteConnection(fDb)) {
                    if (!string.IsNullOrEmpty(fAttachSqlQuery)) {
                        db.Execute(fAttachSqlQuery);
                    }

                    var query = string.Format(fFinalSqlSelect, pos.X, pos.Y, zoom, type);
                    var tiles = db.Query<TileDataRecord>(query);

                    if (tiles.Count > 0) {
                        byte[] tile = tiles[0].Tile;
                        if (GMaps.TileImageProxy != null) {
                            ret = GMaps.TileImageProxy.FromArray(tile);
                        }
                    }

                    if (!string.IsNullOrEmpty(fDetachSqlQuery)) {
                        db.Execute(fDetachSqlQuery);
                    }
                }
            } catch (Exception ex) {
                WriteDebugLine("GetImageFromCache: " + ex);
                ret = null;
            }

            return ret;
        }

        int IPureImageCache.DeleteOlderThan(DateTime date, int? type)
        {
            int affectedRows = 0;
            try {
                using (var db = new SQLiteConnection(fDb)) {
                    string sql = "DELETE FROM Tiles WHERE CacheTime is not NULL and CacheTime < datetime(?)";
                    if (type.HasValue) {
                        sql += " AND Type = ?";
                        affectedRows = db.Execute(sql, date.ToString("s"), type.Value);
                    } else {
                        affectedRows = db.Execute(sql, date.ToString("s"));
                    }
                }
            } catch (Exception ex) {
                WriteDebugLine("DeleteOlderThan: " + ex);
            }

            return affectedRows;
        }

        private static void WriteDebugLine(string msg)
        {
#if OS_MSWIN
            Debug.WriteLine(msg);
#else
            Console.WriteLine(msg);
#endif
        }
    }
}

#endif
