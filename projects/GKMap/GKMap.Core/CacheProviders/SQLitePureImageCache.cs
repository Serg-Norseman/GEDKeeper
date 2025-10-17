/*
 *  This file is part of the "GKMap".
 *  GKMap project borrowed from GMap.NET (by radioman).
 *
 *  Copyright (C) 2009-2018 by radioman (email@radioman.lt).
 *  This program is licensed under the FLAT EARTH License.
 */

#if !MOBILE

using System;
using System.Diagnostics;
using System.IO;
using System.Runtime.InteropServices;
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

        private bool fCreated;
        private string fDb;

        static SQLitePureImageCache()
        {
#if !NETCOREAPP && !NETSTANDARD
            SQLiteLoader.Load();
#endif
        }

        public void SetCacheLocation(string cacheLocation)
        {
            string dbDir = Path.Combine(cacheLocation, "TileDBv5", GMapProvider.LanguageStr) + Path.DirectorySeparatorChar;

            // recreate dir
            if (!Directory.Exists(dbDir)) {
                Directory.CreateDirectory(dbDir);
            }

            // make empty db
            fDb = dbDir + "Data.gmdb";
            fCreated = !File.Exists(fDb) ? CreateEmptyDB() : AlterExistsDB();
        }

        private bool CreateEmptyDB()
        {
            bool ret = true;

            try {
                string dir = Path.GetDirectoryName(fDb);
                if (!Directory.Exists(dir)) {
                    Directory.CreateDirectory(dir);
                }

                using (var db = new SQLiteConnection(fDb)) {
                    db.CreateTable<TileRecord>();
                    db.CreateTable<TileDataRecord>();
                }
            } catch (Exception ex) {
                WriteDebugLine("CreateEmptyDB: " + ex);
                ret = false;
            }

            return ret;
        }

        private bool AlterExistsDB()
        {
            bool ret = true;

            try {
                using (var db = new SQLiteConnection(fDb)) {
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
            } catch (Exception ex) {
                WriteDebugLine("AlterExistsDB: " + ex);
                ret = false;
            }

            return ret;
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
            if (GMaps.TileImageProxy == null) return ret;

            try {
                using (var db = new SQLiteConnection(fDb)) {
                    var query = string.Format(SingleSqlSelect, pos.X, pos.Y, zoom, type);
                    var tiles = db.Query<TileDataRecord>(query);
                    if (tiles.Count > 0) {
                        byte[] tile = tiles[0].Tile;
                        ret = GMaps.TileImageProxy.FromArray(tile);
                    }
                }
            } catch (Exception ex) {
                WriteDebugLine("GetImageFromCache: " + ex);
                ret = null;
            }

            return ret;
        }

        private static void WriteDebugLine(string msg)
        {
            if (RuntimeInformation.IsOSPlatform(OSPlatform.Windows)) {
                Debug.WriteLine(msg);
            } else {
                Console.WriteLine(msg);
            }
        }
    }
}

#endif
