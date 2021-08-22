/*
 *  This file is part of the "GKMap".
 *  GKMap project borrowed from GMap.NET (by radioman).
 *
 *  Copyright (C) 2009-2018 by radioman (email@radioman.lt).
 *  This program is licensed under the FLAT EARTH License.
 */

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Net;
using System.Reflection;
using System.Threading;
using GKMap.CacheProviders;
using GKMap.MapProviders;

namespace GKMap
{
    /// <summary>
    /// maps manager
    /// </summary>
    public class GMaps : Singleton<GMaps>
    {
        private volatile bool fAbortCacheLoop;
        private Thread fCacheEngine;
        private volatile bool fCacheOnIdleRead = true;
        private bool? fIsRunningOnMono;
        private int fReadingCache;
        private readonly Queue<CacheQueueItem> fTileCacheQueue = new Queue<CacheQueueItem>();

        internal readonly AutoResetEvent WaitForCache = new AutoResetEvent(false);
        internal volatile bool NoMapInstances = false;

        /// <summary>
        /// tile access mode
        /// </summary>
        public bool UseTileCache = true;

        /// <summary>
        /// is map using cache for geocoder
        /// </summary>
        public bool UseGeocoderCache = true;

        /// <summary>
        /// is map using cache for placemarks
        /// </summary>
        public bool UsePlacemarkCache = true;

        /// <summary>
        /// is map using cache for other url
        /// </summary>
        public bool UseUrlCache = true;

        /// <summary>
        /// primary cache provider, by default: ultra fast SQLite
        /// </summary>
        public IPureImageCache PrimaryCache
        {
            get {
                return Cache.Instance.ImageCache;
            }
            set {
                Cache.Instance.ImageCache = value;
            }
        }

        /// <summary>
        /// MemoryCache provider
        /// </summary>
        public readonly MemoryCache MemoryCache = new MemoryCache();

        /// <summary>
        /// return true if running on mono
        /// </summary>
        /// <returns></returns>
        public bool IsRunningOnMono
        {
            get {
                if (!fIsRunningOnMono.HasValue) {
                    try {
                        fIsRunningOnMono = (Type.GetType("Mono.Runtime") != null);
                        return fIsRunningOnMono.Value;
                    } catch {
                    }
                } else {
                    return fIsRunningOnMono.Value;
                }
                return false;
            }
        }


        static GMaps()
        {
            if (GMapProvider.TileImageProxy == null) {
                try {
                    AppDomain d = AppDomain.CurrentDomain;
                    var assembliesLoaded = d.GetAssemblies();

                    Assembly l = null;

                    foreach (var a in assembliesLoaded) {
                        if (a.FullName.Contains("GKMap.WinForms") || a.FullName.Contains("GKMap.WPF")) {
                            l = a;
                            break;
                        }
                    }

                    if (l == null) {
                        var jj = Assembly.GetExecutingAssembly().Location;
                        var hh = Path.GetDirectoryName(jj);
                        var f1 = hh + Path.DirectorySeparatorChar + "GKMap.WinForms.dll";
                        var f2 = hh + Path.DirectorySeparatorChar + "GKMap.WPF.dll";
                        if (File.Exists(f1)) {
                            l = Assembly.LoadFile(f1);
                        } else if (File.Exists(f2)) {
                            l = Assembly.LoadFile(f2);
                        }
                    }

                    if (l != null) {
                        Type t = null;

                        if (l.FullName.Contains("GKMap.WinForms")) {
                            t = l.GetType("GKMap.WinForms.GMapImageProxy");
                        } else if (l.FullName.Contains("GKMap.WPF")) {
                            t = l.GetType("GKMap.WPF.GMapImageProxy");
                        }

                        if (t != null) {
                            t.InvokeMember("Enable", BindingFlags.DeclaredOnly | BindingFlags.Public | BindingFlags.Static | BindingFlags.InvokeMethod, null, null, null);
                        }
                    }
                } catch (Exception ex) {
                    Debug.WriteLine("GMaps, try set TileImageProxy failed: " + ex.Message);
                }
            }
        }

        public GMaps()
        {
            if (Instance != null) {
                throw (new Exception("You have tried to create a new singleton class where you should have instanced it. Replace your \"new class()\" with \"class.Instance\""));
            }

            ServicePointManager.DefaultConnectionLimit = 5;
        }

        /// <summary>
        /// triggers dynamic sqlite loading, 
        /// call this before you use sqlite for other reasons than caching maps
        /// </summary>
        public void SQLitePing()
        {
            SQLitePureImageCache.Ping();
        }

        /// <summary>
        /// enqueue tile to cache
        /// </summary>
        /// <param name="task"></param>
        private void EnqueueCacheTask(CacheQueueItem task)
        {
            lock (fTileCacheQueue) {
                if (!fTileCacheQueue.Contains(task)) {
                    Debug.WriteLine("EnqueueCacheTask: " + task);

                    fTileCacheQueue.Enqueue(task);

                    if (fCacheEngine != null && fCacheEngine.IsAlive) {
                        WaitForCache.Set();
                    } else if (fCacheEngine == null || fCacheEngine.ThreadState == System.Threading.ThreadState.Stopped || fCacheEngine.ThreadState == System.Threading.ThreadState.Unstarted) {
                        fCacheEngine = null;
                        fCacheEngine = new Thread(CacheEngineLoop);
                        fCacheEngine.Name = "CacheEngine";
                        fCacheEngine.IsBackground = false;
                        fCacheEngine.Priority = ThreadPriority.Lowest;

                        fAbortCacheLoop = false;
                        fCacheEngine.Start();
                    }
                }
            }
        }

        /// <summary>
        /// immediately stops background tile caching, call it if you want fast exit the process
        /// </summary>
        public void CancelTileCaching()
        {
            Debug.WriteLine("CancelTileCaching...");

            fAbortCacheLoop = true;
            lock (fTileCacheQueue) {
                fTileCacheQueue.Clear();
                WaitForCache.Set();
            }
        }

        /// <summary>
        /// live for cache ;}
        /// </summary>
        private void CacheEngineLoop()
        {
            Debug.WriteLine("CacheEngine: start");

            bool startEvent = false;

            while (!fAbortCacheLoop) {
                try {
                    CacheQueueItem? task = null;

                    int left;
                    lock (fTileCacheQueue) {
                        left = fTileCacheQueue.Count;
                        if (left > 0) {
                            task = fTileCacheQueue.Dequeue();
                        }
                    }

                    if (task.HasValue) {
                        if (startEvent) {
                            startEvent = false;
                        }

                        // check if stream wasn't disposed somehow
                        if (task.Value.Img != null) {
                            Debug.WriteLine("CacheEngine[" + left + "]: storing tile " + task.Value + ", " + task.Value.Img.Length / 1024 + "kB...");

                            if (PrimaryCache != null) {
                                if (fCacheOnIdleRead) {
                                    while (Interlocked.Decrement(ref fReadingCache) > 0) {
                                        Thread.Sleep(1000);
                                    }
                                }
                                PrimaryCache.PutImageToCache(task.Value.Img, task.Value.Tile.Type, task.Value.Tile.Pos, task.Value.Tile.Zoom);
                            }

                            task.Value.Clear();

                            // boost cache engine
                            Thread.Sleep(333);
                        } else {
                            Debug.WriteLine("CacheEngineLoop: skip, tile disposed to early -> " + task.Value);
                        }
                    } else {
                        if (!startEvent) {
                            startEvent = true;
                        }

                        if (fAbortCacheLoop || NoMapInstances || !WaitForCache.WaitOne(33333, false)) {
                            break;
                        }
                    }
                } catch (AbandonedMutexException) {
                    break;
                } catch (Exception ex) {
                    Debug.WriteLine("CacheEngineLoop: " + ex);
                }
            }
            Debug.WriteLine("CacheEngine: stop");
        }

        /// <summary>
        /// gets image from tile server
        /// </summary>
        /// <param name="provider"></param>
        /// <param name="pos"></param>
        /// <param name="zoom"></param>
        /// <returns></returns>
        public PureImage GetImageFrom(GMapProvider provider, GPoint pos, int zoom, out Exception result)
        {
            PureImage ret = null;
            result = null;

            try {
                var rawTile = new RawTile(provider.DbId, pos, zoom);

                // let't check memory first
                var m = MemoryCache.GetTileFromMemoryCache(rawTile);
                if (m != null && GMapProvider.TileImageProxy != null) {
                    ret = GMapProvider.TileImageProxy.FromArray(m);
                }

                if (ret == null) {
                    if (PrimaryCache != null) {
                        // hold writer for 5s
                        if (fCacheOnIdleRead) {
                            Interlocked.Exchange(ref fReadingCache, 5);
                        }

                        ret = PrimaryCache.GetImageFromCache(provider.DbId, pos, zoom);
                        if (ret != null) {
                            MemoryCache.AddTileToMemoryCache(rawTile, ret.Data.GetBuffer());
                            return ret;
                        }
                    }

                    ret = provider.GetTileImage(pos, zoom);
                    // Enqueue Cache
                    if (ret != null) {
                        MemoryCache.AddTileToMemoryCache(rawTile, ret.Data.GetBuffer());

                        EnqueueCacheTask(new CacheQueueItem(rawTile, ret.Data.GetBuffer()));
                    }
                }
            } catch (Exception ex) {
                result = ex;
                ret = null;
                Debug.WriteLine("GetImageFrom: " + ex);
            }

            return ret;
        }
    }
}
