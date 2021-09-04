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
using System.Security.Cryptography;
using System.Text;
using System.Threading;
using GKMap.CacheProviders;
using GKMap.MapProviders;

namespace GKMap
{
    public enum CacheType
    {
        GeocoderCache,
        PlacemarkCache,
        UrlCache,
    }

    /// <summary>
    /// maps manager
    /// </summary>
    public class GMaps : Singleton<GMaps>
    {
        private volatile bool fAbortCacheLoop;
        private Thread fCacheThread;
        private bool fCacheExists = true;
        private volatile bool fCacheOnIdleRead = true;
        private string fImageCacheLocation;
        private int fReadingCache;
        private readonly Queue<CacheQueueItem> fTileCacheQueue = new Queue<CacheQueueItem>();

        internal readonly AutoResetEvent WaitForCache = new AutoResetEvent(false);
        internal volatile bool NoMapInstances = false;

        private static bool fCacheDelay;
        private static string fCacheLocation;


        /// <summary>
        /// primary cache provider, by default: ultra fast SQLite
        /// </summary>
        public IPureImageCache ImageCache { get; private set; }

        public string ImageCacheLocation
        {
            get {
                return fImageCacheLocation;
            }
            set {
                fImageCacheLocation = value;

                var cache = ImageCache as SQLitePureImageCache;
                if (cache != null) {
                    cache.CacheLocation = value;
                }

                fCacheDelay = true;
            }
        }

        public static string CacheLocation
        {
            get {
                if (string.IsNullOrEmpty(fCacheLocation)) {
                    ResetCacheLocation();
                }

                return fCacheLocation;
            }
            set {
                if (string.IsNullOrEmpty(value)) { // setting to null resets to default
                    ResetCacheLocation();
                } else {
                    fCacheLocation = value;
                }

                if (fCacheDelay) {
                    Instance.ImageCacheLocation = fCacheLocation;
                }
            }
        }

        /// <summary>
        /// MemoryCache provider
        /// </summary>
        public readonly MemoryCache MemoryCache = new MemoryCache();

        /// <summary>
        /// internal proxy for image management
        /// </summary>
        internal static PureImageProxy TileImageProxy;


        public GMaps()
        {
            if (Instance != null) {
                throw (new Exception("You have tried to create a new singleton class where you should have instanced it. Replace your \"new class()\" with \"class.Instance\""));
            }

            ServicePointManager.DefaultConnectionLimit = 5;
            InitCache();
        }

        private void InitCache()
        {
            ImageCache = new SQLitePureImageCache();

            string newCache = CacheLocation;
            string oldCache = Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData) + Path.DirectorySeparatorChar + "GKMap" + Path.DirectorySeparatorChar;

            // move database to non-roaming user directory
            if (Directory.Exists(oldCache)) {
                try {
                    if (Directory.Exists(newCache)) {
                        Directory.Delete(oldCache, true);
                    } else {
                        Directory.Move(oldCache, newCache);
                    }
                    ImageCacheLocation = newCache;
                } catch (Exception ex) {
                    ImageCacheLocation = oldCache;
                    Trace.WriteLine("SQLitePureImageCache, moving data: " + ex);
                }
            } else {
                ImageCacheLocation = newCache;
            }
        }

        private static readonly SHA1CryptoServiceProvider HashProvider = new SHA1CryptoServiceProvider();

        private static void ConvertToHash(ref string s)
        {
            s = BitConverter.ToString(HashProvider.ComputeHash(Encoding.Unicode.GetBytes(s)));
        }

        public void SaveContent(string url, CacheType type, string content)
        {
            if (!fCacheExists)
                return;

            try {
                ConvertToHash(ref url);

                string dir = Path.Combine(fImageCacheLocation, type.ToString()) + Path.DirectorySeparatorChar;

                // recreate dir
                if (!Directory.Exists(dir)) {
                    Directory.CreateDirectory(dir);
                }

                string file = dir + url + ".txt";

                using (StreamWriter writer = new StreamWriter(file, false, Encoding.UTF8)) {
                    writer.Write(content);
                }
            } catch (Exception ex) {
                Debug.WriteLine("SaveContent: " + ex);
            }
        }

        public string GetContent(string url, CacheType type, TimeSpan stayInCache)
        {
            if (!fCacheExists)
                return string.Empty;

            string ret = null;

            try {
                ConvertToHash(ref url);

                string dir = Path.Combine(fImageCacheLocation, type.ToString()) + Path.DirectorySeparatorChar;
                string file = dir + url + ".txt";

                if (File.Exists(file)) {
                    var writeTime = File.GetLastWriteTime(file);
                    if (DateTime.Now - writeTime < stayInCache) {
                        using (StreamReader r = new StreamReader(file, Encoding.UTF8)) {
                            ret = r.ReadToEnd();
                        }
                    } else {
                        File.Delete(file);
                    }
                }
            } catch (Exception ex) {
                ret = null;
                Debug.WriteLine("GetContent: " + ex);
            }

            return ret;
        }

        public string GetContent(string url, CacheType type)
        {
            return GetContent(url, type, TimeSpan.FromDays(88));
        }

        private static void ResetCacheLocation()
        {
            string appDataLocation = Stuff.GetApplicationDataFolderPath();

            if (string.IsNullOrEmpty(appDataLocation)) {
                Instance.fCacheExists = false;
            } else {
                CacheLocation = appDataLocation;
            }
        }

        /// <summary>
        /// </summary>
        public static void Initialize(PureImageProxy imageProxy)
        {
            TileImageProxy = imageProxy;

            // triggers dynamic SQLite loading, call this before you use SQLite for other reasons than caching maps
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

                    if (fCacheThread != null && fCacheThread.IsAlive) {
                        WaitForCache.Set();
                    } else if (fCacheThread == null || fCacheThread.ThreadState == System.Threading.ThreadState.Stopped || fCacheThread.ThreadState == System.Threading.ThreadState.Unstarted) {
                        fCacheThread = null;
                        fCacheThread = new Thread(CacheThreadLoop);
                        fCacheThread.Name = "CacheEngine";
                        fCacheThread.IsBackground = false;
                        fCacheThread.Priority = ThreadPriority.Lowest;

                        fAbortCacheLoop = false;
                        fCacheThread.Start();
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
        /// live for cache
        /// </summary>
        private void CacheThreadLoop()
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
                        var taskVal = task.Value;
                        if (taskVal.Img != null) {
                            Debug.WriteLine("CacheEngine[" + left + "]: storing tile " + taskVal + ", " + taskVal.Img.Length / 1024 + "kB...");

                            if (ImageCache != null) {
                                if (fCacheOnIdleRead) {
                                    while (Interlocked.Decrement(ref fReadingCache) > 0) {
                                        Thread.Sleep(1000);
                                    }
                                }
                                ImageCache.PutImageToCache(taskVal.Img, taskVal.Tile.Type, taskVal.Tile.Pos, taskVal.Tile.Zoom);
                            }

                            taskVal.Clear();

                            // boost cache engine
                            Thread.Sleep(333);
                        } else {
                            Debug.WriteLine("CacheEngineLoop: skip, tile disposed to early -> " + taskVal);
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
                if (m != null && TileImageProxy != null) {
                    ret = TileImageProxy.FromArray(m);
                }

                if (ret == null) {
                    if (ImageCache != null) {
                        // hold writer for 5s
                        if (fCacheOnIdleRead) {
                            Interlocked.Exchange(ref fReadingCache, 5);
                        }

                        ret = ImageCache.GetImageFromCache(provider.DbId, pos, zoom);
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
