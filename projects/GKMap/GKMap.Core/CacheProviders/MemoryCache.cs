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

namespace GKMap.CacheProviders
{
    public class MemoryCache : IDisposable
    {
        private readonly Dictionary<RawTile, byte[]> fTileCache;
        private RWLock fCacheLock = new RWLock();
        private long fMemoryCacheSize;
        private readonly Queue<RawTile> fQueue = new Queue<RawTile>();

        /// <summary>
        /// the amount of tiles in MB to keep in memory, default: 22MB, if each ~100Kb it's ~222 tiles
        /// </summary>
        private const int MemoryCacheCapacity = 22;

        /// <summary>
        /// current memory cache size in MB
        /// </summary>
        private double MemoryCacheSize
        {
            get {
                return fMemoryCacheSize / 1048576.0;
            }
        }

        public MemoryCache()
        {
            fTileCache = new Dictionary<RawTile, byte[]>(new RawTileComparer());
        }

        public void Clear()
        {
            fCacheLock.AcquireWriterLock();
            try {
                fQueue.Clear();
                fTileCache.Clear();
                fMemoryCacheSize = 0;
            } finally {
                fCacheLock.ReleaseWriterLock();
            }
        }

        internal byte[] GetTileFromMemoryCache(RawTile tile)
        {
            fCacheLock.AcquireReaderLock();
            try {
                byte[] ret;
                if (fTileCache.TryGetValue(tile, out ret)) {
                    return ret;
                }
            } finally {
                fCacheLock.ReleaseReaderLock();
            }
            return null;
        }

        internal void AddTileToMemoryCache(RawTile tile, byte[] data)
        {
            if (data != null) {
                fCacheLock.AcquireWriterLock();
                try {
                    if (!fTileCache.ContainsKey(tile)) {
                        fQueue.Enqueue(tile);
                        fTileCache.Add(tile, data);
                        fMemoryCacheSize += data.Length;
                    }
                } finally {
                    fCacheLock.ReleaseWriterLock();
                }
            }
        }

        internal void RemoveOverload()
        {
            fCacheLock.AcquireWriterLock();
            try {
                while (MemoryCacheSize > MemoryCacheCapacity) {
                    if (fTileCache.Keys.Count > 0 && fQueue.Count > 0) {
                        RawTile first = fQueue.Dequeue();
                        try {
                            var m = fTileCache[first];
                            fTileCache.Remove(first);
                            fMemoryCacheSize -= m.Length;
                        } catch (Exception ex) {
                            Debug.WriteLine("RemoveMemoryOverload: " + ex);
                        }
                    } else {
                        break;
                    }
                }
            } finally {
                fCacheLock.ReleaseWriterLock();
            }
        }

        ~MemoryCache()
        {
            Dispose(false);
        }

        private void Dispose(bool disposing)
        {
            if (fCacheLock != null) {
                if (disposing) {
                    Clear();
                }

                fCacheLock.Dispose();
                fCacheLock = null;
            }
        }

        public void Dispose()
        {
            Dispose(true);
            GC.SuppressFinalize(this);
        }
    }
}
