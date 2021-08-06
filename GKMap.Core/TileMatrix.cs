/*
 *  This file is part of the "GKMap".
 *  GKMap project borrowed from GMap.NET (by radioman).
 *
 *  Copyright (C) 2009-2018 by radioman (email@radioman.lt).
 *  This program is licensed under the FLAT EARTH License.
 */

using System;
using System.Collections.Generic;

namespace GKMap
{
    /// <summary>
    /// matrix for tiles
    /// </summary>
    internal class TileMatrix : IDisposable
    {
        private List<Dictionary<GPoint, Tile>> fLevels = new List<Dictionary<GPoint, Tile>>(33);
        private RWLock fLock = new RWLock();
        private List<KeyValuePair<GPoint, Tile>> fTemp = new List<KeyValuePair<GPoint, Tile>>(44);

        public TileMatrix()
        {
            for (int i = 0; i < fLevels.Capacity; i++) {
                fLevels.Add(new Dictionary<GPoint, Tile>(55, new GPointComparer()));
            }
        }

        public void ClearAllLevels()
        {
            fLock.AcquireWriterLock();
            try {
                foreach (var matrix in fLevels) {
                    foreach (var t in matrix) {
                        t.Value.Dispose();
                    }
                    matrix.Clear();
                }
            } finally {
                fLock.ReleaseWriterLock();
            }
        }

        public void ClearLevel(int zoom)
        {
            fLock.AcquireWriterLock();
            try {
                if (zoom < fLevels.Count) {
                    var l = fLevels[zoom];

                    foreach (var t in l) {
                        t.Value.Dispose();
                    }

                    l.Clear();
                }
            } finally {
                fLock.ReleaseWriterLock();
            }
        }

        public void ClearLevelAndPointsNotIn(int zoom, List<DrawTile> list)
        {
            fLock.AcquireWriterLock();
            try {
                if (zoom < fLevels.Count) {
                    var l = fLevels[zoom];

                    fTemp.Clear();

                    foreach (var t in l) {
                        if (!list.Exists(p => p.PosXY == t.Key)) {
                            fTemp.Add(t);
                        }
                    }

                    foreach (var r in fTemp) {
                        l.Remove(r.Key);
                        r.Value.Dispose();
                    }

                    fTemp.Clear();
                }
            } finally {
                fLock.ReleaseWriterLock();
            }
        }

        public void ClearLevelsBelow(int zoom)
        {
            fLock.AcquireWriterLock();
            try {
                if (zoom - 1 < fLevels.Count) {
                    for (int i = zoom - 1; i >= 0; i--) {
                        var l = fLevels[i];

                        foreach (var t in l) {
                            t.Value.Dispose();
                        }

                        l.Clear();
                    }
                }
            } finally {
                fLock.ReleaseWriterLock();
            }
        }

        public void ClearLevelsAbove(int zoom)
        {
            fLock.AcquireWriterLock();
            try {
                if (zoom + 1 < fLevels.Count) {
                    for (int i = zoom + 1; i < fLevels.Count; i++) {
                        var l = fLevels[i];

                        foreach (var t in l) {
                            t.Value.Dispose();
                        }

                        l.Clear();
                    }
                }
            } finally {
                fLock.ReleaseWriterLock();
            }
        }

        public void EnterReadLock()
        {
            fLock.AcquireReaderLock();
        }

        public void LeaveReadLock()
        {
            fLock.ReleaseReaderLock();
        }

        public Tile GetTileWithNoLock(int zoom, GPoint p)
        {
            Tile ret;
            fLevels[zoom].TryGetValue(p, out ret);

            return ret;
        }

        public Tile GetTileWithReadLock(int zoom, GPoint p)
        {
            Tile ret;

            fLock.AcquireReaderLock();
            try {
                ret = GetTileWithNoLock(zoom, p);
            } finally {
                fLock.ReleaseReaderLock();
            }

            return ret;
        }

        public void SetTile(Tile t)
        {
            fLock.AcquireWriterLock();
            try {
                if (t.Zoom < fLevels.Count) {
                    fLevels[t.Zoom][t.Pos] = t;
                }
            } finally {
                fLock.ReleaseWriterLock();
            }
        }

        ~TileMatrix()
        {
            Dispose(false);
        }

        void Dispose(bool disposing)
        {
            if (fLock != null) {
                if (disposing) {
                    ClearAllLevels();
                }

                fLevels.Clear();
                fLevels = null;

                fTemp.Clear();
                fTemp = null;

                fLock.Dispose();
                fLock = null;
            }
        }

        public void Dispose()
        {
            this.Dispose(true);
            GC.SuppressFinalize(this);
        }
    }
}
