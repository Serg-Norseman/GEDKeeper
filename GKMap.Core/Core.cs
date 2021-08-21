/*
 *  This file is part of the "GKMap".
 *  GKMap project borrowed from GMap.NET (by radioman).
 *
 *  Copyright (C) 2009-2018 by radioman (email@radioman.lt).
 *  This program is licensed under the FLAT EARTH License.
 */

using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using System.Threading;
using System.Threading.Tasks;
using GKMap.MapProviders;

namespace GKMap
{
    /// <summary>
    /// internal map control core
    /// </summary>
    internal class Core : IDisposable
    {
        private bool fFillEmptyTiles = true;
        private BackgroundWorker fInvalidator;
        private DateTime fLastTileLoadStart = DateTime.Now;
        private DateTime fLastTileLoadEnd = DateTime.Now;
        private GSize fMinOfTiles;
        private GSize fMaxOfTiles;
        private PointLatLng fPosition;
        private GPoint fPositionPixel;
        private GMapProvider fProvider;
        private bool fRaiseEmptyTileError;
        private double fScaleX = 1;
        private double fScaleY = 1;
        private GSize fSizeOfMapArea;
        private int fZoom;

        internal Dictionary<LoadTask, Exception> FailedLoads = new Dictionary<LoadTask, Exception>(new LoadTaskComparer());

        private volatile int okZoom;
        private volatile int skipOverZoom;

        private static readonly BlockingCollection<LoadTask> tileLoadQueue4 = new BlockingCollection<LoadTask>(new ConcurrentStack<LoadTask>());
        private static List<Task> fTileLoadQueue4Tasks;
        private static int fLoadWaitCount;
        private static readonly int GThreadPoolSize = 4;

        internal volatile bool IsStarted;
        public GRect TileRect;

        public GPoint RenderOffset;
        public GPoint CenterTileXYLocation;
        public GPoint CenterTileXYLocationLast;
        public GPoint DragPoint;
        public GPoint CompensationOffset;

        public GPoint MouseDown;
        public GPoint MouseCurrent;
        public GPoint MouseLastZoom;

        /// <summary>
        /// is user dragging map
        /// </summary>
        public bool IsDragging { get; set; }

        public PointLatLng? LastLocationInBounds { get; private set; }

        public TileMatrix Matrix { get; private set; }

        public AutoResetEvent RefreshEvent { get; private set; }

        public List<DrawTile> TileDrawingList { get; private set; }

        public RWLock TileDrawingListLock { get; private set; }

        public bool UpdatingBounds { get; private set; }


        internal int maxZoom = 2;
        internal int minZoom = 2;
        internal int Width;
        internal int Height;
        internal bool ZoomToArea = true;


        /// <summary>
        /// current marker position
        /// </summary>
        public PointLatLng Position
        {
            get {
                return fPosition;
            }
            set {
                fPosition = value;
                fPositionPixel = fProvider.Projection.FromLatLngToPixel(value, Zoom);

                if (IsStarted) {
                    if (!IsDragging) {
                        GoToCurrentPosition();
                    }

                    if (OnCurrentPositionChanged != null)
                        OnCurrentPositionChanged(fPosition);
                }
            }
        }

        /// <summary>
        /// current marker position in pixel coordinates
        /// </summary>
        public GPoint PositionPixel
        {
            get {
                return fPositionPixel;
            }
        }

        public GMapProvider Provider
        {
            get {
                return fProvider;
            }
            set {
                if (fProvider == null || !fProvider.Equals(value)) {
                    bool diffProjection = (fProvider == null || fProvider.Projection != value.Projection);

                    fProvider = value;

                    if (!fProvider.IsInitialized) {
                        fProvider.IsInitialized = true;
                        fProvider.OnInitialized();
                    }

                    if (fProvider.Projection != null && diffProjection) {
                        TileRect = new GRect(GPoint.Empty, fProvider.Projection.TileSize);
                        fMinOfTiles = fProvider.Projection.GetTileMatrixMinXY(Zoom);
                        fMaxOfTiles = fProvider.Projection.GetTileMatrixMaxXY(Zoom);
                        fPositionPixel = fProvider.Projection.FromLatLngToPixel(Position, Zoom);
                    }

                    if (IsStarted) {
                        CancelAsyncTasks();
                        if (diffProjection) {
                            OnMapSizeChanged(Width, Height);
                        }
                        ReloadMap();

                        if (minZoom < fProvider.MinZoom) {
                            minZoom = fProvider.MinZoom;
                        }

                        ZoomToArea = true;

                        if (fProvider.Area.HasValue && !fProvider.Area.Value.Contains(Position)) {
                            SetZoomToFitRect(fProvider.Area.Value);
                            ZoomToArea = false;
                        }

                        if (OnMapTypeChanged != null) {
                            OnMapTypeChanged(value);
                        }
                    }
                }
            }
        }

        /// <summary>
        /// gets current map view top/left coordinate, width in Lng, height in Lat
        /// </summary>
        /// <returns></returns>
        public RectLatLng ViewArea
        {
            get {
                if (fProvider.Projection != null) {
                    var p = FromLocalToLatLng(0, 0);
                    var p2 = FromLocalToLatLng(Width, Height);

                    return RectLatLng.FromLTRB(p.Lng, p.Lat, p2.Lng, p2.Lat);
                }
                return RectLatLng.Empty;
            }
        }

        /// <summary>
        /// map zoom
        /// </summary>
        public int Zoom
        {
            get {
                return fZoom;
            }
            set {
                if (fZoom != value && !IsDragging) {
                    fZoom = value;

                    fMinOfTiles = fProvider.Projection.GetTileMatrixMinXY(value);
                    fMaxOfTiles = fProvider.Projection.GetTileMatrixMaxXY(value);

                    fPositionPixel = fProvider.Projection.FromLatLngToPixel(Position, value);

                    if (IsStarted) {
                        CancelAsyncTasks();

                        Matrix.ClearLevelsBelow(fZoom - LevelsKeepInMemory);
                        Matrix.ClearLevelsAbove(fZoom + LevelsKeepInMemory);

                        lock (FailedLoads) {
                            FailedLoads.Clear();
                            fRaiseEmptyTileError = true;
                        }

                        GoToCurrentPositionOnZoom();
                        UpdateBounds();

                        if (OnMapZoomChanged != null) {
                            OnMapZoomChanged();
                        }
                    }
                }
            }
        }

        /// <summary>
        /// is polygons enabled
        /// </summary>
        public bool PolygonsEnabled = true;

        /// <summary>
        /// is routes enabled
        /// </summary>
        public bool RoutesEnabled = true;

        /// <summary>
        /// is markers enabled
        /// </summary>
        public bool MarkersEnabled = true;

        /// <summary>
        /// retry count to get tile 
        /// </summary>
        public int RetryLoadTile = 0;

        /// <summary>
        /// how many levels of tiles are staying decompresed in memory
        /// </summary>
        public int LevelsKeepInMemory = 5;

        /// <summary>
        /// occurs when current position is changed
        /// </summary>
        public event PositionChanged OnCurrentPositionChanged;

        /// <summary>
        /// occurs when tile set load is complete
        /// </summary>
        public event TileLoadComplete OnTileLoadComplete;

        /// <summary>
        /// occurs when tile set is starting to load
        /// </summary>
        public event TileLoadStart OnTileLoadStart;

        /// <summary>
        /// occurs on empty tile displayed
        /// </summary>
        public event EmptyTileError OnEmptyTileError;

        /// <summary>
        /// occurs on map drag
        /// </summary>
        public event MapDrag OnMapDrag;

        /// <summary>
        /// occurs on map zoom changed
        /// </summary>
        public event MapZoomChanged OnMapZoomChanged;

        /// <summary>
        /// occurs on map type changed
        /// </summary>
        public event MapTypeChanged OnMapTypeChanged;

        internal static int Instances;

        public bool MouseWheelZooming = false;

        internal readonly object invalidationLock = new object();
        internal DateTime lastInvalidation = DateTime.Now;


        public Core()
        {
            Provider = EmptyProvider.Instance;
            RefreshEvent = new AutoResetEvent(false);
            TileDrawingListLock = new RWLock();
            TileDrawingList = new List<DrawTile>();
            Matrix = new TileMatrix();
        }

        /// <summary>
        /// sets zoom to max to fit rect
        /// </summary>
        /// <param name="rect"></param>
        /// <returns></returns>
        public bool SetZoomToFitRect(RectLatLng rect)
        {
            int mmaxZoom = GetMaxZoomToFitRect(rect);
            if (mmaxZoom > 0) {
                PointLatLng center = new PointLatLng(rect.Lat - (rect.HeightLat / 2), rect.Lng + (rect.WidthLng / 2));
                Position = center;

                if (mmaxZoom > maxZoom) {
                    mmaxZoom = maxZoom;
                }

                if (Zoom != mmaxZoom) {
                    Zoom = mmaxZoom;
                }

                return true;
            }
            return false;
        }

        public BackgroundWorker OnMapOpen()
        {
            if (!IsStarted) {
                int x = Interlocked.Increment(ref Instances);
                Debug.WriteLine("OnMapOpen: " + x);

                IsStarted = true;

                if (x == 1) {
                    GMaps.Instance.NoMapInstances = false;
                }

                GoToCurrentPosition();

                fInvalidator = new BackgroundWorker();
                fInvalidator.WorkerSupportsCancellation = true;
                fInvalidator.WorkerReportsProgress = true;
                fInvalidator.DoWork += InvalidatorWatch;
                fInvalidator.RunWorkerAsync();
            }
            return fInvalidator;
        }

        public void OnMapClose()
        {
            Dispose();
        }

        private void InvalidatorWatch(object sender, DoWorkEventArgs e)
        {
            var w = sender as BackgroundWorker;

            TimeSpan span = TimeSpan.FromMilliseconds(111);
            int spanMs = (int)span.TotalMilliseconds;
            bool skiped = false;

            while (RefreshEvent != null && (!skiped && RefreshEvent.WaitOne() || (RefreshEvent.WaitOne(spanMs, false) || true))) {
                if (w.CancellationPending)
                    break;

                var now = DateTime.Now;
                TimeSpan delta;
                lock (invalidationLock) {
                    delta = now - lastInvalidation;
                }

                if (delta > span) {
                    lock (invalidationLock) {
                        lastInvalidation = now;
                    }
                    skiped = false;

                    w.ReportProgress(1);
                    Debug.WriteLine("Invalidate delta: " + (int)delta.TotalMilliseconds + "ms");
                } else {
                    skiped = true;
                }
            }
        }

        public void UpdateCenterTileXYLocation()
        {
            PointLatLng center = FromLocalToLatLng(Width / 2, Height / 2);
            GPoint centerPixel = fProvider.Projection.FromLatLngToPixel(center, Zoom);
            CenterTileXYLocation = fProvider.Projection.FromPixelToTileXY(centerPixel);
        }

        public void OnMapSizeChanged(int width, int height)
        {
            this.Width = width;
            this.Height = height;

            fSizeOfMapArea.Width = 1 + (Width / fProvider.Projection.TileSize.Width) / 2;
            fSizeOfMapArea.Height = 1 + (Height / fProvider.Projection.TileSize.Height) / 2;

            Debug.WriteLine("OnMapSizeChanged, w: " + width + ", h: " + height + ", size: " + fSizeOfMapArea);

            if (IsStarted) {
                UpdateBounds();
                GoToCurrentPosition();
            }
        }

        /// <summary>
        /// gets lat/lng from local control coordinates
        /// </summary>
        /// <param name="x"></param>
        /// <param name="y"></param>
        /// <returns></returns>
        public PointLatLng FromLocalToLatLng(long x, long y)
        {
            GPoint p = new GPoint(x, y);
            p.OffsetNegative(RenderOffset);
            p.Offset(CompensationOffset);

            return fProvider.Projection.FromPixelToLatLng(p, Zoom);
        }

        /// <summary>
        /// return local coordinates from lat/lng
        /// </summary>
        /// <param name="latlng"></param>
        /// <returns></returns>
        public GPoint FromLatLngToLocal(PointLatLng latlng)
        {
            GPoint pLocal = fProvider.Projection.FromLatLngToPixel(latlng, Zoom);
            pLocal.Offset(RenderOffset);
            pLocal.OffsetNegative(CompensationOffset);
            return pLocal;
        }

        /// <summary>
        /// gets max zoom level to fit rectangle
        /// </summary>
        /// <param name="rect"></param>
        /// <returns></returns>
        public int GetMaxZoomToFitRect(RectLatLng rect)
        {
            int zoom = minZoom;

            if (rect.HeightLat == 0 || rect.WidthLng == 0) {
                zoom = maxZoom / 2;
            } else {
                for (int i = zoom; i <= maxZoom; i++) {
                    GPoint p1 = fProvider.Projection.FromLatLngToPixel(rect.LocationTopLeft, i);
                    GPoint p2 = fProvider.Projection.FromLatLngToPixel(rect.LocationRightBottom, i);

                    if (((p2.X - p1.X) <= Width + 10) && (p2.Y - p1.Y) <= Height + 10) {
                        zoom = i;
                    } else {
                        break;
                    }
                }
            }

            return zoom;
        }

        /// <summary>
        /// initiates map dragging
        /// </summary>
        /// <param name="pt"></param>
        public void BeginDrag(GPoint pt)
        {
            DragPoint.X = pt.X - RenderOffset.X;
            DragPoint.Y = pt.Y - RenderOffset.Y;
            IsDragging = true;
        }

        /// <summary>
        /// ends map dragging
        /// </summary>
        public void EndDrag()
        {
            IsDragging = false;
            MouseDown = GPoint.Empty;

            RefreshEvent.Set();
        }

        /// <summary>
        /// reloads map
        /// </summary>
        public void ReloadMap()
        {
            if (IsStarted) {
                Debug.WriteLine("------------------");

                okZoom = 0;
                skipOverZoom = 0;

                CancelAsyncTasks();

                Matrix.ClearAllLevels();

                lock (FailedLoads) {
                    FailedLoads.Clear();
                    fRaiseEmptyTileError = true;
                }

                RefreshEvent.Set();

                UpdateBounds();
            } else {
                throw new Exception("Please, do not call ReloadMap before form is loaded, it's useless");
            }
        }

        /// <summary>
        /// moves current position into map center
        /// </summary>
        public void GoToCurrentPosition()
        {
            CompensationOffset = fPositionPixel;

            // reset stuff
            RenderOffset = GPoint.Empty;
            DragPoint = GPoint.Empty;

            var d = new GPoint(Width / 2, Height / 2);
            Drag(d);
        }

        /// <summary>
        /// moves current position into map center
        /// </summary>
        internal void GoToCurrentPositionOnZoom()
        {
            CompensationOffset = fPositionPixel;

            // reset stuff
            RenderOffset = GPoint.Empty;
            DragPoint = GPoint.Empty;

            // goto location and centering
            if (MouseWheelZooming) {
                GPoint pt = new GPoint(-(fPositionPixel.X - Width / 2), -(fPositionPixel.Y - Height / 2));
                pt.Offset(CompensationOffset);
                RenderOffset.X = pt.X - DragPoint.X;
                RenderOffset.Y = pt.Y - DragPoint.Y;
            } else {
                // use current map center
                MouseLastZoom = GPoint.Empty;

                GPoint pt = new GPoint(-(fPositionPixel.X - Width / 2), -(fPositionPixel.Y - Height / 2));
                pt.Offset(CompensationOffset);
                RenderOffset.X = pt.X - DragPoint.X;
                RenderOffset.Y = pt.Y - DragPoint.Y;
            }

            UpdateCenterTileXYLocation();
        }

        /// <summary>
        /// drag map by offset in pixels
        /// </summary>
        /// <param name="offset"></param>
        public void DragOffset(GPoint offset)
        {
            RenderOffset.Offset(offset);

            UpdateCenterTileXYLocation();

            if (CenterTileXYLocation != CenterTileXYLocationLast) {
                CenterTileXYLocationLast = CenterTileXYLocation;
                UpdateBounds();
            }

            LastLocationInBounds = Position;
            IsDragging = true;
            Position = FromLocalToLatLng(Width / 2, Height / 2);
            IsDragging = false;

            if (OnMapDrag != null) {
                OnMapDrag();
            }
        }

        /// <summary>
        /// drag map
        /// </summary>
        /// <param name="pt"></param>
        public void Drag(GPoint pt)
        {
            RenderOffset.X = pt.X - DragPoint.X;
            RenderOffset.Y = pt.Y - DragPoint.Y;

            UpdateCenterTileXYLocation();

            if (CenterTileXYLocation != CenterTileXYLocationLast) {
                CenterTileXYLocationLast = CenterTileXYLocation;
                UpdateBounds();
            }

            if (IsDragging) {
                LastLocationInBounds = Position;
                Position = FromLocalToLatLng(Width / 2, Height / 2);

                if (OnMapDrag != null) {
                    OnMapDrag();
                }
            }
        }

        /// <summary>
        /// cancels tile loaders and bounds checker
        /// </summary>
        public void CancelAsyncTasks()
        {
            if (IsStarted) {
            }
        }

        private void AddLoadTask(LoadTask t)
        {
            if (fTileLoadQueue4Tasks == null) {
                lock (tileLoadQueue4) {
                    if (fTileLoadQueue4Tasks == null) {
                        fTileLoadQueue4Tasks = new List<Task>();

                        while (fTileLoadQueue4Tasks.Count < GThreadPoolSize) {
                            Debug.WriteLine("creating ProcessLoadTask: " + fTileLoadQueue4Tasks.Count);

                            fTileLoadQueue4Tasks.Add(Task.Factory.StartNew(delegate {
                                string ctid = "ProcessLoadTask[" + Thread.CurrentThread.ManagedThreadId + "]";
                                Thread.CurrentThread.Name = ctid;

                                Debug.WriteLine(ctid + ": started");
                                do {
                                    if (tileLoadQueue4.Count == 0) {
                                        Debug.WriteLine(ctid + ": ready");

                                        if (Interlocked.Increment(ref fLoadWaitCount) >= GThreadPoolSize) {
                                            Interlocked.Exchange(ref fLoadWaitCount, 0);
                                            OnLoadComplete(ctid);
                                        }
                                    }
                                    ProcessLoadTask(tileLoadQueue4.Take(), ctid);
                                }
                                while (!tileLoadQueue4.IsAddingCompleted);

                                Debug.WriteLine(ctid + ": exit");

                            }, TaskCreationOptions.LongRunning));
                        }
                    }
                }
            }
            tileLoadQueue4.Add(t);
        }

        private static void ProcessLoadTask(LoadTask task, string ctid)
        {
            try {
                if (task.Core == null || task.Core.Matrix == null)
                    return;

                var m = task.Core.Matrix.GetTileWithReadLock(task.Zoom, task.Pos);
                if (!m.NotEmpty) {
                    Debug.WriteLine(ctid + " - try load: " + task);

                    Tile t = new Tile(task.Zoom, task.Pos);

                    foreach (var tl in task.Core.fProvider.Overlays) {
                        int retry = 0;
                        do {
                            PureImage img = null;
                            Exception ex = null;

                            if (task.Zoom >= task.Core.fProvider.MinZoom && (!task.Core.fProvider.MaxZoom.HasValue || task.Zoom <= task.Core.fProvider.MaxZoom)) {
                                if (task.Core.skipOverZoom == 0 || task.Zoom <= task.Core.skipOverZoom) {
                                    // tile number inversion(BottomLeft -> TopLeft)
                                    if (tl.InvertedAxisY) {
                                        img = GMaps.Instance.GetImageFrom(tl, new GPoint(task.Pos.X, task.Core.fMaxOfTiles.Height - task.Pos.Y), task.Zoom, out ex);
                                    } else {
                                        img = GMaps.Instance.GetImageFrom(tl, task.Pos, task.Zoom, out ex);
                                    }
                                }
                            }

                            if (img != null && ex == null) {
                                if (task.Core.okZoom < task.Zoom) {
                                    task.Core.okZoom = task.Zoom;
                                    task.Core.skipOverZoom = 0;
                                    Debug.WriteLine("skipOverZoom disabled, okZoom: " + task.Core.okZoom);
                                }
                            } else if (ex != null) {
                                if ((task.Core.skipOverZoom != task.Core.okZoom) && (task.Zoom > task.Core.okZoom)) {
                                    if (ex.Message.Contains("(404) Not Found")) {
                                        task.Core.skipOverZoom = task.Core.okZoom;
                                        Debug.WriteLine("skipOverZoom enabled: " + task.Core.skipOverZoom);
                                    }
                                }
                            }

                            // check for parent tiles if not found
                            if (img == null && task.Core.okZoom > 0 && task.Core.fFillEmptyTiles && task.Core.Provider.Projection is MercatorProjection) {
                                int zoomOffset = task.Zoom > task.Core.okZoom ? task.Zoom - task.Core.okZoom : 1;
                                long Ix = 0;
                                GPoint parentTile = GPoint.Empty;

                                while (img == null && zoomOffset < task.Zoom) {
                                    Ix = (long)Math.Pow(2, zoomOffset);
                                    parentTile = new GPoint((task.Pos.X / Ix), (task.Pos.Y / Ix));
                                    img = GMaps.Instance.GetImageFrom(tl, parentTile, task.Zoom - zoomOffset++, out ex);
                                }

                                if (img != null) {
                                    // offsets in quadrant
                                    long xoff = Math.Abs(task.Pos.X - (parentTile.X * Ix));
                                    long yoff = Math.Abs(task.Pos.Y - (parentTile.Y * Ix));

                                    img.IsParent = true;
                                    img.Ix = Ix;
                                    img.Xoff = xoff;
                                    img.Yoff = yoff;
                                }
                            }

                            if (img != null) {
                                Debug.WriteLine(ctid + " - tile loaded: " + img.Data.Length / 1024 + "KB, " + task);
                                t.AddOverlay(img);
                                break;
                            } else {
                                if (ex != null) {
                                    lock (task.Core.FailedLoads) {
                                        if (!task.Core.FailedLoads.ContainsKey(task)) {
                                            task.Core.FailedLoads.Add(task, ex);

                                            if (task.Core.OnEmptyTileError != null) {
                                                if (!task.Core.fRaiseEmptyTileError) {
                                                    task.Core.fRaiseEmptyTileError = true;
                                                    task.Core.OnEmptyTileError(task.Zoom, task.Pos);
                                                }
                                            }
                                        }
                                    }
                                }

                                if (task.Core.RetryLoadTile > 0) {
                                    Debug.WriteLine(ctid + " - ProcessLoadTask: " + task + " -> empty tile, retry " + retry);
                                    Thread.Sleep(1111);
                                }
                            }
                        }
                        while (++retry < task.Core.RetryLoadTile);
                    }

                    if (t.HasAnyOverlays && task.Core.IsStarted) {
                        task.Core.Matrix.SetTile(t);
                    } else {
                        t.Dispose();
                    }
                }
            } catch (Exception ex) {
                Debug.WriteLine(ctid + " - ProcessLoadTask: " + ex);
            } finally {
                if (task.Core.RefreshEvent != null) {
                    task.Core.RefreshEvent.Set();
                }
            }
        }

        private void OnLoadComplete(string ctid)
        {
            fLastTileLoadEnd = DateTime.Now;
            long lastTileLoadTimeMs = (long)(fLastTileLoadEnd - fLastTileLoadStart).TotalMilliseconds;

            #region -- clear stuff--
            if (IsStarted) {
                TileDrawingListLock.AcquireReaderLock();
                try {
                    Matrix.ClearLevelAndPointsNotIn(Zoom, TileDrawingList);
                } finally {
                    TileDrawingListLock.ReleaseReaderLock();
                }
            }
            #endregion

            UpdateGroundResolution();
#if UseGC
            GC.Collect();
            GC.WaitForPendingFinalizers();
            GC.Collect();
#endif
            Debug.WriteLine(ctid + " - OnTileLoadComplete: " + lastTileLoadTimeMs + "ms");

            if (OnTileLoadComplete != null) {
                OnTileLoadComplete(lastTileLoadTimeMs);
            }
        }

        /// <summary>
        /// updates map bounds
        /// </summary>
        private void UpdateBounds()
        {
            if (!IsStarted || fProvider.Equals(EmptyProvider.Instance)) {
                return;
            }

            UpdatingBounds = true;

            TileDrawingListLock.AcquireWriterLock();
            try {
                #region -- find tiles around --
                TileDrawingList.Clear();

                for (long i = (int)Math.Floor(-fSizeOfMapArea.Width * fScaleX), countI = (int)Math.Ceiling(fSizeOfMapArea.Width * fScaleX); i <= countI; i++) {
                    for (long j = (int)Math.Floor(-fSizeOfMapArea.Height * fScaleY), countJ = (int)Math.Ceiling(fSizeOfMapArea.Height * fScaleY); j <= countJ; j++) {
                        GPoint p = CenterTileXYLocation;
                        p.X += i;
                        p.Y += j;

                        if (p.X >= fMinOfTiles.Width && p.Y >= fMinOfTiles.Height && p.X <= fMaxOfTiles.Width && p.Y <= fMaxOfTiles.Height) {
                            DrawTile dt = new DrawTile() {
                                PosXY = p,
                                PosPixel = new GPoint(p.X * TileRect.Width, p.Y * TileRect.Height),
                                DistanceSqr = (CenterTileXYLocation.X - p.X) * (CenterTileXYLocation.X - p.X) + (CenterTileXYLocation.Y - p.Y) * (CenterTileXYLocation.Y - p.Y)
                            };

                            if (!TileDrawingList.Contains(dt)) {
                                TileDrawingList.Add(dt);
                            }
                        }
                    }
                }

                TileDrawingList.Sort();
                #endregion
            } finally {
                TileDrawingListLock.ReleaseWriterLock();
            }

            Interlocked.Exchange(ref fLoadWaitCount, 0);

            TileDrawingListLock.AcquireReaderLock();
            try {
                foreach (DrawTile p in TileDrawingList) {
                    LoadTask task = new LoadTask(p.PosXY, Zoom, this);
                    AddLoadTask(task);
                }
            } finally {
                TileDrawingListLock.ReleaseReaderLock();
            }

            fLastTileLoadStart = DateTime.Now;
            Debug.WriteLine("OnTileLoadStart - at zoom " + Zoom + ", time: " + fLastTileLoadStart.TimeOfDay);
            UpdatingBounds = false;

            if (OnTileLoadStart != null) {
                OnTileLoadStart();
            }
        }

        /// <summary>
        /// updates ground resolution info
        /// </summary>
        private void UpdateGroundResolution()
        {
            //double rez = fProvider.Projection.GetGroundResolution(Zoom, Position.Lat);
        }

        #region IDisposable Members

        ~Core()
        {
            Dispose(false);
        }

        void Dispose(bool disposing)
        {
            if (IsStarted) {
                if (fInvalidator != null) {
                    fInvalidator.CancelAsync();
                    fInvalidator.DoWork -= InvalidatorWatch;
                    fInvalidator.Dispose();
                    fInvalidator = null;
                }

                if (RefreshEvent != null) {
                    RefreshEvent.Set();
                    RefreshEvent.Close();
                    RefreshEvent = null;
                }

                int x = Interlocked.Decrement(ref Instances);
                Debug.WriteLine("OnMapClose: " + x);

                CancelAsyncTasks();
                IsStarted = false;

                if (Matrix != null) {
                    Matrix.Dispose();
                    Matrix = null;
                }

                if (FailedLoads != null) {
                    lock (FailedLoads) {
                        FailedLoads.Clear();
                        fRaiseEmptyTileError = false;
                    }
                    FailedLoads = null;
                }

                TileDrawingListLock.AcquireWriterLock();
                try {
                    TileDrawingList.Clear();
                } finally {
                    TileDrawingListLock.ReleaseWriterLock();
                }

                if (TileDrawingListLock != null) {
                    TileDrawingListLock.Dispose();
                    TileDrawingListLock = null;
                    TileDrawingList = null;
                }

                if (x == 0) {
#if DEBUG
                    GMaps.Instance.CancelTileCaching();
#endif
                    GMaps.Instance.NoMapInstances = true;
                    GMaps.Instance.WaitForCache.Set();
                }
            }
        }

        public void Dispose()
        {
            Dispose(true);
            GC.SuppressFinalize(this);
        }

        #endregion
    }
}
