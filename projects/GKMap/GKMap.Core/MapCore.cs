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
using GKMap.MapObjects;
using GKMap.MapProviders;

namespace GKMap
{
    /// <summary>
    /// internal map control core
    /// </summary>
    internal class MapCore : IDisposable
    {
        private BackgroundWorker fInvalidator;
        private bool? fIsRunningOnMono;
        private DateTime fLastTileLoadStart = DateTime.Now;
        private DateTime fLastTileLoadEnd = DateTime.Now;
        private bool fLazyEvents = true;
        private RectLatLng? fLazySetZoomToFitRect;
        private int fLoadWaitCount;
        private GSize fMinOfTiles;
        private GSize fMaxOfTiles;
        private bool fMouseIn;
        private PointLatLng fPosition;
        private GPoint fPositionPixel;
        private GMapProvider fProvider;
        private GSize fSizeOfMapArea;
        private readonly int fThreadPoolSize = 4;
        private readonly BlockingCollection<LoadTask> fTileLoadQueue4 = new BlockingCollection<LoadTask>(new ConcurrentStack<LoadTask>());
        private readonly object fTileLoadQueue4Lock = new object();
        private List<Task> fTileLoadQueue4Tasks;
        private readonly IMapControl fView;
        private int fZoom;

        private volatile int okZoom;
        private volatile int skipOverZoom;

        private static int fInstances;

        public GRect TileRect;
        public GPoint RenderOffset;
        public GPoint CenterTileXYLocation;
        public GPoint CenterTileXYLocationLast;
        public GPoint DragPoint;
        public GPoint CompensationOffset;

        public bool InvertedMouseWheelZooming { get; set; }

        /// <summary>
        /// is user dragging map
        /// </summary>
        public bool IsDragging { get; set; }

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

        public TileMatrix Matrix { get; private set; }

        public GPoint MouseDown { get; set; }

        public GPoint MouseLastZoom { get; set; }

        public bool MouseIn
        {
            get { return fMouseIn; }
            set { fMouseIn = value; }
        }

        public AutoResetEvent RefreshEvent { get; private set; }

        public List<DrawTile> TileDrawingList { get; private set; }

        public RWLock TileDrawingListLock { get; private set; }

        public bool UpdatingBounds { get; private set; }


        internal Dictionary<LoadTask, Exception> FailedLoads = new Dictionary<LoadTask, Exception>(new LoadTaskComparer());
        internal readonly object FailedLoadsLock = new object();
        internal volatile bool IsStarted;
        internal readonly object InvalidationLock = new object();
        internal DateTime LastInvalidation = DateTime.Now;
        internal bool MouseWheelZooming = false;
        internal int MaxZoom = 2;
        internal int MinZoom = 2;
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

                        if (MinZoom < fProvider.MinZoom) {
                            MinZoom = fProvider.MinZoom;
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
                    int newZoom;
                    if (value > MaxZoom) {
                        newZoom = MaxZoom;
                    } else if (value < MinZoom) {
                        newZoom = MinZoom;
                    } else {
                        newZoom = value;
                    }
                    if (fZoom == newZoom) return;

                    fZoom = newZoom;

                    fMinOfTiles = fProvider.Projection.GetTileMatrixMinXY(value);
                    fMaxOfTiles = fProvider.Projection.GetTileMatrixMaxXY(value);
                    fPositionPixel = fProvider.Projection.FromLatLngToPixel(Position, value);

                    if (IsStarted) {
                        CancelAsyncTasks();

                        Matrix.ClearLevelsBelow(fZoom - LevelsKeepInMemory);
                        Matrix.ClearLevelsAbove(fZoom + LevelsKeepInMemory);

                        lock (FailedLoads) {
                            FailedLoads.Clear();
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
        /// retry count to get tile 
        /// </summary>
        public int RetryLoadTile = 0;

        /// <summary>
        /// how many levels of tiles are staying decompressed in memory
        /// </summary>
        public int LevelsKeepInMemory = 5;


        /// <summary>
        /// occurs when current position is changed
        /// </summary>
        public event PositionChanged OnCurrentPositionChanged;

        /// <summary>
        /// occurs on map zoom changed
        /// </summary>
        public event MapZoomChanged OnMapZoomChanged;

        /// <summary>
        /// occurs on map type changed
        /// </summary>
        public event MapTypeChanged OnMapTypeChanged;

        /// <summary>
        /// occurs on mouse enters marker area
        /// </summary>
        public event MarkerEnter OnMarkerEnter;

        /// <summary>
        /// occurs on mouse leaves marker area
        /// </summary>
        public event MarkerLeave OnMarkerLeave;

        /// <summary>
        /// occurs on mouse enters Polygon area
        /// </summary>
        public event PolygonEnter OnPolygonEnter;

        /// <summary>
        /// occurs on mouse leaves Polygon area
        /// </summary>
        public event PolygonLeave OnPolygonLeave;

        /// <summary>
        /// occurs on mouse enters route area
        /// </summary>
        public event RouteEnter OnRouteEnter;

        /// <summary>
        /// occurs on mouse leaves route area
        /// </summary>
        public event RouteLeave OnRouteLeave;


        public MapCore(IMapControl view)
        {
            fView = view;

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
            if (fLazyEvents) {
                fLazySetZoomToFitRect = rect;
            } else {
                int maxZoom = GetMaxZoomToFitRect(rect);
                if (maxZoom > 0) {
                    PointLatLng center = new PointLatLng(rect.Lat - (rect.HeightLat / 2), rect.Lng + (rect.WidthLng / 2));
                    fView.Position = center;

                    if (maxZoom > MaxZoom) {
                        maxZoom = MaxZoom;
                    }

                    if (Zoom != maxZoom) {
                        fView.Zoom = maxZoom;
                    }

                    return true;
                }
            }
            return false;
        }

        public void ResetZoomToFitRect()
        {
            if (fLazyEvents) {
                fLazyEvents = false;

                if (fLazySetZoomToFitRect.HasValue) {
                    SetZoomToFitRect(fLazySetZoomToFitRect.Value);
                    fLazySetZoomToFitRect = null;
                }
            }
        }

        public BackgroundWorker OnMapOpen()
        {
            if (!IsStarted) {
                int x = Interlocked.Increment(ref fInstances);
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

            while (RefreshEvent != null && (!skiped && RefreshEvent.WaitOne() || RefreshEvent.WaitOne(spanMs, false))) {
                if (w.CancellationPending)
                    break;

                var now = DateTime.Now;
                TimeSpan delta;
                lock (InvalidationLock) {
                    delta = now - LastInvalidation;
                }

                if (delta > span) {
                    lock (InvalidationLock) {
                        LastInvalidation = now;
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

            var tileSize = fProvider.Projection.TileSize;
            fSizeOfMapArea = new GSize(1 + (Width / tileSize.Width) / 2, 1 + (Height / tileSize.Height) / 2);

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
            int zoom = MinZoom;

            if (rect.HeightLat == 0 || rect.WidthLng == 0) {
                zoom = MaxZoom / 2;
            } else {
                for (int i = zoom; i <= MaxZoom; i++) {
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

            Debug.WriteLine("IsDragging = " + IsDragging);
        }

        /// <summary>
        /// ends map dragging
        /// </summary>
        public void EndDrag()
        {
            IsDragging = false;
            MouseDown = GPoint.Empty;
            RefreshEvent.Set();

            Debug.WriteLine("IsDragging = " + IsDragging);
        }

        /// <summary>
        /// reloads map
        /// </summary>
        public void ReloadMap()
        {
            if (IsStarted) {
                okZoom = 0;
                skipOverZoom = 0;

                CancelAsyncTasks();

                Matrix.ClearAllLevels();

                lock (FailedLoads) {
                    FailedLoads.Clear();
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

            Drag(Width / 2, Height / 2);
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
            if (!MouseWheelZooming) {
                // use current map center
                MouseLastZoom = GPoint.Empty;
            }

            GPoint pt = new GPoint(-(fPositionPixel.X - Width / 2), -(fPositionPixel.Y - Height / 2));
            pt.Offset(CompensationOffset);
            RenderOffset.X = pt.X - DragPoint.X;
            RenderOffset.Y = pt.Y - DragPoint.Y;

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

            IsDragging = true;
            Position = FromLocalToLatLng(Width / 2, Height / 2);
            IsDragging = false;

            ForceUpdateOverlays();
        }

        /// <summary>
        /// drag map
        /// </summary>
        public void Drag(long x, long y)
        {
            RenderOffset.X = x - DragPoint.X;
            RenderOffset.Y = y - DragPoint.Y;

            UpdateCenterTileXYLocation();

            if (CenterTileXYLocation != CenterTileXYLocationLast) {
                CenterTileXYLocationLast = CenterTileXYLocation;
                UpdateBounds();
            }

            if (IsDragging) {
                Position = FromLocalToLatLng(Width / 2, Height / 2);
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
            lock (fTileLoadQueue4Lock) {
                if (fTileLoadQueue4Tasks == null) {
                    fTileLoadQueue4Tasks = new List<Task>();

                    while (fTileLoadQueue4Tasks.Count < fThreadPoolSize) {
                        Debug.WriteLine("creating ProcessLoadTask: " + fTileLoadQueue4Tasks.Count);

                        fTileLoadQueue4Tasks.Add(Task.Factory.StartNew(delegate {
                            string ctid = "ProcessLoadTask[" + Thread.CurrentThread.ManagedThreadId + "]";
                            Thread.CurrentThread.Name = ctid;

                            Debug.WriteLine(ctid + ": started");
                            do {
                                if (fTileLoadQueue4.Count == 0) {
                                    Debug.WriteLine(ctid + ": ready");

                                    if (Interlocked.Increment(ref fLoadWaitCount) >= fThreadPoolSize) {
                                        Interlocked.Exchange(ref fLoadWaitCount, 0);
                                        OnLoadComplete(ctid);
                                    }
                                }
                                ProcessLoadTask(fTileLoadQueue4.Take(), ctid);
                            } while (!fTileLoadQueue4.IsAddingCompleted);

                            Debug.WriteLine(ctid + ": exit");

                        }, TaskCreationOptions.LongRunning));
                    }
                }
            }
            fTileLoadQueue4.Add(t);
        }

        private void ProcessLoadTask(LoadTask task, string ctid)
        {
            try {
                if (Matrix == null)
                    return;

                var m = Matrix.GetTileWithReadLock(task.Zoom, task.Pos);
                if (!m.NotEmpty) {
                    Debug.WriteLine(ctid + " - try load: " + task);

                    Tile t = new Tile(task.Zoom, task.Pos);

                    var providerOverlays = fProvider.Overlays;

                    if (providerOverlays != null) foreach (var tl in providerOverlays) {
                        int retry = 0;
                        do {
                            PureImage img = null;
                            Exception ex = null;

                            if (task.Zoom >= fProvider.MinZoom && (!fProvider.MaxZoom.HasValue || task.Zoom <= fProvider.MaxZoom)) {
                                if (skipOverZoom == 0 || task.Zoom <= skipOverZoom) {
                                    // tile number inversion(BottomLeft -> TopLeft)
                                    if (tl.InvertedAxisY) {
                                        img = GMaps.Instance.GetImageFrom(tl, new GPoint(task.Pos.X, fMaxOfTiles.Height - task.Pos.Y), task.Zoom, out ex);
                                    } else {
                                        img = GMaps.Instance.GetImageFrom(tl, task.Pos, task.Zoom, out ex);
                                    }
                                }
                            }

                            if (img != null && ex == null) {
                                if (okZoom < task.Zoom) {
                                    okZoom = task.Zoom;
                                    skipOverZoom = 0;
                                    Debug.WriteLine("skipOverZoom disabled, okZoom: " + okZoom);
                                }
                            } else if (ex != null) {
                                if ((skipOverZoom != okZoom) && (task.Zoom > okZoom)) {
                                    if (ex.Message.Contains("(404) Not Found")) {
                                        skipOverZoom = okZoom;
                                        Debug.WriteLine("skipOverZoom enabled: " + skipOverZoom);
                                    }
                                }
                            }

                            // check for parent tiles if not found
                            if (img == null && okZoom > 0 && fProvider.Projection is MercatorProjection) {
                                int zoomOffset = task.Zoom > okZoom ? task.Zoom - okZoom : 1;
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
                                    lock (FailedLoads) {
                                        if (!FailedLoads.ContainsKey(task)) {
                                            FailedLoads.Add(task, ex);
                                        }
                                    }
                                }

                                if (RetryLoadTile > 0) {
                                    Debug.WriteLine(ctid + " - ProcessLoadTask: " + task + " -> empty tile, retry " + retry);
                                    Thread.Sleep(1111);
                                }
                            }
                        }
                        while (++retry < RetryLoadTile);
                    }

                    if (t.HasAnyOverlays && IsStarted) {
                        Matrix.SetTile(t);
                    } else {
                        t.Dispose();
                    }
                }
            } catch (Exception ex) {
                Debug.WriteLine(ctid + " - ProcessLoadTask: " + ex);
            } finally {
                if (RefreshEvent != null) {
                    RefreshEvent.Set();
                }
            }
        }

        private void OnLoadComplete(string ctid)
        {
            fLastTileLoadEnd = DateTime.Now;
            long lastTileLoadTimeMs = (long)(fLastTileLoadEnd - fLastTileLoadStart).TotalMilliseconds;

            if (IsStarted) {
                TileDrawingListLock.AcquireReaderLock();
                try {
                    Matrix.ClearLevelAndPointsNotIn(Zoom, TileDrawingList);
                } finally {
                    TileDrawingListLock.ReleaseReaderLock();
                }
            }

#if UseGC
            GC.Collect();
            GC.WaitForPendingFinalizers();
            GC.Collect();
#endif
            Debug.WriteLine(ctid + " - OnTileLoadComplete: " + lastTileLoadTimeMs + "ms");

            fView.Refresh();
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
                TileDrawingList.Clear();

                for (long i = -fSizeOfMapArea.Width, countI = fSizeOfMapArea.Width; i <= countI; i++) {
                    for (long j = -fSizeOfMapArea.Height, countJ = fSizeOfMapArea.Height; j <= countJ; j++) {
                        GPoint p = CenterTileXYLocation;
                        p.X += i;
                        p.Y += j;

                        if (p.X >= fMinOfTiles.Width && p.Y >= fMinOfTiles.Height && p.X <= fMaxOfTiles.Width && p.Y <= fMaxOfTiles.Height) {
                            DrawTile dt = new DrawTile(
                                p, new GPoint(p.X * TileRect.Width, p.Y * TileRect.Height),
                                (CenterTileXYLocation.X - p.X) * (CenterTileXYLocation.X - p.X) + (CenterTileXYLocation.Y - p.Y) * (CenterTileXYLocation.Y - p.Y)
                            );

                            if (!TileDrawingList.Contains(dt)) {
                                TileDrawingList.Add(dt);
                            }
                        }
                    }
                }

                TileDrawingList.Sort();
            } finally {
                TileDrawingListLock.ReleaseWriterLock();
            }

            Interlocked.Exchange(ref fLoadWaitCount, 0);

            TileDrawingListLock.AcquireReaderLock();
            try {
                foreach (DrawTile p in TileDrawingList) {
                    LoadTask task = new LoadTask(p.PosXY, Zoom);
                    AddLoadTask(task);
                }
            } finally {
                TileDrawingListLock.ReleaseReaderLock();
            }

            fLastTileLoadStart = DateTime.Now;
            Debug.WriteLine("OnTileLoadStart - at zoom " + Zoom + ", time: " + fLastTileLoadStart.TimeOfDay);
            UpdatingBounds = false;
        }

        ~MapCore()
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

                int x = Interlocked.Decrement(ref fInstances);
                Debug.WriteLine("OnMapClose: " + x);

                CancelAsyncTasks();
                IsStarted = false;

                if (Matrix != null) {
                    Matrix.Dispose();
                    Matrix = null;
                }

                if (FailedLoads != null) {
                    lock (FailedLoadsLock) {
                        FailedLoads.Clear();
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

        internal void ProcessOverlaysMouseMove(int eX, int eY)
        {
            GPoint rp = new GPoint(eX, eY);
            rp.OffsetNegative(RenderOffset);

            for (int i = fView.Overlays.Count - 1; i >= 0; i--) {
                MapOverlay o = fView.Overlays[i];
                if (o == null || !o.IsVisible || !o.IsHitTestVisible)
                    continue;

                foreach (MapMarker m in o.Markers) {
                    if (!m.IsVisible || !m.IsHitTestVisible)
                        continue;

                    if (m.IsInside((int)rp.X, (int)rp.Y)) {
                        if (!m.IsMouseOver) {
                            fView.SetCursorHandOnEnter();
                            m.IsMouseOver = true;
                            fView.IsMouseOverMarker = true;

                            var markerEnter = OnMarkerEnter;
                            if (markerEnter != null) {
                                markerEnter(m);
                            }

                            fView.Refresh();
                        }
                    } else if (m.IsMouseOver) {
                        m.IsMouseOver = false;
                        fView.IsMouseOverMarker = false;
                        fView.RestoreCursorOnLeave();

                        var markerLeave = OnMarkerLeave;
                        if (markerLeave != null) {
                            markerLeave(m);
                        }

                        fView.Refresh();
                    }
                }

                foreach (MapRoute m in o.Routes) {
                    if (!m.IsVisible || !m.IsHitTestVisible)
                        continue;

                    if (m.IsInside((int)rp.X, (int)rp.Y)) {
                        if (!m.IsMouseOver) {
                            fView.SetCursorHandOnEnter();
                            m.IsMouseOver = true;
                            fView.IsMouseOverRoute = true;

                            var routeEnter = OnRouteEnter;
                            if (routeEnter != null) {
                                routeEnter(m);
                            }

                            fView.Refresh();
                        }
                    } else {
                        if (m.IsMouseOver) {
                            m.IsMouseOver = false;
                            fView.IsMouseOverRoute = false;
                            fView.RestoreCursorOnLeave();

                            var routeLeave = OnRouteLeave;
                            if (routeLeave != null) {
                                routeLeave(m);
                            }

                            fView.Refresh();
                        }
                    }
                }

                foreach (MapPolygon m in o.Polygons) {
                    if (!m.IsVisible || !m.IsHitTestVisible)
                        continue;

                    if (m.IsInside((int)rp.X, (int)rp.Y)) {
                        if (!m.IsMouseOver) {
                            fView.SetCursorHandOnEnter();
                            m.IsMouseOver = true;
                            fView.IsMouseOverPolygon = true;

                            var polygonEnter = OnPolygonEnter;
                            if (polygonEnter != null) {
                                polygonEnter(m);
                            }

                            fView.Refresh();
                        }
                    } else {
                        if (m.IsMouseOver) {
                            m.IsMouseOver = false;
                            fView.IsMouseOverPolygon = false;
                            fView.RestoreCursorOnLeave();

                            var polygonLeave = OnPolygonLeave;
                            if (polygonLeave != null) {
                                polygonLeave(m);
                            }

                            fView.Refresh();
                        }
                    }
                }
            }
        }

        /// <summary>
        /// updates markers local position
        /// </summary>
        /// <param name="marker"></param>
        public void UpdateMarkerLocalPosition(MapMarker marker)
        {
            GPoint p = FromLatLngToLocal(marker.Position);
            p.OffsetNegative(RenderOffset);
            marker.LocalPosition = new GPoint((int)(p.X + marker.Offset.X), (int)(p.Y + marker.Offset.Y));
        }

        /// <summary>
        /// updates routes local position
        /// </summary>
        /// <param name="route"></param>
        public void UpdateRouteLocalPosition(MapRoute route)
        {
            route.LocalPoints.Clear();

            for (int i = 0; i < route.Points.Count; i++) {
                GPoint p = FromLatLngToLocal(route.Points[i]);
                p.OffsetNegative(RenderOffset);
                route.LocalPoints.Add(p);
            }
            route.UpdateGraphicsPath();
        }

        /// <summary>
        /// updates polygons local position
        /// </summary>
        /// <param name="polygon"></param>
        public void UpdatePolygonLocalPosition(MapPolygon polygon)
        {
            polygon.LocalPoints.Clear();

            for (int i = 0; i < polygon.Points.Count; i++) {
                GPoint p = FromLatLngToLocal(polygon.Points[i]);
                p.OffsetNegative(RenderOffset);
                polygon.LocalPoints.Add(p);
            }
            polygon.UpdateGraphicsPath();
        }

        /// <summary>
        /// set current position using keywords
        /// </summary>
        /// <param name="keys"></param>
        /// <returns>true if successful</returns>
        public GeocoderStatusCode SetPositionByKeywords(string keys)
        {
            var status = GeocoderStatusCode.Unknown;

            var gp = Provider as IGeocodingProvider;
            if (gp == null) {
                gp = GMapProviders.OpenStreetMap;
            }

            if (gp != null) {
                var pt = gp.GetPoint(keys, out status);
                if (status == GeocoderStatusCode.Success && pt.HasValue) {
                    Position = pt.Value;
                }
            }

            return status;
        }

        /// <summary>
        /// gets rect of route or polygon
        /// </summary>
        /// <param name="figure"></param>
        /// <returns></returns>
        public RectLatLng? GetRectOfFigure(MapFigure figure)
        {
            RectLatLng? ret = null;

            double left = double.MaxValue;
            double top = double.MinValue;
            double right = double.MinValue;
            double bottom = double.MaxValue;

            if (figure.HasLines) {
                foreach (PointLatLng p in figure.Points) {
                    // left
                    if (p.Lng < left) {
                        left = p.Lng;
                    }

                    // top
                    if (p.Lat > top) {
                        top = p.Lat;
                    }

                    // right
                    if (p.Lng > right) {
                        right = p.Lng;
                    }

                    // bottom
                    if (p.Lat < bottom) {
                        bottom = p.Lat;
                    }
                }
                ret = RectLatLng.FromLTRB(left, top, right, bottom);
            }
            return ret;
        }

        /// <summary>
        /// gets rectangle with all objects inside
        /// </summary>
        /// <param name="overlayId">overlay id or null to check all except zoomInsignificant</param>
        /// <returns></returns>
        public RectLatLng? GetRectOfAllMarkers(string overlayId)
        {
            RectLatLng? ret = null;

            double left = double.MaxValue;
            double top = double.MinValue;
            double right = double.MinValue;
            double bottom = double.MaxValue;

            foreach (MapOverlay o in fView.Overlays) {
                if (((overlayId == null && o.IsZoomSignificant) || o.Id == overlayId) && o.IsVisible && o.Markers.Count > 0) {
                    foreach (MapMarker m in o.Markers) {
                        if (!m.IsVisible) continue;

                        // left
                        if (m.Position.Lng < left) {
                            left = m.Position.Lng;
                        }

                        // top
                        if (m.Position.Lat > top) {
                            top = m.Position.Lat;
                        }

                        // right
                        if (m.Position.Lng > right) {
                            right = m.Position.Lng;
                        }

                        // bottom
                        if (m.Position.Lat < bottom) {
                            bottom = m.Position.Lat;
                        }
                    }
                }
            }

            if (left != double.MaxValue && right != double.MinValue && top != double.MinValue && bottom != double.MaxValue) {
                ret = RectLatLng.FromLTRB(left, top, right, bottom);
            }

            return ret;
        }

        /// <summary>
        /// gets rectangle with all objects inside
        /// </summary>
        /// <param name="overlayId">overlay id or null to check all except zoomInsignificant</param>
        /// <returns></returns>
        public RectLatLng? GetRectOfAllRoutes(string overlayId)
        {
            RectLatLng? ret = null;

            double left = double.MaxValue;
            double top = double.MinValue;
            double right = double.MinValue;
            double bottom = double.MaxValue;

            foreach (MapOverlay o in fView.Overlays) {
                if (((overlayId == null && o.IsZoomSignificant) || o.Id == overlayId) && o.IsVisible && o.Routes.Count > 0) {
                    foreach (MapRoute route in o.Routes) {
                        if (!route.IsVisible || !route.HasLines) continue;

                        foreach (PointLatLng p in route.Points) {
                            // left
                            if (p.Lng < left) {
                                left = p.Lng;
                            }

                            // top
                            if (p.Lat > top) {
                                top = p.Lat;
                            }

                            // right
                            if (p.Lng > right) {
                                right = p.Lng;
                            }

                            // bottom
                            if (p.Lat < bottom) {
                                bottom = p.Lat;
                            }
                        }
                    }
                }
            }

            if (left != double.MaxValue && right != double.MinValue && top != double.MinValue && bottom != double.MaxValue) {
                ret = RectLatLng.FromLTRB(left, top, right, bottom);
            }

            return ret;
        }

        public void ProcessMouseWheel(int eX, int eY, int eDelta)
        {
            if (fMouseIn && !IsDragging) {
                if (MouseLastZoom.X != eX && MouseLastZoom.Y != eY) {
                    fView.Position = FromLocalToLatLng(eX, eY);
                    MouseLastZoom = new GPoint(eX, eY);
                }

                fView.SetMousePositionToMapCenter();

                MouseWheelZooming = true;

                if (eDelta > 0) {
                    if (!InvertedMouseWheelZooming) {
                        fView.Zoom = Zoom + 1;
                    } else {
                        fView.Zoom = ((int)(Zoom + 0.99)) - 1;
                    }
                } else if (eDelta < 0) {
                    if (!InvertedMouseWheelZooming) {
                        fView.Zoom = ((int)(Zoom + 0.99)) - 1;
                    } else {
                        fView.Zoom = Zoom + 1;
                    }
                }

                MouseWheelZooming = false;
            }
        }

        public void ProcessMouseClick(int eX, int eY, EventArgs e)
        {
            if (IsDragging) return;

            GPoint rp = new GPoint(eX, eY);
            rp.OffsetNegative(RenderOffset);

            for (int i = fView.Overlays.Count - 1; i >= 0; i--) {
                MapOverlay o = fView.Overlays[i];
                if (o == null || !o.IsVisible || !o.IsHitTestVisible) continue;

                foreach (MapMarker m in o.Markers) {
                    if (m.IsVisible && m.IsHitTestVisible && m.IsInside((int)rp.X, (int)rp.Y)) {
                        fView.DoMouseClick(m, e);
                        break;
                    }
                }

                foreach (MapRoute m in o.Routes) {
                    if (m.IsVisible && m.IsHitTestVisible && m.IsInside((int)rp.X, (int)rp.Y)) {
                        fView.DoMouseClick(m, e);
                        break;
                    }
                }

                foreach (MapPolygon m in o.Polygons) {
                    if (m.IsVisible && m.IsHitTestVisible && m.IsInsideLatLng(FromLocalToLatLng(eX, eY))) {
                        fView.DoMouseClick(m, e);
                        break;
                    }
                }
            }
        }

        public void ProcessMouseDoubleClick(int eX, int eY, EventArgs e)
        {
            if (IsDragging) return;

            GPoint rp = new GPoint(eX, eY);
            rp.OffsetNegative(RenderOffset);

            for (int i = fView.Overlays.Count - 1; i >= 0; i--) {
                MapOverlay o = fView.Overlays[i];
                if (o == null || !o.IsVisible || !o.IsHitTestVisible) continue;

                foreach (MapMarker m in o.Markers) {
                    if (m.IsVisible && m.IsHitTestVisible && m.IsInside((int)rp.X, (int)rp.Y)) {
                        fView.DoMouseDoubleClick(m, e);
                        break;
                    }
                }

                foreach (MapRoute m in o.Routes) {
                    if (m.IsVisible && m.IsHitTestVisible && m.IsInside((int)rp.X, (int)rp.Y)) {
                        fView.DoMouseDoubleClick(m, e);
                        break;
                    }
                }

                foreach (MapPolygon m in o.Polygons) {
                    if (m.IsVisible && m.IsHitTestVisible && m.IsInsideLatLng(FromLocalToLatLng(eX, eY))) {
                        fView.DoMouseDoubleClick(m, e);
                        break;
                    }
                }
            }
        }

        /// <summary>
        /// update objects when map is dragged/zoomed
        /// </summary>
        public void ForceUpdateOverlays()
        {
            try {
                fView.HoldInvalidation = true;

                foreach (var o in fView.Overlays) {
                    if (o.IsVisible) {
                        o.ForceUpdate();
                    }
                }
            } finally {
                fView.Refresh();
            }
        }

        /// <summary>
        /// sets to max zoom to fit all markers and centers them in map
        /// </summary>
        /// <param name="overlayId">overlay id or null to check all</param>
        /// <returns></returns>
        public bool ZoomAndCenterMarkers(string overlayId)
        {
            RectLatLng? rect = GetRectOfAllMarkers(overlayId);
            if (rect.HasValue) {
                return SetZoomToFitRect(rect.Value);
            }
            return false;
        }

        /// <summary>
        /// zooms and centers all route
        /// </summary>
        /// <param name="overlayId">overlay id or null to check all</param>
        /// <returns></returns>
        public bool ZoomAndCenterRoutes(string overlayId)
        {
            RectLatLng? rect = GetRectOfAllRoutes(overlayId);
            if (rect.HasValue) {
                return SetZoomToFitRect(rect.Value);
            }
            return false;
        }

        /// <summary>
        /// zooms and centers route 
        /// </summary>
        /// <param name="figure"></param>
        /// <returns></returns>
        public bool ZoomAndCenterFigure(MapFigure figure)
        {
            RectLatLng? rect = GetRectOfFigure(figure);
            if (rect.HasValue) {
                return SetZoomToFitRect(rect.Value);
            }
            return false;
        }

        public void DrawMap(object g)
        {
            if (UpdatingBounds || Equals(fProvider, EmptyProvider.Instance) || fProvider == null) {
                Debug.WriteLine("Core.updatingBounds");
                return;
            }

            TileDrawingListLock.AcquireReaderLock();
            Matrix.EnterReadLock();

            try {
                foreach (var tilePoint in TileDrawingList) {
                    TileRect.Location = tilePoint.PosPixel;
                    TileRect.OffsetNegative(CompensationOffset);

                    bool found = false;

                    Tile t = Matrix.GetTileWithNoLock(Zoom, tilePoint.PosXY);
                    if (t.NotEmpty) {
                        // render tile
                        foreach (var pureImage in t.Overlays) {
                            fView.DrawTile(g, pureImage, ref found);
                        }
                    } else if (fProvider.Projection is MercatorProjection) {
                        // filling empty tiles using lower level images
                        int zoomOffset = 1;
                        Tile parentTile = Tile.Empty;
                        long Ix = 0;

                        while (!parentTile.NotEmpty && zoomOffset < Zoom && zoomOffset <= LevelsKeepInMemory) {
                            Ix = (long)Math.Pow(2, zoomOffset);
                            parentTile = Matrix.GetTileWithNoLock(Zoom - zoomOffset++,
                                new GPoint((int)(tilePoint.PosXY.X / Ix), (int)(tilePoint.PosXY.Y / Ix)));
                        }

                        if (parentTile.NotEmpty) {
                            long xoff = Math.Abs(tilePoint.PosXY.X - (parentTile.Pos.X * Ix));
                            long yoff = Math.Abs(tilePoint.PosXY.Y - (parentTile.Pos.Y * Ix));

                            // render tile 
                            foreach (var pureImage in parentTile.Overlays) {
                                fView.DrawLowerTile(g, pureImage, Ix, xoff, yoff, ref found);
                            }
                        }
                    }

                    // add text if tile is missing
                    if (!found) {
                        lock (FailedLoads) {
                            var lt = new LoadTask(tilePoint.PosXY, Zoom);
                            if (FailedLoads.ContainsKey(lt)) {
                                var ex = FailedLoads[lt];
                                fView.DrawMissingTile(g, ex);
                            }
                        }
                    }

                    fView.ShowTileGridLines(g, tilePoint);
                }
            } finally {
                Matrix.LeaveReadLock();
                TileDrawingListLock.ReleaseReaderLock();
            }
        }
    }
}
