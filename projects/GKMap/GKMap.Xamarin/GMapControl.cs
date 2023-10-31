/*
 *  This file is part of the "GKMap".
 *  GKMap project borrowed from GMap.NET (by radioman).
 *
 *  Copyright (C) 2009-2018 by radioman (email@radioman.lt).
 *  This program is licensed under the FLAT EARTH License.
 */

//#define DEBUG_TILE_COORDS
//#define DEBUG_CENTER
//#define DEBUG_RENDER

using System;
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using GKMap.MapObjects;
using GKMap.MapProviders;
using SkiaSharp;
using SkiaSharp.Views.Forms;

namespace GKMap.Xamarin
{
    public delegate void MarkerClick(MapMarker item, SKTouchEventArgs e);
    public delegate void MarkerDoubleClick(MapMarker item, SKTouchEventArgs e);

    public delegate void PolygonClick(MapPolygon item, SKTouchEventArgs e);
    public delegate void PolygonDoubleClick(MapPolygon item, SKTouchEventArgs e);

    public delegate void RouteClick(MapRoute item, SKTouchEventArgs e);
    public delegate void RouteDoubleClick(MapRoute item, SKTouchEventArgs e);

    /// <summary>
    /// GKMap control for Windows Forms
    /// </summary>   
    public class GMapControl : SKCanvasView, IMapControl
    {
        public static readonly bool IsDesignerHosted = LicenseManager.UsageMode == LicenseUsageMode.Designtime;

        //private SKBitmap fBackBuffer;
        private MapCore fCore;
        //private Cursor fCursorBefore = Cursors.Default;
        private bool fForceDoubleBuffer;
        //private SKCanvas fGxOff;
        private bool fIsMouseOverMarker;
        private bool fIsMouseOverPolygon;
        private bool fIsMouseOverRoute;
        private int fOverObjectCount;

        private readonly SKColor EmptyMapBackground = SKColors.WhiteSmoke;
        private readonly SKPaint EmptyTileBorders = new SKPaint() { Color = SKColors.White, StrokeWidth = 1, Style = SKPaintStyle.Stroke };
        private readonly SKPaint EmptyTileBrush = new SKPaint() { Color = SKColors.Navy, Style = SKPaintStyle.Fill };
        private readonly string EmptyTileText = "We are sorry, but we don't\nhave imagery at this zoom\nlevel for this region.";
        private readonly SKPaint MissingDataFont = new SKPaint(new SKFont(SKTypeface.FromFamilyName("Sans"), 11) { Embolden = true }) { Color = SKColors.Red };
        //private readonly SKFont ScaleFont = new SKFont(FontFamilies.SansFamilyName, 5, FontStyle.Italic);
        //private readonly ImageAttributes TileFlipXYAttributes = new ImageAttributes();

#if DEBUG_CENTER || DEBUG_RENDER
        private readonly Pen CenterPen = new Pen(Brushes.Red, 1);
        private readonly Pen ScalePen = new Pen(Brushes.Blue, 1);
#endif

        #region Properties

        /// <summary>
        /// location of cache
        /// </summary>
        [DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)]
        [Browsable(false)]
        public string CacheLocation
        {
            get {
#if !DESIGN
                return GMaps.CacheLocation;
#else
                return string.Empty;
#endif
            }
            set {
#if !DESIGN
                GMaps.CacheLocation = value;
#endif
            }
        }

        MapCore IMapControl.Core { get { return fCore; } }

        /// <summary>
        /// stops immediate marker/route/polygon invalidation;
        /// call Refresh to perform single refresh and reset invalidation state
        /// </summary>
        public bool HoldInvalidation { get; set; }

        /// <summary>
        /// reverses MouseWheel zooming direction
        /// </summary>
        public bool InvertedMouseWheelZooming
        {
            get {
                return fCore.InvertedMouseWheelZooming;
            }
            set {
                fCore.InvertedMouseWheelZooming = value;
            }
        }


        /// <summary>
        /// is mouse over marker
        /// </summary>
        [DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)]
        [Browsable(false)]
        public bool IsMouseOverMarker
        {
            get {
                return fIsMouseOverMarker;
            }
            set {
                fIsMouseOverMarker = value;
                fOverObjectCount += value ? 1 : -1;
            }
        }

        /// <summary>
        /// is mouse over polygon
        /// </summary>
        [DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)]
        [Browsable(false)]
        public bool IsMouseOverPolygon
        {
            get {
                return fIsMouseOverPolygon;
            }
            set {
                fIsMouseOverPolygon = value;
                fOverObjectCount += value ? 1 : -1;
            }
        }

        /// <summary>
        /// is mouse over route
        /// </summary>
        [DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)]
        [Browsable(false)]
        public bool IsMouseOverRoute
        {
            get {
                return fIsMouseOverRoute;
            }
            set {
                fIsMouseOverRoute = value;
                fOverObjectCount += value ? 1 : -1;
            }
        }

        [DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)]
        [Browsable(false)]
        public GMapProvider MapProvider
        {
            get {
                return fCore.Provider;
            }
            set {
                if (fCore.Provider == null || !fCore.Provider.Equals(value)) {
                    Debug.WriteLine("MapType: " + fCore.Provider.Name + " -> " + value.Name);

                    RectLatLng viewarea = fCore.ViewArea;

                    fCore.Provider = value;

                    if (fCore.IsStarted) {
                        if (fCore.ZoomToArea) {
                            // restore zoom rect as close as possible
                            if (viewarea != RectLatLng.Empty && viewarea != fCore.ViewArea) {
                                int bestZoom = fCore.GetMaxZoomToFitRect(viewarea);
                                if (bestZoom > 0 && Zoom != bestZoom) {
                                    Zoom = bestZoom;
                                }
                            }
                        } else {
                            fCore.ForceUpdateOverlays();
                        }
                    }
                }
            }
        }

        /// <summary>
        /// max zoom
        /// </summary>         
        [Category("GKMap")]
        [Description("maximum zoom level of map")]
        public int MaxZoom
        {
            get {
                return fCore.MaxZoom;
            }
            set {
                fCore.MaxZoom = value;
            }
        }

        /// <summary>
        /// min zoom
        /// </summary>      
        [Category("GKMap")]
        [Description("minimum zoom level of map")]
        public int MinZoom
        {
            get {
                return fCore.MinZoom;
            }
            set {
                fCore.MinZoom = value;
            }
        }

        /// <summary>
        /// list of overlays, should be thread safe
        /// </summary>
        public ObservableCollectionThreadSafe<MapOverlay> Overlays { get; private set; }

        /// <summary>
        /// current map center position
        /// </summary>
        [DesignerSerializationVisibility(DesignerSerializationVisibility.Hidden)]
        [Browsable(false)]
        public PointLatLng Position
        {
            get {
                return fCore.Position;
            }
            set {
                fCore.Position = value;

                if (fCore.IsStarted) {
                    fCore.ForceUpdateOverlays();
                }
            }
        }

        [Category("GKMap"), DefaultValue(0)]
        public int Zoom
        {
            get {
                return fCore.Zoom;
            }
            set {
                if (fCore.Zoom != value) {
                    Debug.WriteLine("ZoomPropertyChanged: " + fCore.Zoom + " -> " + value);

                    if (value > MaxZoom) {
                        fCore.Zoom = MaxZoom;
                    } else if (value < MinZoom) {
                        fCore.Zoom = MinZoom;
                    } else {
                        fCore.Zoom = value;
                    }

                    if (fCore.IsStarted && !fCore.IsDragging) {
                        fCore.ForceUpdateOverlays();
                    }
                }
            }
        }

        #endregion

        #region Events

        /// <summary>
        /// occurs when current position is changed
        /// </summary>
        public event PositionChanged OnPositionChanged
        {
            add {
                fCore.OnCurrentPositionChanged += value;
            }
            remove {
                fCore.OnCurrentPositionChanged -= value;
            }
        }

        /// <summary>
        /// occurs when tile set load is complete
        /// </summary>
        public event TileLoadComplete OnTileLoadComplete
        {
            add {
                fCore.OnTileLoadComplete += value;
            }
            remove {
                fCore.OnTileLoadComplete -= value;
            }
        }

        /// <summary>
        /// occurs when tile set is starting to load
        /// </summary>
        public event TileLoadStart OnTileLoadStart
        {
            add {
                fCore.OnTileLoadStart += value;
            }
            remove {
                fCore.OnTileLoadStart -= value;
            }
        }

        /// <summary>
        /// occurs on map drag
        /// </summary>
        public event MapDrag OnMapDrag
        {
            add {
                fCore.OnMapDrag += value;
            }
            remove {
                fCore.OnMapDrag -= value;
            }
        }

        /// <summary>
        /// occurs on map zoom changed
        /// </summary>
        public event MapZoomChanged OnMapZoomChanged
        {
            add {
                fCore.OnMapZoomChanged += value;
            }
            remove {
                fCore.OnMapZoomChanged -= value;
            }
        }

        /// <summary>
        /// occurs on map type changed
        /// </summary>
        public event MapTypeChanged OnMapTypeChanged
        {
            add {
                fCore.OnMapTypeChanged += value;
            }
            remove {
                fCore.OnMapTypeChanged -= value;
            }
        }

        /// <summary>
        /// occurs when clicked on marker
        /// </summary>
        public event MarkerClick OnMarkerClick;

        /// <summary>
        /// occurs when double clicked on marker
        /// </summary>
        public event MarkerDoubleClick OnMarkerDoubleClick;

        /// <summary>
        /// occurs on mouse enters marker area
        /// </summary>
        public event MarkerEnter OnMarkerEnter
        {
            add {
                fCore.OnMarkerEnter += value;
            }
            remove {
                fCore.OnMarkerEnter -= value;
            }
        }

        /// <summary>
        /// occurs on mouse leaves marker area
        /// </summary>
        public event MarkerLeave OnMarkerLeave
        {
            add {
                fCore.OnMarkerLeave += value;
            }
            remove {
                fCore.OnMarkerLeave -= value;
            }
        }

        /// <summary>
        /// occurs when clicked on polygon
        /// </summary>
        public event PolygonClick OnPolygonClick;

        /// <summary>
        /// occurs when double clicked on polygon
        /// </summary>
        public event PolygonDoubleClick OnPolygonDoubleClick;

        /// <summary>
        /// occurs on mouse enters Polygon area
        /// </summary>
        public event PolygonEnter OnPolygonEnter
        {
            add {
                fCore.OnPolygonEnter += value;
            }
            remove {
                fCore.OnPolygonEnter -= value;
            }
        }

        /// <summary>
        /// occurs on mouse leaves Polygon area
        /// </summary>
        public event PolygonLeave OnPolygonLeave
        {
            add {
                fCore.OnPolygonLeave += value;
            }
            remove {
                fCore.OnPolygonLeave -= value;
            }
        }

        /// <summary>
        /// occurs when clicked on route
        /// </summary>
        public event RouteClick OnRouteClick;

        /// <summary>
        /// occurs when double clicked on route
        /// </summary>
        public event RouteDoubleClick OnRouteDoubleClick;

        /// <summary>
        /// occurs on mouse enters route area
        /// </summary>
        public event RouteEnter OnRouteEnter
        {
            add {
                fCore.OnRouteEnter += value;
            }
            remove {
                fCore.OnRouteEnter -= value;
            }
        }

        /// <summary>
        /// occurs on mouse leaves route area
        /// </summary>
        public event RouteLeave OnRouteLeave
        {
            add {
                fCore.OnRouteLeave += value;
            }
            remove {
                fCore.OnRouteLeave -= value;
            }
        }

        #endregion


        static GMapControl()
        {
            if (!IsDesignerHosted) {
                GMaps.Initialize(GMapImageProxy.Instance);
            }
        }

#if !DESIGN
        /// <summary>
        /// constructor
        /// </summary>
        public GMapControl()
        {
            SizeChanged += OnSizeChanged;
            EnableTouchEvents = true;

            fCore = new MapCore(this);

            Overlays = new ObservableCollectionThreadSafe<MapOverlay>();

            InvertedMouseWheelZooming = false;
            MaxZoom = 17;
            MinZoom = 2;
            Zoom = 0;

            if (!IsDesignerHosted) {
                //SetStyle(ControlStyles.OptimizedDoubleBuffer, true);
                //SetStyle(ControlStyles.AllPaintingInWmPaint, true);
                //SetStyle(ControlStyles.UserPaint, true);
                //SetStyle(ControlStyles.Opaque, true);
                //ResizeRedraw = true;
                //TileFlipXYAttributes.SetWrapMode(WrapMode.TileFlipXY);

                //CenterFormat.Alignment = StringAlignment.Center;
                //CenterFormat.LineAlignment = StringAlignment.Center;

                //BottomFormat.Alignment = StringAlignment.Center;
                //BottomFormat.LineAlignment = StringAlignment.Far;

                Overlays.CollectionChanged += Overlays_CollectionChanged;
            }
        }

#endif

        /*protected override void Dispose(bool disposing)
        {
            if (disposing) {
                fCore.OnMapClose();

                Overlays.CollectionChanged -= Overlays_CollectionChanged;

                foreach (var o in Overlays) {
                    o.Dispose();
                }
                Overlays.Clear();

#if DEBUG_CENTER
                CenterPen.Dispose();
                ScalePen.Dispose();
#endif

                ScaleFont.Dispose();
                //CenterFormat.Dispose();
                //BottomFormat.Dispose();
                CopyrightFont.Dispose();
                EmptyTileBorders.Dispose();
                EmptyTileBrush.Dispose();
                ClearBackBuffer();
            }
            base.Dispose(disposing);
        }*/

        /// <summary>
        /// Call it to empty tile cache & reload tiles
        /// </summary>
        public void ReloadMap()
        {
            fCore.ReloadMap();
        }

        /// <summary>
        /// set current position using keywords
        /// </summary>
        /// <param name="keys"></param>
        /// <returns>true if successful</returns>
        public GeocoderStatusCode SetPositionByKeywords(string keys)
        {
            return fCore.SetPositionByKeywords(keys);
        }

        /// <summary>
        /// gets world coordinate from local control coordinate 
        /// </summary>
        /// <param name="x"></param>
        /// <param name="y"></param>
        /// <returns></returns>
        public PointLatLng FromLocalToLatLng(int x, int y)
        {
            return fCore.FromLocalToLatLng(x, y);
        }

        /// <summary>
        /// gets local coordinate from world coordinate
        /// </summary>
        /// <param name="point"></param>
        /// <returns></returns>
        public GPoint FromLatLngToLocal(PointLatLng point)
        {
            return fCore.FromLatLngToLocal(point);
        }

        /// <summary>
        /// call this to stop HoldInvalidation and perform single forced instant refresh 
        /// </summary>
        public /*override*/ void Refresh()
        {
            HoldInvalidation = false;

            lock (fCore.InvalidationLock) {
                fCore.LastInvalidation = DateTime.Now;
            }

            base.InvalidateSurface();
            //base.Refresh();
        }

#if !DESIGN
        /// <summary>
        /// enqueue built-in thread safe invalidation
        /// </summary>
        public void Invalidate()
        {
            if (fCore.RefreshEvent != null && !HoldInvalidation) {
                fCore.RefreshEvent.Set();
            }
        }
#endif

        /// <summary>
        /// sets to max zoom to fit all markers and centers them in map
        /// </summary>
        /// <param name="overlayId">overlay id or null to check all</param>
        /// <returns></returns>
        public bool ZoomAndCenterMarkers(string overlayId)
        {
            return fCore.ZoomAndCenterMarkers(overlayId);
        }

        /// <summary>
        /// zooms and centers all route
        /// </summary>
        /// <param name="overlayId">overlay id or null to check all</param>
        /// <returns></returns>
        public bool ZoomAndCenterRoutes(string overlayId)
        {
            return fCore.ZoomAndCenterRoutes(overlayId);
        }

        /// <summary>
        /// zooms and centers route 
        /// </summary>
        /// <param name="figure"></param>
        /// <returns></returns>
        public bool ZoomAndCenterFigure(MapFigure figure)
        {
            return fCore.ZoomAndCenterFigure(figure);
        }

        /// <summary>
        /// gets image of the current view
        /// </summary>
        /// <returns></returns>
        public SKBitmap ToImage()
        {
            SKBitmap ret = null;

            //bool r = fForceDoubleBuffer;
            try {
                /*UpdateBackBuffer();

                if (!r) {
                    fForceDoubleBuffer = true;
                }*/

                Refresh();
                //Application.DoEvents();

                /*using (MemoryStream ms = new MemoryStream()) {
                    using (var frame = (fBackBuffer.Clone() as SKBitmap)) {
                        frame.Save(ms, ImageFormat.Png);
                    }
                    ret = new SKBitmap(ms);
                }*/
            } catch (Exception) {
                throw;
            } finally {
                /*if (!r) {
                    fForceDoubleBuffer = false;
                    ClearBackBuffer();
                }*/
            }
            return ret;
        }

        /// <summary>
        /// offset position in pixels
        /// </summary>
        /// <param name="x"></param>
        /// <param name="y"></param>
        public void Offset(int x, int y)
        {
            /*if (IsHandleCreated)*/ {
                fCore.DragOffset(new GPoint(x, y));
            }
        }

        #region UserControl Events

        /*protected override void OnLoad(EventArgs e)
        {
            base.OnLoad(e);

            if (!IsDesignerHosted) {
                fCore.ResetZoomToFitRect();
                fCore.OnMapOpen().ProgressChanged += invalidatorEngage;
                fCore.ForceUpdateOverlays();
            }
        }*/

        /*protected override void OnCreateControl()
        {
            base.OnCreateControl();

            if (!IsDesignerHosted) {
                var f = ParentForm;
                if (f != null) {
                    while (f.ParentForm != null) {
                        f = f.ParentForm;
                    }

                    f.FormClosing += ParentForm_FormClosing;
                }
            }
        }*/

#if DEBUG_RENDER
        private int fCounter;
        private readonly Font fDebugFont = new Font(FontFamilies.SansFamilyName, 12, FontStyle.None);
        private DateTime fStart;
        private DateTime fEnd;
        private int fDelta;
#endif

        protected override void OnPaintSurface(SKPaintSurfaceEventArgs e)
        {
#if DEBUG_RENDER
            fStart = DateTime.Now;
#endif

            /*if (fForceDoubleBuffer) {
                if (fGxOff != null) {
                    DrawGraphics(fGxOff);
                    e.Graphics.DrawImage(fBackBuffer, 0, 0);
                }
            } else */{
                DrawGraphics(e.Surface.Canvas);
            }

#if DEBUG_RENDER
            fEnd = DateTime.Now;
            fDelta = (int)(fEnd - fStart).TotalMilliseconds;
#endif
        }

        private void DrawGraphics(SKCanvas g)
        {
            // render white background
            g.Clear(EmptyMapBackground);

            g.Save();

            g.Translate(fCore.RenderOffset.X, fCore.RenderOffset.Y);
            fCore.DrawMap(g);
            OnPaintOverlays(g);
        }

        /// <summary>
        /// override, to render something more
        /// </summary>
        /// <param name="g"></param>
        protected virtual void OnPaintOverlays(SKCanvas g)
        {
            //g.AntiAlias = true;
            //g.SmoothingMode = SmoothingMode.HighQuality;
            foreach (GMapOverlay o in Overlays) {
                if (o.IsVisible) {
                    o.OnRender(g);
                }
            }

            // center in virtual space...
#if DEBUG_CENTER
            g.DrawLine(ScalePen, -20, 0, 20, 0);
            g.DrawLine(ScalePen, 0, -20, 0, 20);
            g.DrawString("debug: virtual space center", CopyrightFont, Brushes.Blue, 2, CopyrightFont.Height);
#endif

            g.Restore();

#if DEBUG_RENDER
            // show center
            g.DrawLine(CenterPen, Width / 2 - 5, Height / 2, Width / 2 + 5, Height / 2);
            g.DrawLine(CenterPen, Width / 2, Height / 2 - 5, Width / 2, Height / 2 + 5);

            // debug info
            g.DrawString(string.Format("{0:0.0}", Zoom) + "z, " + MapProvider + ", refresh: " + fCounter++ + ", render: " + fDelta + "ms", fDebugFont, Brushes.Blue, fDebugFont.Height, fDebugFont.Height + 20);
#endif
        }

        private void OnSizeChanged(object sender, EventArgs e)
        {
            if (Width == 0 || Height == 0) {
                Debug.WriteLine("minimized");
                return;
            }

            if (Width == fCore.Width && Height == fCore.Height) {
                Debug.WriteLine("maximized");
                return;
            }

            if (!IsDesignerHosted) {
                if (fForceDoubleBuffer) {
                    //UpdateBackBuffer();
                }

                fCore.OnMapSizeChanged((int)CanvasSize.Width, (int)CanvasSize.Height);

                if (IsVisible /*&& IsHandleCreated*/ && fCore.IsStarted) {
                    fCore.ForceUpdateOverlays();
                }
            }
        }

        protected override void OnTouch(SKTouchEventArgs e)
        {

        }

        /*protected override void OnMouseDown(MouseEventArgs e)
        {
            base.OnMouseDown(e);

            SKPoint mpt = e.Location.ToPoint();
            if (!IsMouseOverMarker && e.Buttons == MouseButtons.Primary) {
                fCore.MouseDown = new GPoint(mpt.X, mpt.Y);
                Invalidate();
            }
        }*/

        /*protected override void OnMouseUp(MouseEventArgs e)
        {
            base.OnMouseUp(e);

            if (fCore.IsDragging) {
                RestoreCursor();
                fCore.EndDrag();
            } else {
                if (e.Buttons == MouseButtons.Primary) {
                    fCore.MouseDown = GPoint.Empty;
                }

                Invalidate();
            }
        }*/

        // FIXME
        /*protected override void OnMouseClick(MouseEventArgs e)
        {
            base.OnMouseClick(e);

            Point mpt = e.Location.ToPoint();
            fCore.ProcessMouseClick(mpt.X, mpt.Y, e);
        }*/

        /*protected override void OnMouseDoubleClick(MouseEventArgs e)
        {
            base.OnMouseDoubleClick(e);

            SKPoint mpt = e.Location.ToPoint();
            fCore.ProcessMouseDoubleClick(mpt.X, mpt.Y, e);
        }*/

        /*protected override void OnMouseMove(MouseEventArgs e)
        {
            base.OnMouseMove(e);

            SKPoint mpt = e.Location.ToPoint();

            if (!fCore.IsDragging && !fCore.MouseDown.IsEmpty) {
                // Gets the width and height of a rectangle centered on the point the mouse
                // button was pressed, within which a drag operation will not begin.

                // FIXME
                var dragSize = new SKSize(8, 8); //SystemInformation.DragSize;

                if (Math.Abs(mpt.X - fCore.MouseDown.X) * 2 >= dragSize.Width || Math.Abs(mpt.Y - fCore.MouseDown.Y) * 2 >= dragSize.Height) {
                    fCore.BeginDrag(fCore.MouseDown);
                }
            }

            if (fCore.IsDragging) {
                SetCursorDrag();
                fCore.MouseCurrent = new GPoint(mpt.X, mpt.Y);
                fCore.Drag(fCore.MouseCurrent);
                base.InvalidateSurface();
            } else {
                if (fCore.MouseDown.IsEmpty) {
                    fCore.ProcessOverlaysMouseMove(mpt.X, mpt.Y);
                }
            }
        }*/

        /*protected override void OnMouseEnter(MouseEventArgs e)
        {
            base.OnMouseEnter(e);

            Focus();
            fCore.MouseIn = true;
        }*/

        /*protected override void OnMouseLeave(MouseEventArgs e)
        {
            base.OnMouseLeave(e);

            fCore.MouseIn = false;
        }*/

        /*protected override void OnMouseWheel(MouseEventArgs e)
        {
            base.OnMouseWheel(e);

            SKPoint mpt = e.Location.ToPoint();
            fCore.ProcessMouseWheel(mpt.X, mpt.Y, (int)e.Delta.Height);
        }*/

        #endregion

        #region Core callbacks and aux private methods

        // TODO
        /*private void ParentForm_FormClosing(object sender, FormClosingEventArgs e)
        {
            if (e.CloseReason == CloseReason.WindowsShutDown || e.CloseReason == CloseReason.TaskManagerClosing) {
                GMaps.Instance.CancelTileCaching();
            }
        }*/

        /*private void UpdateBackBuffer()
        {
            ClearBackBuffer();

            //fBackBuffer = new SKBitmap(Width, Height, PixelFormat.Format32bppRgb);
            //fGxOff = new Graphics(fBackBuffer);
        }

        private void ClearBackBuffer()
        {
            if (fBackBuffer != null) {
                fBackBuffer.Dispose();
                fBackBuffer = null;
            }
            if (fGxOff != null) {
                fGxOff.Dispose();
                fGxOff = null;
            }
        }*/

        private void Overlays_CollectionChanged(object sender, NotifyCollectionChangedEventArgs e)
        {
            if (e.NewItems != null) {
                foreach (GMapOverlay obj in e.NewItems) {
                    if (obj != null) {
                        obj.Control = this;
                    }
                }

                if (fCore.IsStarted) {
                    Invalidate();
                }
            }
        }

        private void invalidatorEngage(object sender, ProgressChangedEventArgs e)
        {
            base.InvalidateSurface();
        }

        void IMapControl.DrawTile(object go, PureImage pureImage, ref bool found)
        {
            SKCanvas g = (SKCanvas)go;
            var img = (GMapImage)pureImage;
            if (img != null && img.Img != null) {
                if (!found)
                    found = true;

                if (!img.IsParent) {
                    SKRect dst = SKRect.Create(fCore.TileRect.X, fCore.TileRect.Y, fCore.TileRect.Width, fCore.TileRect.Height);
                    g.DrawBitmap(img.Img, dst);
                } else {
                    float wix = ((float)img.Img.Width / img.Ix);
                    float hix = ((float)img.Img.Height / img.Ix);
                    SKRect srcRect = SKRect.Create(img.Xoff * wix, img.Yoff * hix, wix, hix);
                    SKRect dst = SKRect.Create(fCore.TileRect.X, fCore.TileRect.Y, fCore.TileRect.Width, fCore.TileRect.Height);
                    g.DrawBitmap(img.Img, srcRect, dst);
                }
            }
        }

        void IMapControl.DrawLowerTile(object go, PureImage pureImage, long Ix, long xoff, long yoff, ref bool found)
        {
            SKCanvas g = (SKCanvas)go;
            var img = (GMapImage)pureImage;
            if (img != null && img.Img != null && !img.IsParent) {
                if (!found)
                    found = true;

                SKRect srcRect = SKRect.Create(xoff * (img.Img.Width / Ix),
                    yoff * (img.Img.Height / Ix), (img.Img.Width / Ix), (img.Img.Height / Ix));
                SKRect dst = SKRect.Create((int)fCore.TileRect.X, (int)fCore.TileRect.Y,
                    (int)fCore.TileRect.Width, (int)fCore.TileRect.Height);

                g.DrawBitmap(img.Img, srcRect, dst);
            }
        }

        void IMapControl.DrawMissingTile(object go, Exception ex)
        {
            SKCanvas g = (SKCanvas)go;

            g.DrawRect(SKRect.Create(fCore.TileRect.X, fCore.TileRect.Y, fCore.TileRect.Width, fCore.TileRect.Height), EmptyTileBrush);

            g.DrawText("Exception: " + ex.Message, fCore.TileRect.X + 11, fCore.TileRect.Y + 11, MissingDataFont);

            g.DrawText(EmptyTileText, fCore.TileRect.X, fCore.TileRect.Y, MissingDataFont);

            g.DrawRect(SKRect.Create(fCore.TileRect.X, fCore.TileRect.Y, fCore.TileRect.Width, fCore.TileRect.Height), EmptyTileBorders);
        }

        void IMapControl.ShowTileGridLines(object go, DrawTile tilePoint)
        {
#if DEBUG_TILE_COORDS
            Graphics g = (Graphics)go;

            // show tile grid lines
            g.DrawRectangle(EmptyTileBorders, (int)fCore.TileRect.X, (int)fCore.TileRect.Y,
                (int)fCore.TileRect.Width, (int)fCore.TileRect.Height);
            g.DrawString(
                (tilePoint.PosXY == fCore.CenterTileXYLocation ? "CENTER: " : "TILE: ") + tilePoint,
                MissingDataFont, Brushes.DimGray,
                new RectangleF(fCore.TileRect.X, fCore.TileRect.Y, fCore.TileRect.Width, fCore.TileRect.Height),
                CenterFormat);
#endif
        }

        void IMapControl.DoMouseClick(MapObject obj, EventArgs e)
        {
            /*if (obj is MapMarker) {
                if (OnMarkerClick != null) {
                    OnMarkerClick(obj as MapMarker, (MouseEventArgs)e);
                }
            } else if (obj is MapRoute) {
                if (OnRouteClick != null) {
                    OnRouteClick(obj as MapRoute, (MouseEventArgs)e);
                }
            } else if (obj is MapPolygon) {
                if (OnPolygonClick != null) {
                    OnPolygonClick(obj as MapPolygon, (MouseEventArgs)e);
                }
            }*/
        }

        void IMapControl.DoMouseDoubleClick(MapObject obj, EventArgs e)
        {
            /*if (obj is MapMarker) {
                if (OnMarkerDoubleClick != null) {
                    OnMarkerDoubleClick(obj as MapMarker, (MouseEventArgs)e);
                }
            } else if (obj is MapRoute) {
                if (OnRouteDoubleClick != null) {
                    OnRouteDoubleClick(obj as MapRoute, (MouseEventArgs)e);
                }
            } else if (obj is MapPolygon) {
                if (OnPolygonDoubleClick != null) {
                    OnPolygonDoubleClick(obj as MapPolygon, (MouseEventArgs)e);
                }
            }*/
        }

        private void SetCursorHand()
        {
            //fCursorBefore = Cursor;
            //Cursor = Cursors.Pointer;
        }

        private void SetCursorDrag()
        {
            //fCursorBefore = Cursor;
            //Cursor = Cursors.Move;
        }

        private void RestoreCursor()
        {
            //Cursor = Cursors.Arrow;
            //fCursorBefore = null;
        }

        void IMapControl.RestoreCursorOnLeave()
        {
            /*if (fOverObjectCount <= 0 && fCursorBefore != null) {
                fOverObjectCount = 0;
                RestoreCursor();
            }*/
        }

        void IMapControl.SetCursorHandOnEnter()
        {
            /*if (fOverObjectCount <= 0 && Cursor != Cursors.Pointer) {
                fOverObjectCount = 0;
                SetCursorHand();
            }*/
        }

        void IMapControl.SetMousePositionToMapCenter()
        {
            //SKPoint p = PointToScreen(new SKPoint(Width / 2, Height / 2));
            //Mouse.Position = p;
        }

        #endregion
    }
}
