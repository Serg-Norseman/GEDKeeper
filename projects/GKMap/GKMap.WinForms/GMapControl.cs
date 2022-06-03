﻿/*
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
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Drawing.Imaging;
using System.IO;
using System.Runtime.InteropServices;
using System.Windows.Forms;
using GKMap.MapObjects;
using GKMap.MapProviders;

namespace GKMap.WinForms
{
    public delegate void MarkerClick(MapMarker item, MouseEventArgs e);
    public delegate void MarkerDoubleClick(MapMarker item, MouseEventArgs e);

    public delegate void PolygonClick(MapPolygon item, MouseEventArgs e);
    public delegate void PolygonDoubleClick(MapPolygon item, MouseEventArgs e);

    public delegate void RouteClick(MapRoute item, MouseEventArgs e);
    public delegate void RouteDoubleClick(MapRoute item, MouseEventArgs e);

    /// <summary>
    /// GKMap control for Windows Forms
    /// </summary>   
    public class GMapControl : UserControl, IMapControl
    {
        public static readonly bool IsDesignerHosted = LicenseManager.UsageMode == LicenseUsageMode.Designtime;

        private Bitmap fBackBuffer;
        private MapCore fCore;
        private Cursor fCursorBefore = Cursors.Default;
        private bool fForceDoubleBuffer;
        private Graphics fGxOff;
        private bool fIsMouseOverMarker;
        private bool fIsMouseOverPolygon;
        private bool fIsMouseOverRoute;
        private int fOverObjectCount;

        private readonly StringFormat BottomFormat = new StringFormat();
        private readonly StringFormat CenterFormat = new StringFormat();
        private readonly Font CopyrightFont = new Font(FontFamily.GenericSansSerif, 7, FontStyle.Regular);
        private readonly Color EmptyMapBackground = Color.WhiteSmoke;
        private readonly Pen EmptyTileBorders = new Pen(Brushes.White, 1);
        private readonly Brush EmptyTileBrush = new SolidBrush(Color.Navy);
        private readonly string EmptyTileText = "We are sorry, but we don't\nhave imagery at this zoom\nlevel for this region.";
        private readonly Font MissingDataFont = new Font(FontFamily.GenericSansSerif, 11, FontStyle.Bold);
        private readonly Font ScaleFont = new Font(FontFamily.GenericSansSerif, 5, FontStyle.Italic);
        private readonly ImageAttributes TileFlipXYAttributes = new ImageAttributes();

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
            fCore = new MapCore(this);

            Overlays = new ObservableCollectionThreadSafe<MapOverlay>();

            InvertedMouseWheelZooming = false;
            MaxZoom = 17;
            MinZoom = 2;
            Zoom = 0;

            if (!IsDesignerHosted) {
                SetStyle(ControlStyles.OptimizedDoubleBuffer, true);
                SetStyle(ControlStyles.AllPaintingInWmPaint, true);
                SetStyle(ControlStyles.UserPaint, true);
                SetStyle(ControlStyles.Opaque, true);
                ResizeRedraw = true;

                TileFlipXYAttributes.SetWrapMode(WrapMode.TileFlipXY);

                CenterFormat.Alignment = StringAlignment.Center;
                CenterFormat.LineAlignment = StringAlignment.Center;

                BottomFormat.Alignment = StringAlignment.Center;
                BottomFormat.LineAlignment = StringAlignment.Far;

                Overlays.CollectionChanged += Overlays_CollectionChanged;
            }
        }

#endif

        protected override void Dispose(bool disposing)
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
                CenterFormat.Dispose();
                BottomFormat.Dispose();
                CopyrightFont.Dispose();
                EmptyTileBorders.Dispose();
                EmptyTileBrush.Dispose();
                ClearBackBuffer();
            }
            base.Dispose(disposing);
        }

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
        public override void Refresh()
        {
            HoldInvalidation = false;

            lock (fCore.InvalidationLock) {
                fCore.LastInvalidation = DateTime.Now;
            }

            base.Refresh();
        }

#if !DESIGN
        /// <summary>
        /// enqueue built-in thread safe invalidation
        /// </summary>
        public new void Invalidate()
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
        public Image ToImage()
        {
            Image ret;

            bool r = fForceDoubleBuffer;
            try {
                UpdateBackBuffer();

                if (!r) {
                    fForceDoubleBuffer = true;
                }

                Refresh();
                Application.DoEvents();

                using (MemoryStream ms = new MemoryStream()) {
                    using (var frame = (fBackBuffer.Clone() as Bitmap)) {
                        frame.Save(ms, ImageFormat.Png);
                    }
                    ret = Image.FromStream(ms);
                }
            } catch (Exception) {
                throw;
            } finally {
                if (!r) {
                    fForceDoubleBuffer = false;
                    ClearBackBuffer();
                }
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
            if (IsHandleCreated) {
                fCore.DragOffset(new GPoint(x, y));
            }
        }

        #region UserControl Events

        protected override void OnLoad(EventArgs e)
        {
            base.OnLoad(e);

            if (!IsDesignerHosted) {
                fCore.ResetZoomToFitRect();
                fCore.OnMapOpen().ProgressChanged += invalidatorEngage;
                fCore.ForceUpdateOverlays();
            }
        }

        protected override void OnCreateControl()
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
        }

#if !DESIGN
#if DEBUG
        private int fCounter;
        private readonly Font fDebugFont = new Font(FontFamily.GenericSansSerif, 12, FontStyle.Regular);
        private DateTime fStart;
        private DateTime fEnd;
        private int fDelta;
#endif

        protected override void OnPaint(PaintEventArgs e)
        {
#if DEBUG
            fStart = DateTime.Now;
#endif

            if (fForceDoubleBuffer) {
                if (fGxOff != null) {
                    DrawGraphics(fGxOff);
                    e.Graphics.DrawImage(fBackBuffer, 0, 0);
                }
            } else {
                DrawGraphics(e.Graphics);
            }

            base.OnPaint(e);

#if DEBUG
            fEnd = DateTime.Now;
            fDelta = (int)(fEnd - fStart).TotalMilliseconds;
#endif
        }

        private void DrawGraphics(Graphics g)
        {
            // render white background
            g.Clear(EmptyMapBackground);
            g.TranslateTransform(fCore.RenderOffset.X, fCore.RenderOffset.Y);
            fCore.DrawMap(g);
            OnPaintOverlays(g);
        }
#endif

        /// <summary>
        /// override, to render something more
        /// </summary>
        /// <param name="g"></param>
        protected virtual void OnPaintOverlays(Graphics g)
        {
            g.SmoothingMode = SmoothingMode.HighQuality;
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

            g.ResetTransform();

            // copyright
            if (!string.IsNullOrEmpty(fCore.Provider.Copyright)) {
                g.DrawString(fCore.Provider.Copyright, CopyrightFont, Brushes.Navy, 3, Height - CopyrightFont.Height - 5);
            }

#if DEBUG_RENDER
            // show center
            g.DrawLine(CenterPen, Width / 2 - 5, Height / 2, Width / 2 + 5, Height / 2);
            g.DrawLine(CenterPen, Width / 2, Height / 2 - 5, Width / 2, Height / 2 + 5);

            // debug info
            g.DrawString(string.Format("{0:0.0}", Zoom) + "z, " + MapProvider + ", refresh: " + fCounter++ + ", render: " + fDelta + "ms", fDebugFont, Brushes.Blue, fDebugFont.Height, fDebugFont.Height + 20);
#endif
        }

        protected override void OnSizeChanged(EventArgs e)
        {
            base.OnSizeChanged(e);

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
                    UpdateBackBuffer();
                }

                fCore.OnMapSizeChanged(Width, Height);

                if (Visible && IsHandleCreated && fCore.IsStarted) {
                    fCore.ForceUpdateOverlays();
                }
            }
        }

        protected override void OnMouseDown(MouseEventArgs e)
        {
            base.OnMouseDown(e);

            if (!IsMouseOverMarker && e.Button == MouseButtons.Left) {
                fCore.MouseDown = new GPoint(e.X, e.Y);
                Invalidate();
            }
        }

        protected override void OnMouseUp(MouseEventArgs e)
        {
            base.OnMouseUp(e);

            if (fCore.IsDragging) {
                RestoreCursor();
                fCore.EndDrag();
            } else {
                if (e.Button == MouseButtons.Left) {
                    fCore.MouseDown = GPoint.Empty;
                }

                Invalidate();
            }
        }

        protected override void OnMouseClick(MouseEventArgs e)
        {
            base.OnMouseClick(e);

            fCore.ProcessMouseClick(e.X, e.Y, e);
        }

        protected override void OnMouseDoubleClick(MouseEventArgs e)
        {
            base.OnMouseDoubleClick(e);

            fCore.ProcessMouseDoubleClick(e.X, e.Y, e);
        }

        protected override void OnMouseMove(MouseEventArgs e)
        {
            base.OnMouseMove(e);

            if (!fCore.IsDragging && !fCore.MouseDown.IsEmpty) {
                // Gets the width and height of a rectangle centered on the point the mouse
                // button was pressed, within which a drag operation will not begin.
                var dragSize = SystemInformation.DragSize;

                if (Math.Abs(e.X - fCore.MouseDown.X) * 2 >= dragSize.Width || Math.Abs(e.Y - fCore.MouseDown.Y) * 2 >= dragSize.Height) {
                    fCore.BeginDrag(fCore.MouseDown);
                }
            }

            if (fCore.IsDragging) {
                SetCursorDrag();
                fCore.MouseCurrent = new GPoint(e.X, e.Y);
                fCore.Drag(fCore.MouseCurrent);
                base.Invalidate();
            } else {
                if (fCore.MouseDown.IsEmpty) {
                    fCore.ProcessOverlaysMouseMove(e.X, e.Y);
                }
            }
        }

        protected override void OnMouseEnter(EventArgs e)
        {
            base.OnMouseEnter(e);

            Focus();
            fCore.MouseIn = true;
        }

        protected override void OnMouseLeave(EventArgs e)
        {
            base.OnMouseLeave(e);

            fCore.MouseIn = false;
        }

        protected override void OnMouseWheel(MouseEventArgs e)
        {
            base.OnMouseWheel(e);

            fCore.ProcessMouseWheel(e.X, e.Y, e.Delta);
        }

        #endregion

        #region Core callbacks and aux private methods

        private void ParentForm_FormClosing(object sender, FormClosingEventArgs e)
        {
            if (e.CloseReason == CloseReason.WindowsShutDown || e.CloseReason == CloseReason.TaskManagerClosing) {
                GMaps.Instance.CancelTileCaching();
            }
        }

        private void UpdateBackBuffer()
        {
            ClearBackBuffer();

            fBackBuffer = new Bitmap(Width, Height);
            fGxOff = Graphics.FromImage(fBackBuffer);
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
        }

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
            base.Invalidate();
        }

        void IMapControl.DrawTile(object go, PureImage pureImage, ref bool found)
        {
            Graphics g = (Graphics)go;
            var img = (GMapImage)pureImage;
            if (img != null && img.Img != null) {
                if (!found)
                    found = true;

                if (!img.IsParent) {
                    g.DrawImage(img.Img, fCore.TileRect.X, fCore.TileRect.Y, fCore.TileRect.Width, fCore.TileRect.Height);
                } else {
                    float wix = ((float)img.Img.Width / img.Ix);
                    float hix = ((float)img.Img.Height / img.Ix);
                    RectangleF srcRect = new RectangleF(img.Xoff * wix, img.Yoff * hix, wix, hix);
                    Rectangle dst = new Rectangle((int)fCore.TileRect.X, (int)fCore.TileRect.Y, (int)fCore.TileRect.Width, (int)fCore.TileRect.Height);
                    g.DrawImage(img.Img, dst, srcRect.X, srcRect.Y, srcRect.Width, srcRect.Height, GraphicsUnit.Pixel, TileFlipXYAttributes);
                }
            }
        }

        void IMapControl.DrawLowerTile(object go, PureImage pureImage, long Ix, long xoff, long yoff, ref bool found)
        {
            Graphics g = (Graphics)go;
            var img = (GMapImage)pureImage;
            if (img != null && img.Img != null && !img.IsParent) {
                if (!found)
                    found = true;

                RectangleF srcRect = new RectangleF(xoff * (img.Img.Width / Ix),
                    yoff * (img.Img.Height / Ix), (img.Img.Width / Ix), (img.Img.Height / Ix));
                Rectangle dst = new Rectangle((int)fCore.TileRect.X, (int)fCore.TileRect.Y,
                    (int)fCore.TileRect.Width, (int)fCore.TileRect.Height);

                g.DrawImage(img.Img, dst, srcRect.X, srcRect.Y, srcRect.Width, srcRect.Height,
                    GraphicsUnit.Pixel, TileFlipXYAttributes);
            }
        }

        void IMapControl.DrawMissingTile(object go, Exception ex)
        {
            Graphics g = (Graphics)go;

            g.FillRectangle(EmptyTileBrush,
                new RectangleF(fCore.TileRect.X, fCore.TileRect.Y, fCore.TileRect.Width,
                    fCore.TileRect.Height));

            g.DrawString("Exception: " + ex.Message, MissingDataFont, Brushes.Red,
                new RectangleF(fCore.TileRect.X + 11, fCore.TileRect.Y + 11,
                    fCore.TileRect.Width - 11, fCore.TileRect.Height - 11));

            g.DrawString(EmptyTileText, MissingDataFont, Brushes.Blue,
                new RectangleF(fCore.TileRect.X, fCore.TileRect.Y, fCore.TileRect.Width,
                    fCore.TileRect.Height), CenterFormat);

            g.DrawRectangle(EmptyTileBorders, (int)fCore.TileRect.X, (int)fCore.TileRect.Y,
                (int)fCore.TileRect.Width, (int)fCore.TileRect.Height);
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
            if (obj is MapMarker) {
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
            }
        }

        void IMapControl.DoMouseDoubleClick(MapObject obj, EventArgs e)
        {
            if (obj is MapMarker) {
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
            }
        }

        private void SetCursorHand()
        {
            fCursorBefore = Cursor;
            Cursor = Cursors.Hand;
        }

        private void SetCursorDrag()
        {
            fCursorBefore = Cursor;
            Cursor = Cursors.SizeAll;
        }

        private void RestoreCursor()
        {
            Cursor = Cursors.Arrow;
            fCursorBefore = null;
        }

        void IMapControl.RestoreCursorOnLeave()
        {
            if (fOverObjectCount <= 0 && fCursorBefore != null) {
                fOverObjectCount = 0;
                RestoreCursor();
            }
        }

        void IMapControl.SetCursorHandOnEnter()
        {
            if (fOverObjectCount <= 0 && Cursor != Cursors.Hand) {
                fOverObjectCount = 0;
                SetCursorHand();
            }
        }

        void IMapControl.SetMousePositionToMapCenter()
        {
            if (!fCore.IsRunningOnMono) {
                Point p = PointToScreen(new Point(Width / 2, Height / 2));
                SetCursorPos(p.X, p.Y);
            }
        }

        [DllImport("user32.dll", EntryPoint = "SetCursorPos")]
        [return: MarshalAs(UnmanagedType.Bool)]
        private static extern bool SetCursorPos(int x, int y);

        #endregion
    }
}
