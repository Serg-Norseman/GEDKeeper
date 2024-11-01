/*
 *  This file is part of the "GKMap".
 *  GKMap project borrowed from GMap.NET (by radioman).
 *
 *  Copyright (C) 2009-2018 by radioman (email@radioman.lt).
 *  This program is licensed under the FLAT EARTH License.
 */

//#define DEBUG_TILE_COORDS
//#define DEBUG_CENTER

using System;
using System.ComponentModel;
using System.Diagnostics;
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
    public class GMapControl : SKCanvasView, IMapControlEx
    {
        private MapCore fCore;
        private bool fIsMouseOverMarker;
        private bool fIsMouseOverPolygon;
        private bool fIsMouseOverRoute;
        private int fOverObjectCount;

        private readonly SKColor EmptyMapBackground = SKColors.WhiteSmoke;
        private readonly SKPaint EmptyTileBorders = new SKPaint() { Color = SKColors.White, StrokeWidth = 1, Style = SKPaintStyle.Stroke };
        private readonly SKPaint EmptyTileBrush = new SKPaint() { Color = SKColors.Navy, Style = SKPaintStyle.Fill };
        private readonly string EmptyTileText = "We are sorry, but we don't\nhave imagery at this zoom\nlevel for this region.";
        private readonly SKPaint MissingDataFont = new SKPaint(new SKFont(SKTypeface.FromFamilyName("Sans"), 11) { Embolden = true }) { Color = SKColors.Red };
        //private readonly ImageAttributes TileFlipXYAttributes = new ImageAttributes();

#if DEBUG_CENTER
        private readonly SKPaint CenterPen = new SKPaint() { Color = SKColors.Red, StrokeWidth = 1, Style = SKPaintStyle.Stroke };
        private readonly SKPaint ScalePen = new SKPaint() { Color = SKColors.Blue, StrokeWidth = 1, Style = SKPaintStyle.Stroke };
#endif

        #region Properties

        /// <summary>
        /// location of cache
        /// </summary>
        public string CacheLocation
        {
            get {
                return GMaps.CacheLocation;
            }
            set {
                GMaps.CacheLocation = value;
            }
        }

        MapCore IMapControlEx.Core { get { return fCore; } }

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
        /// maximum zoom level of map
        /// </summary>
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
        /// minimum zoom level of map
        /// </summary>
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

        public int Zoom
        {
            get {
                return fCore.Zoom;
            }
            set {
                if (fCore.Zoom != value) {
                    fCore.Zoom = value;

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
            GMaps.Initialize(GMapImageProxy.Instance);
        }

        public GMapControl()
        {
            EnableTouchEvents = true;
            SizeChanged += OnSizeChanged;

            fCore = new MapCore(this);

            Overlays = new ObservableCollectionThreadSafe<MapOverlay>();

            InvertedMouseWheelZooming = false;
            MaxZoom = 17;
            MinZoom = 2;
            Zoom = 0;

            //TileFlipXYAttributes.SetWrapMode(WrapMode.TileFlipXY);
            Overlays.CollectionChanged += Overlays_CollectionChanged;

            OnLoad(null);
        }

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

                //CenterFormat.Dispose();
                //BottomFormat.Dispose();
                CopyrightFont.Dispose();
                EmptyTileBorders.Dispose();
                EmptyTileBrush.Dispose();
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
        }

        /// <summary>
        /// enqueue built-in thread safe invalidation
        /// </summary>
        public void Invalidate()
        {
            if (fCore.RefreshEvent != null && !HoldInvalidation) {
                fCore.RefreshEvent.Set();
            }
        }

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
            try {
                Refresh();
                /*using (MemoryStream ms = new MemoryStream()) {
                    using (var frame = (fBackBuffer.Clone() as SKBitmap)) {
                        frame.Save(ms, ImageFormat.Png);
                    }
                    ret = new SKBitmap(ms);
                }*/
            } catch (Exception) {
                throw;
            } finally {
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
            fCore.DragOffset(new GPoint(x, y));
        }

        #region UserControl Events

        protected /*override*/ void OnLoad(EventArgs e)
        {
            //base.OnLoad(e);

            fCore.ResetZoomToFitRect();
            fCore.OnMapOpen().ProgressChanged += invalidatorEngage;
            fCore.ForceUpdateOverlays();
        }

        /*protected override void OnCreateControl()
        {
            base.OnCreateControl();

                var f = ParentForm;
                if (f != null) {
                    while (f.ParentForm != null) {
                        f = f.ParentForm;
                    }

                    f.FormClosing += ParentForm_FormClosing;
                }
        }*/

        protected override void OnPaintSurface(SKPaintSurfaceEventArgs e)
        {
            SKCanvas g = e.Surface.Canvas;

            // render white background
            g.Clear(EmptyMapBackground);
            g.Save();
            g.Translate(fCore.RenderOffset.X, fCore.RenderOffset.Y);

            fCore.DrawMap(g);

            foreach (GMapOverlay o in Overlays) {
                if (o.IsVisible) {
                    o.OnRender(g);
                }
            }

#if DEBUG_CENTER
            // center in virtual space...
            g.DrawLine(-20, 0, 20, 0, ScalePen);
            g.DrawLine(0, -20, 0, 20, ScalePen);
#endif

            g.Restore();

#if DEBUG_CENTER
            // show center
            float hw = (float)Width / 2;
            float hh = (float)Height / 2;
            g.DrawLine(hw - 5, hh, hw + 5, hh, CenterPen);
            g.DrawLine(hw, hh - 5, hw, hh + 5, CenterPen);
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

            fCore.OnMapSizeChanged((int)Width, (int)Height);
            if (IsVisible && fCore.IsStarted) {
                fCore.ForceUpdateOverlays();
            }
        }

        protected override void OnTouch(SKTouchEventArgs e)
        {
            base.OnTouch(e);

            //Point mpt = new Point(e.Location.X, e.Location.Y);
            //SKPoint mpt = e.Location.ToPoint();
            var gpt = new GPoint((long)e.Location.X, (long)e.Location.Y);

            switch (e.ActionType) {
                case SKTouchAction.Pressed: {
                        if (!IsMouseOverMarker && e.MouseButton == SKMouseButton.Left) {
                            fCore.MouseDown = gpt;
                            Invalidate();
                        }
                    }
                    break;

                case SKTouchAction.Released:
                case SKTouchAction.Cancelled: {
                        if (fCore.IsDragging) {
                            fCore.EndDrag();
                        } else {
                            if (e.MouseButton == SKMouseButton.Left) {
                                fCore.MouseDown = GPoint.Empty;
                            }
                            Invalidate();
                        }
                    }
                    break;

                case SKTouchAction.Moved: {
                        if (!fCore.IsDragging && !fCore.MouseDown.IsEmpty) {
                            // Gets the width and height of a rectangle centered on the point the mouse
                            // button was pressed, within which a drag operation will not begin.

                            // FIXME
                            var dragSize = new SKSize(8, 8); //SystemInformation.DragSize;

                            if (Math.Abs(gpt.X - fCore.MouseDown.X) * 2 >= dragSize.Width || Math.Abs(gpt.Y - fCore.MouseDown.Y) * 2 >= dragSize.Height) {
                                fCore.BeginDrag(fCore.MouseDown);
                            }
                        }

                        if (fCore.IsDragging) {
                            fCore.Drag(gpt.X, gpt.Y);
                            base.InvalidateSurface();
                        } else {
                            if (fCore.MouseDown.IsEmpty) {
                                fCore.ProcessOverlaysMouseMove((int)gpt.X, (int)gpt.Y);
                            }
                        }
                    }
                    break;

                case SKTouchAction.WheelChanged: {
                        fCore.ProcessMouseWheel((int)gpt.X, (int)gpt.Y, (int)e.WheelDelta);
                    }
                    break;

                case SKTouchAction.Entered: {
                        Focus();
                        fCore.MouseIn = true;
                    }
                    break;

                case SKTouchAction.Exited: {
                        fCore.MouseIn = false;
                    }
                    break;
            }

            e.Handled = true;
        }

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

        #endregion

        #region Core callbacks and aux private methods

        // TODO
        /*private void ParentForm_FormClosing(object sender, FormClosingEventArgs e)
        {
            if (e.CloseReason == CloseReason.WindowsShutDown || e.CloseReason == CloseReason.TaskManagerClosing) {
                GMaps.Instance.CancelTileCaching();
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
                    g.DrawImage(img.Img, dst);
                } else {
                    float wix = ((float)img.Img.Width / img.Ix);
                    float hix = ((float)img.Img.Height / img.Ix);
                    SKRect srcRect = SKRect.Create(img.Xoff * wix, img.Yoff * hix, wix, hix);
                    SKRect dst = SKRect.Create(fCore.TileRect.X, fCore.TileRect.Y, fCore.TileRect.Width, fCore.TileRect.Height);
                    g.DrawImage(img.Img, srcRect, dst);
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

                g.DrawImage(img.Img, srcRect, dst);
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
            SKCanvas g = (SKCanvas)go;
            // show tile grid lines
            g.DrawRect((int)fCore.TileRect.X, (int)fCore.TileRect.Y, (int)fCore.TileRect.Width, (int)fCore.TileRect.Height, EmptyTileBorders);
            g.DrawText((tilePoint.PosXY == fCore.CenterTileXYLocation ? "CENTER: " : "TILE: ") + tilePoint, fCore.TileRect.X, fCore.TileRect.Y, MissingDataFont);
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

        void IMapControl.RestoreCursorOnLeave()
        {
            if (fOverObjectCount <= 0) {
                fOverObjectCount = 0;
            }
        }

        void IMapControl.SetCursorHandOnEnter()
        {
            if (fOverObjectCount <= 0) {
                fOverObjectCount = 0;
            }
        }

        void IMapControl.SetMousePositionToMapCenter()
        {
            // not used
        }

        #endregion
    }
}
