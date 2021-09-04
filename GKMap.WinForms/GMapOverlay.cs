/*
 *  This file is part of the "GKMap".
 *  GKMap project borrowed from GMap.NET (by radioman).
 *
 *  Copyright (C) 2009-2018 by radioman (email@radioman.lt).
 *  This program is licensed under the FLAT EARTH License.
 */

using System;
using System.Drawing;

namespace GKMap.WinForms
{
    /// <summary>
    /// GKMap overlay
    /// </summary>
    public class GMapOverlay : IDisposable
    {
        private GMapControl fControl;
        private bool fDisposed;
        private bool fIsHitTestVisible = true;
        private bool fIsVisible = true;
        private bool fIsZoomSignificant = true;

        /// <summary>
        /// is overlay visible
        /// </summary>
        public bool IsVisible
        {
            get {
                return fIsVisible;
            }
            set {
                if (value != fIsVisible) {
                    fIsVisible = value;

                    if (Control != null) {
                        if (fIsVisible) {
                            Control.HoldInvalidation = true;
                            ForceUpdate();
                            Control.Refresh();
                        } else {
                            if (Control.IsMouseOverMarker) {
                                Control.IsMouseOverMarker = false;
                            }

                            if (Control.IsMouseOverPolygon) {
                                Control.IsMouseOverPolygon = false;
                            }

                            if (Control.IsMouseOverRoute) {
                                Control.IsMouseOverRoute = false;
                            }
                            Control.RestoreCursorOnLeave();

                            if (!Control.HoldInvalidation) {
                                Control.Invalidate();
                            }
                        }
                    }
                }
            }
        }

        /// <summary>
        /// HitTest visibility for entire overlay
        /// </summary>
        public bool IsHitTestVisible
        {
            get { return fIsHitTestVisible; }
            set { fIsHitTestVisible = value; }
        }

        /// <summary>
        /// if false don't consider contained objects when box zooming
        /// </summary>
        public bool IsZoomSignificant
        {
            get { return fIsZoomSignificant; }
            set { fIsZoomSignificant = value; }
        }

        /// <summary>
        /// overlay Id
        /// </summary>
        public string Id { get; set; }

        /// <summary>
        /// list of markers, should be thread safe
        /// </summary>
        public readonly ObservableCollectionThreadSafe<GMapMarker> Markers = new ObservableCollectionThreadSafe<GMapMarker>();

        /// <summary>
        /// list of routes, should be thread safe
        /// </summary>
        public readonly ObservableCollectionThreadSafe<GMapRoute> Routes = new ObservableCollectionThreadSafe<GMapRoute>();

        /// <summary>
        /// list of polygons, should be thread safe
        /// </summary>
        public readonly ObservableCollectionThreadSafe<GMapPolygon> Polygons = new ObservableCollectionThreadSafe<GMapPolygon>();

        public GMapControl Control
        {
            get {
                return fControl;
            }
            internal set {
                fControl = value;
            }
        }

        public GMapOverlay()
        {
            CreateEvents();
        }

        public GMapOverlay(string id)
        {
            Id = id;
            CreateEvents();
        }

        private void CreateEvents()
        {
            Markers.CollectionChanged += Markers_CollectionChanged;
            Routes.CollectionChanged += Routes_CollectionChanged;
            Polygons.CollectionChanged += Polygons_CollectionChanged;
        }

        private void ClearEvents()
        {
            Markers.CollectionChanged -= Markers_CollectionChanged;
            Routes.CollectionChanged -= Routes_CollectionChanged;
            Polygons.CollectionChanged -= Polygons_CollectionChanged;
        }

        public void Clear()
        {
            Markers.Clear();
            Routes.Clear();
            Polygons.Clear();
        }

        private void Polygons_CollectionChanged(object sender, NotifyCollectionChangedEventArgs e)
        {
            if (e.NewItems != null) {
                foreach (GMapPolygon obj in e.NewItems) {
                    if (obj != null) {
                        obj.Overlay = this;
                        if (Control != null) {
                            Control.UpdatePolygonLocalPosition(obj);
                        }
                    }
                }
            }

            if (Control != null) {
                if (e.Action == NotifyCollectionChangedAction.Remove || e.Action == NotifyCollectionChangedAction.Reset) {
                    if (Control.IsMouseOverPolygon) {
                        Control.IsMouseOverPolygon = false;
                        Control.RestoreCursorOnLeave();
                    }
                }

                if (!Control.HoldInvalidation) {
                    Control.Invalidate();
                }
            }
        }

        private void Routes_CollectionChanged(object sender, NotifyCollectionChangedEventArgs e)
        {
            if (e.NewItems != null) {
                foreach (GMapRoute obj in e.NewItems) {
                    if (obj != null) {
                        obj.Overlay = this;
                        if (Control != null) {
                            Control.UpdateRouteLocalPosition(obj);
                        }
                    }
                }
            }

            if (Control != null) {
                if (e.Action == NotifyCollectionChangedAction.Remove || e.Action == NotifyCollectionChangedAction.Reset) {
                    if (Control.IsMouseOverRoute) {
                        Control.IsMouseOverRoute = false;
                        Control.RestoreCursorOnLeave();
                    }
                }

                if (!Control.HoldInvalidation) {
                    Control.Invalidate();
                }
            }
        }

        private void Markers_CollectionChanged(object sender, NotifyCollectionChangedEventArgs e)
        {
            if (e.NewItems != null) {
                foreach (GMapMarker obj in e.NewItems) {
                    if (obj != null) {
                        obj.Overlay = this;
                        if (Control != null) {
                            Control.UpdateMarkerLocalPosition(obj);
                        }
                    }
                }
            }

            if (Control != null) {
                if (e.Action == NotifyCollectionChangedAction.Remove || e.Action == NotifyCollectionChangedAction.Reset) {
                    if (Control.IsMouseOverMarker) {
                        Control.IsMouseOverMarker = false;
                        Control.RestoreCursorOnLeave();
                    }
                }

                if (!Control.HoldInvalidation) {
                    Control.Invalidate();
                }
            }
        }

        /// <summary>
        /// updates local positions of objects
        /// </summary>
        internal void ForceUpdate()
        {
            if (Control != null) {
                foreach (GMapMarker obj in Markers) {
                    if (obj.IsVisible) {
                        Control.UpdateMarkerLocalPosition(obj);
                    }
                }

                foreach (GMapPolygon obj in Polygons) {
                    if (obj.IsVisible) {
                        Control.UpdatePolygonLocalPosition(obj);
                    }
                }

                foreach (GMapRoute obj in Routes) {
                    if (obj.IsVisible) {
                        Control.UpdateRouteLocalPosition(obj);
                    }
                }
            }
        }

        /// <summary>
        /// renders objects/routes/polygons
        /// </summary>
        /// <param name="g"></param>
        public virtual void OnRender(Graphics g)
        {
            if (Control != null) {
                foreach (GMapRoute r in Routes) {
                    if (r.IsVisible) {
                        r.OnRender(g);
                    }
                }

                foreach (GMapPolygon r in Polygons) {
                    if (r.IsVisible) {
                        r.OnRender(g);
                    }
                }

                foreach (GMapMarker m in Markers) {
                    if (m.IsVisible) {
                        m.OnRender(g);
                    }
                }

                // tooltips above
                foreach (GMapMarker m in Markers) {
                    if (m.ToolTip != null && m.IsVisible) {
                        if (!string.IsNullOrEmpty(m.ToolTipText) && (m.ToolTipMode == MarkerTooltipMode.Always || m.IsMouseOver)) {
                            m.ToolTip.OnRender(g);
                        }
                    }
                }
            }
        }

        public void Dispose()
        {
            if (!fDisposed) {
                fDisposed = true;

                ClearEvents();

                foreach (var m in Markers) {
                    m.Dispose();
                }

                foreach (var r in Routes) {
                    r.Dispose();
                }

                foreach (var p in Polygons) {
                    p.Dispose();
                }

                Clear();
            }
        }
    }
}
