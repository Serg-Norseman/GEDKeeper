/*
 *  This file is part of the "GKMap".
 *  GKMap project borrowed from GMap.NET (by radioman).
 *
 *  Copyright (C) 2009-2018 by radioman (email@radioman.lt).
 *  This program is licensed under the FLAT EARTH License.
 */

using System;

namespace GKMap.MapObjects
{
    /// <summary>
    /// GKMap overlay.
    /// </summary>
    public abstract class MapOverlay : IDisposable
    {
        private IMapControlEx fControl;
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

                            Control.Invalidate();
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
        public ObservableCollectionThreadSafe<MapMarker> Markers { get; private set; }

        /// <summary>
        /// list of routes, should be thread safe
        /// </summary>
        public ObservableCollectionThreadSafe<MapRoute> Routes { get; private set; }

        /// <summary>
        /// list of polygons, should be thread safe
        /// </summary>
        public ObservableCollectionThreadSafe<MapPolygon> Polygons { get; private set; }

        internal IMapControlEx Control
        {
            get {
                return fControl;
            }
            set {
                fControl = value;
            }
        }

        protected MapOverlay()
        {
            Markers = new ObservableCollectionThreadSafe<MapMarker>();
            Routes = new ObservableCollectionThreadSafe<MapRoute>();
            Polygons = new ObservableCollectionThreadSafe<MapPolygon>();
            CreateEvents();
        }

        protected MapOverlay(string id) : this()
        {
            Id = id;
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
                foreach (MapPolygon obj in e.NewItems) {
                    if (obj != null) {
                        obj.Overlay = this;
                        if (Control != null) {
                            Control.Core.UpdatePolygonLocalPosition(obj);
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

                Control.Invalidate();
            }
        }

        private void Routes_CollectionChanged(object sender, NotifyCollectionChangedEventArgs e)
        {
            if (e.NewItems != null) {
                foreach (MapRoute obj in e.NewItems) {
                    if (obj != null) {
                        obj.Overlay = this;
                        if (Control != null) {
                            Control.Core.UpdateRouteLocalPosition(obj);
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

                Control.Invalidate();
            }
        }

        private void Markers_CollectionChanged(object sender, NotifyCollectionChangedEventArgs e)
        {
            if (e.NewItems != null) {
                foreach (MapMarker obj in e.NewItems) {
                    if (obj != null) {
                        obj.Overlay = this;
                        if (Control != null) {
                            Control.Core.UpdateMarkerLocalPosition(obj);
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

                Control.Invalidate();
            }
        }

        /// <summary>
        /// updates local positions of objects
        /// </summary>
        public void ForceUpdate()
        {
            if (Control == null) return;

            foreach (var obj in Markers) {
                if (obj.IsVisible) {
                    Control.Core.UpdateMarkerLocalPosition(obj);
                }
            }

            foreach (var obj in Polygons) {
                if (obj.IsVisible) {
                    Control.Core.UpdatePolygonLocalPosition(obj);
                }
            }

            foreach (var obj in Routes) {
                if (obj.IsVisible) {
                    Control.Core.UpdateRouteLocalPosition(obj);
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
