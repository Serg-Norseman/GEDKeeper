/*
 *  This file is part of the "GKMap".
 *  GKMap project borrowed from GMap.NET (by radioman).
 *
 *  Copyright (C) 2009-2018 by radioman (email@radioman.lt).
 *  This program is licensed under the FLAT EARTH License.
 */

using System;
using GKMap.MapProviders;

namespace GKMap
{
    public delegate void PositionChanged(PointLatLng point);

    public delegate void TileLoadComplete(long elapsedMilliseconds);
    public delegate void TileLoadStart();

    public delegate void MapDrag();
    public delegate void MapZoomChanged();
    public delegate void MapTypeChanged(GMapProvider type);

    public delegate void MarkerEnter(IMapMarker item);
    public delegate void MarkerLeave(IMapMarker item);

    public delegate void PolygonEnter(IMapPolygon item);
    public delegate void PolygonLeave(IMapPolygon item);

    public delegate void RouteEnter(IMapRoute item);
    public delegate void RouteLeave(IMapRoute item);

    public interface IMapControl
    {
        PointLatLng Position { get; set; }

        string CacheLocation { get; set; }

        GMapProvider MapProvider { get; set; }

        int Zoom { get; set; }

        int MaxZoom { get; set; }

        int MinZoom { get; set; }

        ObservableCollection<IMapOverlay> Overlays { get; }

        event PositionChanged OnPositionChanged;
        event TileLoadComplete OnTileLoadComplete;
        event TileLoadStart OnTileLoadStart;
        event MapDrag OnMapDrag;
        event MapZoomChanged OnMapZoomChanged;
        event MapTypeChanged OnMapTypeChanged;

        PointLatLng FromLocalToLatLng(int x, int y);
        GPoint FromLatLngToLocal(PointLatLng point);

        void Invalidate();

        void Refresh();

        void ReloadMap();

        GeocoderStatusCode SetPositionByKeywords(string keys);

        bool ZoomAndCenterMarkers(string overlayId);

        bool HoldInvalidation { get; set; }
        bool IsMouseOverMarker { get; set; }
        bool IsMouseOverRoute { get; set; }
        bool IsMouseOverPolygon { get; set; }

        void DoMouseClick(IMapObject obj, EventArgs e);
        void DoMouseDoubleClick(IMapObject obj, EventArgs e);
        //void ForceUpdateOverlays();
        void RestoreCursorOnLeave();
        void SetCursorHandOnEnter();
        void SetMousePositionToMapCenter();
        void UpdateMarkerLocalPosition(IMapMarker marker);
        void UpdateRouteLocalPosition(IMapRoute route);
        void UpdatePolygonLocalPosition(IMapPolygon polygon);
    }
}
