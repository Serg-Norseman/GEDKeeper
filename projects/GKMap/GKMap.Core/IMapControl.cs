/*
 *  This file is part of the "GKMap".
 *  GKMap project borrowed from GMap.NET (by radioman).
 *
 *  Copyright (C) 2009-2018 by radioman (email@radioman.lt).
 *  This program is licensed under the FLAT EARTH License.
 */

using System;
using GKMap.MapObjects;
using GKMap.MapProviders;

namespace GKMap
{
    public delegate void PositionChanged(PointLatLng point);
    public delegate void MapZoomChanged();
    public delegate void MapTypeChanged(GMapProvider type);

    public delegate void MarkerEnter(MapMarker item);
    public delegate void MarkerLeave(MapMarker item);

    public delegate void PolygonEnter(MapPolygon item);
    public delegate void PolygonLeave(MapPolygon item);

    public delegate void RouteEnter(MapRoute item);
    public delegate void RouteLeave(MapRoute item);

    public interface IMapControl
    {
        string CacheLocation { get; set; }

        GMapProvider MapProvider { get; set; }

        int MaxZoom { get; set; }

        int MinZoom { get; set; }

        PointLatLng Position { get; set; }

        int Zoom { get; set; }

        ObservableCollectionThreadSafe<MapOverlay> Overlays { get; }

        event PositionChanged OnPositionChanged;
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

        #region Callbacks

        void DoMouseClick(MapObject obj, EventArgs e);
        void DoMouseDoubleClick(MapObject obj, EventArgs e);
        void RestoreCursorOnLeave();
        void SetCursorHandOnEnter();
        void DrawTile(object go, PureImage pureImage, ref bool found);
        void DrawLowerTile(object go, PureImage pureImage, long Ix, long xoff, long yoff, ref bool found);
        void DrawMissingTile(object go, Exception ex);
        void ShowTileGridLines(object go, DrawTile tilePoint);
        void SetMousePositionToMapCenter();

        #endregion
    }


    internal interface IMapControlEx : IMapControl
    {
        MapCore Core { get; }
    }
}
