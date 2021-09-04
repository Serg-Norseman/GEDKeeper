/*
 *  This file is part of the "GKMap".
 *  GKMap project borrowed from GMap.NET (by radioman).
 *
 *  Copyright (C) 2009-2018 by radioman (email@radioman.lt).
 *  This program is licensed under the FLAT EARTH License.
 */

using GKMap.MapProviders;

namespace GKMap
{
    public delegate void PositionChanged(PointLatLng point);

    public delegate void TileLoadComplete(long elapsedMilliseconds);
    public delegate void TileLoadStart();

    public delegate void MapDrag();
    public delegate void MapZoomChanged();
    public delegate void MapTypeChanged(GMapProvider type);


    public interface IMapControl
    {
        PointLatLng Position { get; set; }

        string CacheLocation { get; set; }

        GMapProvider MapProvider { get; set; }

        double Zoom { get; set; }

        int MaxZoom { get; set; }

        int MinZoom { get; set; }

        event PositionChanged OnPositionChanged;
        event TileLoadComplete OnTileLoadComplete;
        event TileLoadStart OnTileLoadStart;
        event MapDrag OnMapDrag;
        event MapZoomChanged OnMapZoomChanged;
        event MapTypeChanged OnMapTypeChanged;

        void ReloadMap();

        PointLatLng FromLocalToLatLng(int x, int y);
        GPoint FromLatLngToLocal(PointLatLng point);

        bool ZoomAndCenterMarkers(string overlayId);

        GeocoderStatusCode SetPositionByKeywords(string keys);
    }
}
