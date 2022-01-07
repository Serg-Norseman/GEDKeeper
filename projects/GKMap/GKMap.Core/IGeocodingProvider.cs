/*
 *  This file is part of the "GKMap".
 *  GKMap project borrowed from GMap.NET (by radioman).
 *
 *  Copyright (C) 2009-2018 by radioman (email@radioman.lt).
 *  This program is licensed under the FLAT EARTH License.
 */

using System.Collections.Generic;

namespace GKMap
{
    /// <summary>
    /// Geocoding interface
    /// </summary>
    public interface IGeocodingProvider
    {
        GeocoderStatusCode GetPoints(string keywords, out List<PointLatLng> pointList);

        PointLatLng? GetPoint(string keywords, out GeocoderStatusCode status);

        GeocoderStatusCode GetPoints(Placemark placemark, out List<PointLatLng> pointList);

        PointLatLng? GetPoint(Placemark placemark, out GeocoderStatusCode status);

        GeocoderStatusCode GetPlacemarks(PointLatLng location, out List<Placemark> placemarkList);

        Placemark? GetPlacemark(PointLatLng location, out GeocoderStatusCode status);
    }
}
