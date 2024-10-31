/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
 *
 *  This file is part of "GEDKeeper".
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

using System;
using System.Collections.Generic;
using GDModel.Providers.GEDCOM;
using GKCore.Maps;
using GKCore.Design.Controls;
using NSubstitute;
using NUnit.Framework;

namespace GKCore
{
    [TestFixture]
    public class PlacesLoaderTests
    {
        public PlacesLoaderTests()
        {
        }

        [Test]
        public void Test_PlaceRef()
        {
            var placeRef = new PlaceRef(null, null);
            Assert.IsNotNull(placeRef);
            Assert.IsNull(placeRef.Event);
        }

        [Test]
        public void Test_MapPlace()
        {
            var mapPlace = new MapPlace(string.Empty);
            Assert.IsNotNull(mapPlace);
            Assert.IsNotNull(mapPlace.Points);
            Assert.IsNotNull(mapPlace.PlaceRefs);
        }

        [Test]
        public void Test_GeoPoint()
        {
            GeoPoint mapPoint = new GeoPoint(0.5f, 0.5f, "test");
            Assert.IsNotNull(mapPoint);
            Assert.AreEqual(0.5f, mapPoint.Latitude);
            Assert.AreEqual(0.5f, mapPoint.Longitude);
            Assert.AreEqual("test", mapPoint.Hint);
        }

        [Test]
        public void Test_Geocoding()
        {
            IGeocoder geocoder = IGeocoder.Create("", "");

            geocoder.SetKey("");
            geocoder.SetProxy(null);
            geocoder.SetLang("");
        }

        [Test]
        public void Test_Geocoding_Google()
        {
            var geocoder = IGeocoder.Create("Google", "us");
            geocoder.SetKey(GKData.GAPI_KEY);
            var geoPoints = geocoder.Geocode("New York", 1);
            Assert.IsNotNull(geoPoints);
            //Assert.IsTrue(geoPoints.Count > 0);
        }

        [Test]
        public void Test_Geocoding_Yandex()
        {
            var geocoder = IGeocoder.Create("Yandex", "us");
            var geoPoints = geocoder.Geocode("New York", 1);
            Assert.IsNotNull(geoPoints);
            //Assert.IsTrue(geoPoints.Count > 0);
        }

        [Test]
        public void Test_Geocoding_OSM()
        {
            var geocoder = IGeocoder.Create("OSM", "us");
            var geoPoints = geocoder.Geocode("New York", 1);
            Assert.IsNotNull(geoPoints);
            //Assert.IsTrue(geoPoints.Count > 0);
        }

        [Test]
        public void Test_AddPoint()
        {
            var gmapPoints = new List<GeoPoint>();

            PlacesLoader.AddPoint(gmapPoints, new GeoPoint(0, 0, "test"), new PlaceRef(null, null));
            Assert.AreEqual(1, gmapPoints.Count);

            PlacesLoader.AddPoint(gmapPoints, new GeoPoint(0, 0, "test"), new PlaceRef(null, null));
            Assert.AreEqual(1, gmapPoints.Count); // duplicate will be excluded
        }

        [Test]
        public void Test_CopyPoints()
        {
            var mapBrowser = Substitute.For<IMapBrowser>();

            Assert.Throws(typeof(ArgumentNullException), () => { PlacesLoader.CopyPoints(mapBrowser, null, true); });

            var gmapPoints = new List<GeoPoint>();
            gmapPoints.Add(new GeoPoint(0, 0, "test"));
            PlacesLoader.CopyPoints(mapBrowser, gmapPoints, true);
        }

        [Test]
        public void Test_GetPointsFrame()
        {
            var coordsRect = PlacesLoader.GetPointsFrame(null);
            Assert.AreEqual(0.0d, coordsRect.MinLon);
            Assert.AreEqual(0.0d, coordsRect.MinLat);
            Assert.AreEqual(0.0d, coordsRect.MaxLon);
            Assert.AreEqual(0.0d, coordsRect.MaxLat);

            var mapPoints = new List<GeoPoint>();
            mapPoints.Add(new GeoPoint(11, 13, "pt1"));
            mapPoints.Add(new GeoPoint(22, 25, "pt1"));
            coordsRect = PlacesLoader.GetPointsFrame(mapPoints);
            Assert.AreEqual(13.0d, coordsRect.MinLon);
            Assert.AreEqual(11.0d, coordsRect.MinLat);
            Assert.AreEqual(25.0d, coordsRect.MaxLon);
            Assert.AreEqual(22.0d, coordsRect.MaxLat);

            mapPoints.Clear();
            mapPoints.Add(new GeoPoint(21, 21, "pt1"));
            coordsRect = PlacesLoader.GetPointsFrame(mapPoints);
            Assert.AreEqual(1.0d, coordsRect.MinLon);
            Assert.AreEqual(1.0d, coordsRect.MinLat);
            Assert.AreEqual(41.0d, coordsRect.MaxLon);
            Assert.AreEqual(41.0d, coordsRect.MaxLat);
        }

        [Test]
        public void Test_CoordToStr()
        {
            string coord = GEDCOMUtils.CoordToStr(2.005216);
            Assert.AreEqual("2.005216", coord);
        }

        [Test]
        public void Test_PlacesCache_GetPlacePoints()
        {
            Assert.Throws(typeof(ArgumentNullException), () => { PlacesCache.Instance.GetPlacePoints(null, null); });
            Assert.Throws(typeof(ArgumentNullException), () => { PlacesCache.Instance.GetPlacePoints("test", null); });
        }
    }
}
