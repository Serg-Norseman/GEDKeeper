/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;
using GKCore.Design.Controls;
using NSubstitute;
using NUnit.Framework;

namespace GKCore.Maps
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
            /*var geocoder = IGeocoder.Create("Yandex", "ru");
            var geoPoints = geocoder.Geocode("Москва", 1);
            Assert.IsNotNull(geoPoints);
            //Assert.IsTrue(geoPoints.Count > 0);*/
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
        public void Test_PlacesCache_GetPlacePoints()
        {
            Assert.Throws(typeof(ArgumentNullException), () => { PlacesCache.Instance.GetPlacePoints(null, null); });
            Assert.Throws(typeof(ArgumentNullException), () => { PlacesCache.Instance.GetPlacePoints("test", null); });
        }
    }
}
