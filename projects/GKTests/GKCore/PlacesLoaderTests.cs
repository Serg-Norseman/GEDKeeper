/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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
using GKCore.Maps;
using NUnit.Framework;

namespace GKTests.GKCore
{
    [TestFixture]
    public class PlacesLoaderTests
    {
        [TestFixtureSetUp]
        public void Init()
        {
        }

        [Test]
        public void Test_PlaceRef()
        {
            var placeRef = new PlaceRef(null);
            Assert.IsNotNull(placeRef);
            Assert.IsNull(placeRef.Event);
            Assert.AreEqual(0, placeRef.Date.ToBinary());
        }

        [Test]
        public void Test_MapPlace()
        {
            var mapPlace = new MapPlace();
            Assert.IsNotNull(mapPlace);
            Assert.IsNotNull(mapPlace.Points);
            Assert.IsNotNull(mapPlace.PlaceRefs);
        }

        [Test]
        public void Test_GetPointsFrame()
        {
            CoordsRect coordsRect = PlacesLoader.GetPointsFrame(null);
            Assert.AreEqual(0.0d, coordsRect.MinLon);
            Assert.AreEqual(0.0d, coordsRect.MinLat);
            Assert.AreEqual(0.0d, coordsRect.MaxLon);
            Assert.AreEqual(0.0d, coordsRect.MaxLat);
        }

        [Test]
        public void Test_CoordToStr()
        {
            string coord = PlacesLoader.CoordToStr(2.005216);
            Assert.AreEqual("2.005216", coord);
        }
    }
}
