﻿/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2018-2025 by Sergey V. Zhdanovskih.
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

namespace GKCore.Maps
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class PlacesCache
    {
        private static PlacesCache fInstance = null;

        public static PlacesCache Instance
        {
            get {
                if (fInstance == null) fInstance = new PlacesCache();
                return fInstance;
            }
        }

        private readonly Dictionary<string, List<GeoPoint>> fMemoryCache;

        private PlacesCache()
        {
            fMemoryCache = new Dictionary<string, List<GeoPoint>>();
        }

        private static string GetCachFilename()
        {
            string path = AppHost.GetAppDataPathStatic() + "geocache.yml";
            return path;
        }

        public void GetPlacePoints(string searchValue, List<GeoPoint> pointsList, short results = 1)
        {
            if (string.IsNullOrEmpty(searchValue))
                throw new ArgumentNullException(@"searchValue");

            if (pointsList == null)
                throw new ArgumentNullException(@"pointsList");

            try {
                List<GeoPoint> cachedPoints;

                if (!fMemoryCache.TryGetValue(searchValue, out cachedPoints) || cachedPoints.Count != results) {
                    cachedPoints = new List<GeoPoint>();

                    AppHost.Instance.RequestGeoCoords(searchValue, cachedPoints, results);

                    if (cachedPoints.Count > 0) {
                        fMemoryCache[searchValue] = cachedPoints;
                    }
                }

                pointsList.AddRange(cachedPoints);
            } catch (Exception ex) {
                Logger.WriteError("PlacesCache.GetPlacePoints()", ex);
            }
        }

        public void Load()
        {
            /*string fileName = GetCachFilename();
            if (!File.Exists(fileName)) return;

            try {
                using (var reader = new StreamReader(fileName)) {
                    string content = reader.ReadToEnd();
                    var rawData = YamlHelper.Deserialize(content, typeof(Dictionary<string, GeoPoint>));
                    fMemoryCache = rawData[0] as Dictionary<string, GeoPoint>;
                }
            } catch (Exception ex) {
                Logger.WriteError("PlacesCache.Load()", ex);
            }*/
        }

        public void Save()
        {
            /*string fileName = GetCachFilename();

            try {
                using (var writer = new StreamWriter(fileName)) {
                    string content = YamlHelper.Serialize(fMemoryCache);
                    writer.Write(content);
                }
            } catch (Exception ex) {
                Logger.WriteError("PlacesCache.Save()", ex);
            }*/
        }
    }
}
