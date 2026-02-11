/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
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
                throw new ArgumentNullException(nameof(searchValue));

            if (pointsList == null)
                throw new ArgumentNullException(nameof(pointsList));

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
