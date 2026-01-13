/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GDModel;

namespace GKCore.Maps
{
    public sealed class GeoPoint
    {
        public double Latitude { get; set; }
        public double Longitude { get; set; }
        public string Hint { get; set; }
        public GDMCustomDate Date { get; set; }

        public GeoPoint(double latitude, double longitude, string hint, GDMCustomDate date = null)
        {
            Latitude = latitude;
            Longitude = longitude;
            Hint = hint;
            Date = date;
        }
    }
}
