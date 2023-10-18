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

using System.Collections.Generic;
using GKCore.Design.Controls;
using GKCore.Maps;
using Xamarin.Forms;

namespace GKUI.Components
{
    public class GKMapBrowser : ContentView, IMapBrowser
    {
        public bool ShowLines { get; set; }
        public bool ShowPoints { get; set; }

        public IList<GeoPoint> MapPoints { get; }

        public bool Enabled { get; set; }

        public void Activate()
        {
        }

        public int AddPoint(double latitude, double longitude, string hint)
        {
            return 0;
        }

        public void BeginUpdate()
        {
        }

        public void ClearPoints()
        {
        }

        public void DeletePoint(int index)
        {
        }

        public void EndUpdate()
        {
        }

        public void RefreshPoints()
        {
        }

        public void SaveSnapshot(string fileName)
        {
        }

        public void SetCenter(double latitude, double longitude, int scale)
        {
        }

        public void ZoomToBounds()
        {
        }
    }
}
