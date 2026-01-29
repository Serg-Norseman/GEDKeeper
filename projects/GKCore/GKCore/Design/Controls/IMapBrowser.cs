/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

#if !TERM

using System.Collections.Generic;
using GKCore.Maps;
using GKMap;

namespace GKCore.Design.Controls
{
    /// <summary>
    /// 
    /// </summary>
    public interface IMapBrowser : IBaseControl
    {
        IMapControl MapControl { get; }

        bool ShowPoints { get; set; }
        bool ShowLines { get; set; }
        IList<GeoPoint> MapPoints { get; }
        PointLatLng TargetPosition { get; set; }

        int AddPoint(double latitude, double longitude, string hint);
        int AddPoint(GeoPoint pt);
        void ClearPoints();
        void DeletePoint(int index);
        void BeginUpdate();
        void EndUpdate();
        void RefreshPoints();
        void SaveSnapshot(string fileName);
        void SetCenter(double latitude, double longitude, int scale);
        void ZoomToBounds();
    }
}

#endif
