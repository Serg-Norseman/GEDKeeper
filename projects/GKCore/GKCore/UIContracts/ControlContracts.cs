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
using System.Reflection;

using GKCommon;
using GKCore.Geocoding;

namespace GKCore.UIContracts
{
    /// <summary>
    /// 
    /// </summary>
    public interface IMapBrowser
    {
        bool ShowPoints { get; set; }
        bool ShowLines { get; set; }
        ExtList<GeoPoint> MapPoints { get; }

        int AddPoint(double latitude, double longitude, string hint);
        void ClearPoints();
        void DeletePoint(int index);
        void BeginUpdate();
        void EndUpdate();
        void InitMap();
        void RefreshPoints();
        void SaveSnapshot(string fileName);
        void SetCenter(double latitude, double longitude, int scale);
        void ZoomToBounds();
    }


    /// <summary>
    /// 
    /// </summary>
    public interface IX
    {
        
    }


    /// <summary>
    /// 
    /// </summary>
    public interface IBaseController
    {
    }


    /// <summary>
    /// 
    /// </summary>
    public interface IPathReplacer
    {
        void Load(string fileName);
        bool TryReplacePath(string path, out string newPath);
    }


    /// <summary>
    /// 
    /// </summary>
    public interface IUtilities
    {
        Assembly GetExecutingAssembly();
        Version GetAppVersion();
        string GetAppCopyright();

        int GetKeyLayout();
        void SetKeyLayout(int layout);
    }


    /// <summary>
    /// 
    /// </summary>
    public interface ITextControl
    {
        void AppendText(string text);
        void Clear();
    }
}
