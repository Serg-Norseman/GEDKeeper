/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2018 by Sergey V. Zhdanovskih.
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
using BSLib;
using BSLib.Design.Graphics;
using BSLib.Design.MVP;
using GDModel;
using GKCore.Interfaces;
using GKCore.Maps;
using GKCore.Stats;

namespace GKCore.MVP.Controls
{
    public interface IDateBox : IBaseControl
    {
        string NormalizeDate { get; set; }
        bool ReadOnly { get; set; }
        string SelectedText { get; set; }
        string Text { get; set; }

        void Clear();
        void Copy();
        void SelectAll();
    }


    public interface ILogChart : IBaseControl
    {
        void AddFragment(int val);
        void Clear();
    }


    /// <summary>
    /// 
    /// </summary>
    public interface IMapBrowser : IBaseControl
    {
        bool ShowPoints { get; set; }
        bool ShowLines { get; set; }
        ExtList<GeoPoint> MapPoints { get; }

        int AddPoint(double latitude, double longitude, string hint);
        void ClearPoints();
        void DeletePoint(int index);
        void BeginUpdate();
        void EndUpdate();
        void RefreshPoints();
        void SaveSnapshot(string fileName);
        void SetCenter(double latitude, double longitude, int scale);
        void ZoomToBounds();
    }


    /// <summary>
    /// 
    /// </summary>
    public interface IPortraitControl : IBaseControl
    {
        int Height { get; set; }
        int Width { get; set; }
    }


    public enum ChartStyle
    {
        Bar,
        Point,
        ClusterBar
    }

    /// <summary>
    /// 
    /// </summary>
    public interface IGraphControl : IBaseControl
    {
        void Clear();
        void PrepareArray(string title, string xAxis, string yAxis, ChartStyle style, bool excludeUnknowns, List<StatsItem> vals);
    }


    /// <summary>
    /// 
    /// </summary>
    public interface IDateControl : IBaseControl
    {
        GDMCustomDate Date { get; set; }
    }


    /// <summary>
    /// 
    /// </summary>
    public interface IMergeControl : IBaseControl
    {
        IBaseWindow Base { get; set; }
        GDMRecordType MergeMode { get; set; }
        GDMRecord Rec1 { get; }
        GDMRecord Rec2 { get; }

        void SetRec1(GDMRecord value);
        void SetRec2(GDMRecord value);
    }


    /// <summary>
    /// 
    /// </summary>
    public interface IHyperView : IBaseControl
    {
        StringList Lines { get; }
    }


    /// <summary>
    /// 
    /// </summary>
    public interface IImageView : IBaseControl
    {
        ExtRect SelectionRegion { get; set; }
        bool ShowNamedRegionTips { get; set; }

        void AddNamedRegion(string name, ExtRect region);
        void OpenImage(IImage image);
    }


    public interface IFilterGridView : IBaseControl
    {
        int Count { get; }
        FilterCondition this[int index] { get; }

        void AddCondition(FilterCondition fcond);
        void Clear();
    }
}
