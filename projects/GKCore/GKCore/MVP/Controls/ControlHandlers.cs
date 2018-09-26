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
using GKCommon.GEDCOM;
using GKCore.Interfaces;
using GKCore.Maps;
using GKCore.Stats;

namespace GKCore.MVP.Controls
{
    public interface ILabelHandler : IBaseControl
    {
        string Text { get; set; }
    }


    public interface IButtonHandler : IBaseControl
    {
        string Text { get; set; }
    }


    public interface ICheckBoxHandler : IBaseControl
    {
        bool Checked { get; set; }
        string Text { get; set; }
    }


    public interface IRadioButtonHandler : IBaseControl
    {
        bool Checked { get; set; }
        string Text { get; set; }
    }


    public interface IComboBoxHandler : IBaseControl
    {
        bool ReadOnly { get; set; }
        int SelectedIndex { get; set; }
        object SelectedItem { get; set; }
        object SelectedTag { get; set; }
        string Text { get; set; }

        void Add(object item);
        void AddItem(string caption, object tag, IImage image = null);
        void AddRange(object[] items, bool sorted = false);
        void AddStrings(StringList strings);
        void BeginUpdate();
        void Clear();
        void EndUpdate();
        void SortItems();
    }


    public interface ITextBoxHandler : IBaseControl
    {
        string[] Lines { get; set; }
        bool ReadOnly { get; set; }
        string SelectedText { get; set; }
        string Text { get; set; }

        void AppendText(string text);
        void Clear();
        void Copy();
        void SelectAll();
    }


    public interface INumericBoxHandler : IBaseControl
    {
        bool ReadOnly { get; set; }
        string Text { get; set; }
        double Value { get; set; }
    }


    public interface ITVNode
    {
        object Tag { get; set; }
    }

    public interface ITreeViewHandler : IBaseControl
    {
        ITVNode AddNode(ITVNode parent, string name, object tag);
        void BeginUpdate();
        void Clear();
        void EndUpdate();
        void Expand(ITVNode node);
    }


    public interface IProgressBarHandler : IBaseControl
    {
        int Minimum { get; set; }
        int Maximum { get; set; }
        int Value { get; set; }

        void Increment(int value);
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
        void InitMap();
        void RefreshPoints();
        void SaveSnapshot(string fileName);
        void SetCenter(double latitude, double longitude, int scale);
        void ZoomToBounds();
    }


    public delegate void ItemAction(IMenuItem sender);

    /// <summary>
    /// 
    /// </summary>
    public interface IMenuItem : IControl
    {
        bool Checked { get; set; }
        bool Enabled { get; set; }
        int ItemsCount { get; }
        object Tag { get; set; }

        IMenuItem AddItem(string text, object tag, IImage image, ItemAction action);
        void ClearItems();
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
    public interface IMergeControl : IBaseControl
    {
        IBaseWindow Base { get; set; }
        GEDCOMRecordType MergeMode { get; set; }
        GEDCOMRecord Rec1 { get; }
        GEDCOMRecord Rec2 { get; }

        void SetRec1(GEDCOMRecord value);
        void SetRec2(GEDCOMRecord value);
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


    /// <summary>
    /// 
    /// </summary>
    public interface ITabControl : IBaseControl
    {
        int SelectedIndex { get; set; }
    }


    public interface IFilterGridView : IBaseControl
    {
        int Count { get; }
        FilterCondition this[int index] { get; }

        void AddCondition(FilterCondition fcond);
        void Clear();
    }
}
