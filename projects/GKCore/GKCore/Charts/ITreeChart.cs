/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2020 by Sergey V. Zhdanovskih.
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
using BSLib;
using GDModel;
using GKCore.Options;

namespace GKCore.Charts
{
    public enum ChartControlMode
    {
        Default,
        DragImage,
        ControlsVisible
    }

    public enum MouseAction
    {
        None,
        Select,
        Expand,
        Drag,
        Properties,
        Highlight,
        PersonExpand,
        Info,
    }

    public enum MouseEvent
    {
        meDown,
        meMove,
        meUp
    }

    public interface ITreeChart : IChart
    {
        int DepthLimitAncestors { get; set; }
        int DepthLimitDescendants { get; set; }
        int Height { get; set; }
        TreeChartKind Kind { get; set; }
        TreeChartModel Model { get; }
        TreeChartOptions Options { get; set; }
        float Scale { get; }
        TreeChartPerson Selected { get; set; }
        int Width { get; set; }

        void GenChart(GDMIndividualRecord iRec, TreeChartKind kind, bool rootCenter);
        ExtRect GetClientRect();
        ExtSize GetImageSize();
        ExtPoint GetOffsets();
        void Invalidate();
        void RefreshTree();
        void RenderImage(RenderTarget target, bool forciblyCentered = false);
        void SetRenderer(ChartRenderer renderer);
        void SetScale(float value);
    }
}
