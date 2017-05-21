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
using System.Drawing;
using GKCommon;
using GKCommon.GEDCOM;
using GKCore.Interfaces;
using GKCore.Options;

namespace GKCore.Charts
{
    public interface ITreeChartBox
    {
        IBaseWindow Base { get; set; }
        int DepthLimit { get; set; }
        int Height { get; set; }
        TreeChartKind Kind { get; set; }
        TreeChartModel Model { get; }
        TreeChartOptions Options { get; set; }
        float Scale { get; }
        int Width { get; set; }

        void GenChart(GEDCOMIndividualRecord iRec, TreeChartKind kind, bool rootCenter);
        ExtRect GetClientRect();
        Size GetImageSize();
        Point GetOffsets();
        void Invalidate();
        void RenderStatic(BackgroundMode background, bool centered);
        void SetRenderer(ChartRenderer renderer);
        void SetScale(float value);
    }
}
