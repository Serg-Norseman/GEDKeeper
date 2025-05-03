﻿/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
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

using BSLib;
using GKCore.Controllers;
using GKCore.Design.Graphics;
using GKCore.Interfaces;
using GKUI.Themes;

namespace GKCore.Design.Controls
{
    /// <summary>
    /// This control is used only in MediaViewerWin and PortraitSelectDlg.
    /// </summary>
    public interface IImageView : IBaseControl, ILocalizable, IThemedView
    {
        ExtRect SelectionRegion { get; set; }
        bool ShowNamedRegionTips { get; set; }

        void ClearNamedRegions();
        void AddNamedRegion(string name, ExtRect region);
        void OpenImage(MediaViewerController controller, IImage image);
        void Refresh();
    }
}
