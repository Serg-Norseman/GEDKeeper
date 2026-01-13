/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using BSLib;
using GKCore.Controllers;
using GKCore.Design.Graphics;
using GKCore.Locales;
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
