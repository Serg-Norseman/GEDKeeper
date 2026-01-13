/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore.Design.Controls;

namespace GKCore.Plugins
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class WidgetInfo
    {
        public IWidgetPlugin Widget;
        public IMenuItem MenuItem;

        public WidgetInfo(IWidgetPlugin widget, IMenuItem menuItem)
        {
            Widget = widget;
            MenuItem = menuItem;
        }
    }
}
