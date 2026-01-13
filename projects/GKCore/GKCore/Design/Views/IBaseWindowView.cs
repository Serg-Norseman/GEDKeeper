/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore.Design.Controls;

namespace GKCore.Design.Views
{
    public interface IBaseWindowView : IBaseWindow
    {
        ITabControl RecordTabs { get; }
        IMenuItem ReportsItem { get; }
        IMenuItem PluginsItem { get; }

        void LoadBase(string fileName);

        void EnableSplitterEvent(object control, bool enable);
    }


    public interface IPartialView : IWorkWindow, IWindowDependent
    {
    }
}
