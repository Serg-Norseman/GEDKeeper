/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

namespace GKCore.Design.Controls
{
    public interface ITabPages : IControlItems<ITabPage>
    {
    }


    public interface ITabControl : IBaseControl
    {
        int SelectedIndex { get; set; }
        ITabPages Pages { get; }

        void SetTabVisible(ITabPage tabPage, bool visible);
    }
}
