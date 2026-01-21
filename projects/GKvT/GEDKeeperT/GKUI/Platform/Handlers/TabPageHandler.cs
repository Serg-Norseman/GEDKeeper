/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore.Design;
using GKCore.Design.Controls;
using Terminal.Gui;

namespace GKUI.Platform.Handlers
{
    public sealed class TabPageHandler : ControlHandler<TabView.Tab, TabPageHandler>, ITabPage
    {
        public TabPageHandler(TabView.Tab control) : base(control)
        {
        }

        public string Text
        {
            get { return Control.Text.ToString(); }
            set { Control.Text = value; }
        }

        public bool Enabled
        {
            get { return true; }
            set { }
        }

        public bool Visible
        {
            get { return true; }
            set { }
        }

        public void Activate()
        {
        }
    }
}
