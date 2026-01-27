/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore.Design;
using GKCore.Design.Controls;
using Terminal.Gui.Views;

namespace GKUI.Platform.Handlers
{
    public sealed class TabPageHandler : ControlHandler<Tab, TabPageHandler>, ITabPage
    {
        public TabPageHandler(Tab control) : base(control)
        {
        }

        public string Text
        {
            get { return Control.DisplayText; }
            set { Control.DisplayText = value; }
        }

        public bool Enabled
        {
            get { return Control.Enabled; }
            set { Control.Enabled = value; }
        }

        public bool Visible
        {
            get { return Control.Visible; }
            set { Control.Visible = value; }
        }

        public void Activate()
        {
            Control.SetFocus();
        }
    }
}
