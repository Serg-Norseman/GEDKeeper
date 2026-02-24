/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Graphics;
using Xamarin.Forms;

namespace GKUI.Platform
{
    public sealed class ToolbarItemHandler : ControlHandler<ToolbarItem, ToolbarItemHandler>, IToolItem
    {
        public ToolbarItemHandler(ToolbarItem control) : base(control)
        {
        }

        public bool Enabled
        {
            get { return Control.IsEnabled; }
            set { Control.IsEnabled = value; }
        }

        public IImage Glyph { get; set; }

        public string Text
        {
            get { return Control.Text; }
            set { Control.Text = value; }
        }

        public bool Visible
        {
            get { return Control.IsEnabled; }
            set { Control.IsEnabled = value; }
        }
    }
}
