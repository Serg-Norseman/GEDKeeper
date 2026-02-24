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
    public sealed class MenuItemHandler : ControlHandler<MenuItem, MenuItemHandler>, IMenuItem
    {
        public MenuItemHandler(MenuItem control) : base(control)
        {
        }

        public bool Checked
        {
            get { return false; }
            set { }
        }

        public bool Enabled
        {
            get { return Control.IsEnabled; }
            set { Control.IsEnabled = value; }
        }

        public IImage Glyph
        {
            get { return null; }
            set { }
        }

        public IMenuItems SubItems
        {
            get { return null; }
            set { }
        }

        public object Tag
        {
            get { return null; }
            set { }
        }

        public string Text
        {
            get { return string.Empty; }
            set { }
        }

        public bool Visible
        {
            get { return true; }
            set { }
        }

        public int ItemsCount
        {
            get { return 0; }
        }

        public IMenuItem AddItem(string text, object tag, IImage image, ItemAction action)
        {
            return null;
        }

        public void ClearItems()
        {
        }
    }
}
