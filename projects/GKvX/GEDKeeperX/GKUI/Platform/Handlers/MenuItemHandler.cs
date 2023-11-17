/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
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
            set {  }
        }

        public string Text
        {
            get { return string.Empty; }
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
