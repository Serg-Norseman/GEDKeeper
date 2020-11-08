/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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

using System;
using BSLib.Design.Graphics;
using BSLib.Design.MVP.Controls;
using Eto.Forms;

namespace GKUI.Components
{
    /// <summary>
    /// 
    /// </summary>
    public class MenuItemEx : ButtonMenuItem, IMenuItem
    {
        private ItemAction fAction;

        public bool Checked
        {
            get; set;
        }

        public MenuItemEx(string text) : base()
        {
            Text = text;
        }

        public MenuItemEx(string text, object tag) : base()
        {
            Text = text;
            Tag = tag;
        }

        public MenuItemEx(string text, object tag, IImage image, ItemAction action) : base()
        {
            Text = text;
            Click += Item_Click;
            Tag = tag;
            ImageHandler hIcon = image as ImageHandler;
            Image = (hIcon == null) ? null : hIcon.Handle;
            fAction = action;
        }

        public int ItemsCount { get { return Items.Count; } }

        public IMenuItem AddItem(string text, object tag, IImage image, ItemAction action)
        {
            var item = new MenuItemEx(text, tag, image, action);
            Items.Add(item);
            return item;
        }

        public void ClearItems()
        {
            Items.Clear();
        }

        private void Item_Click(object sender, EventArgs e)
        {
            if (fAction != null) {
                fAction(this);
            }
        }
    }
}
