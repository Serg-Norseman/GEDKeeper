/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
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
using System.Windows.Forms;
using GKCore.Design.Controls;
using GKCore.Design.Graphics;
using GKUI.Platform.Handlers;

namespace GKUI.Platform
{
    /// <summary>
    /// 
    /// </summary>
    public class MenuItemEx : ToolStripMenuItem, IMenuItem
    {
        private ItemAction fAction;
        private IImage fGlyph;
        private MenuSubItems fItems;


        public IImage Glyph
        {
            get { return fGlyph; }
            set {
                fGlyph = value;
                base.Image = (fGlyph == null) ? null : ((ImageHandler)fGlyph).Handle;
            }
        }


        public MenuItemEx(string text) : base(text)
        {
            fItems = new MenuSubItems(this);
        }

        public MenuItemEx(string text, object tag) : base(text)
        {
            fItems = new MenuSubItems(this);

            Tag = tag;
        }

        public MenuItemEx(string text, object tag, IImage image, ItemAction action) : base(text)
        {
            fItems = new MenuSubItems(this);

            Click += Item_Click;
            Tag = tag;
            Glyph = image;
            fAction = action;
        }

        public IMenuItems SubItems
        {
            get { return fItems; }
        }

        public IMenuItem AddItem(string text, object tag, IImage image, ItemAction action)
        {
            var item = new MenuItemEx(text, tag, image, action);
            DropDownItems.Add(item);
            return item;
        }

        public void ClearItems()
        {
            DropDownItems.Clear();
        }

        private void Item_Click(object sender, EventArgs e)
        {
            fAction?.Invoke(this);
        }
    }
}
