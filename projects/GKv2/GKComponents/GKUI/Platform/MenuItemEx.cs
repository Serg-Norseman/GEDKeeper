/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
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
