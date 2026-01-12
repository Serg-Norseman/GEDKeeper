/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using Eto.Forms;
using GKCore.Design.Controls;
using GKCore.Design.Graphics;
using GKUI.Platform.Handlers;

namespace GKUI.Platform
{
    /// <summary>
    /// 
    /// </summary>
    public class MenuItemEx : ButtonMenuItem, IMenuItem
    {
        private ItemAction fAction;
        private IImage fGlyph;
        private MenuSubItems fItems;

        public bool Checked
        {
            get; set;
        }

        public IImage Glyph
        {
            get { return fGlyph; }
            set {
                fGlyph = value;
                base.Image = (fGlyph == null) ? null : ((ImageHandler)fGlyph).Handle;
            }
        }


        public MenuItemEx(string text)
        {
            fItems = new MenuSubItems(this);
            Text = text;
        }

        public MenuItemEx(string text, object tag)
        {
            fItems = new MenuSubItems(this);
            Text = text;
            Tag = tag;
        }

        public MenuItemEx(string text, object tag, IImage image, ItemAction action)
        {
            fItems = new MenuSubItems(this);
            Text = text;
            Click += Item_Click;
            Tag = tag;
            ImageHandler hIcon = image as ImageHandler;
            Image = (hIcon == null) ? null : hIcon.Handle;
            fAction = action;
        }

        public IMenuItems SubItems
        {
            get { return fItems; }
        }

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
            fAction?.Invoke(this);
        }
    }
}
