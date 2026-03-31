/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Linq;
using GKCore.Design.Controls;
using GKCore.Design.Graphics;
using GKUI.Platform.Handlers;
using Terminal.Gui;

namespace GKUI.Platform
{
    /// <summary>
    /// 
    /// </summary>
    public class MenuItemEx : MenuBarItem, IMenuItem
    {
        private ItemAction fAction;
        private MenuSubItems fItems;

        public bool Enabled
        {
            get { return IsEnabled(); }
            set { /* TODO! */ }
        }

        public IImage Glyph
        {
            get { return null; }
            set { }
        }

        public IMenuItems SubItems
        {
            get { return fItems; }
        }

        public string Text
        {
            get { return Title.ToString(); }
            set { Title = value; }
        }

        public bool Visible
        {
            get { return true; }
            set { }
        }

        public MenuItemEx(string text)
        {
            fItems = new MenuSubItems(this);
            Title = text;
        }

        public MenuItemEx(string text, object tag)
        {
            fItems = new MenuSubItems(this);
            Title = text;
            Tag = tag;
        }

        public MenuItemEx(string text, object tag, IImage image, ItemAction action)
        {
            fItems = new MenuSubItems(this);
            Title = text;
            Action += Item_Click;
            Tag = tag;
            fAction = action;
        }

        public int ItemsCount
        {
            get {
                return 0;
                //Items.Count;
            }
        }

        public IMenuItem AddItem(string text, object tag, IImage image, ItemAction action)
        {
            IMenuItem result = null;
            if (this is MenuBarItem barItem) {
                var item = new MenuItemEx(text, tag, image, action);
                barItem.Children.Add(item);
                result = item;
            }
            return null;
        }

        public void ClearItems()
        {
            //Items.Clear();
        }

        private void Item_Click(object sender, EventArgs e)
        {
            if (fAction != null) {
                fAction(this);
            }
        }
    }
}
