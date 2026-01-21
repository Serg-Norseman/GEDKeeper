/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore.Design.Controls;
using GKCore.Design.Graphics;
using GKUI.Platform.Handlers;
using Terminal.Gui;

namespace GKUI.Platform
{
    /// <summary>
    /// 
    /// </summary>
    public class MenuItemEx : MenuItem, IMenuItem
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

        public object Tag
        {
            get { return Data; }
            set { Data = value; }
        }

        public string Text
        {
            get { return Title.ToString(); }
            set { Title = value; }
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
            Data = tag;
        }

        public MenuItemEx(string text, object tag, IImage image, ItemAction action)
        {
            fItems = new MenuSubItems(this);
            Title = text;
            Action = Item_Click;
            Data = tag;
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
            /*var item = new MenuItemEx(text, tag, image, action);
            Items.Add(item);
            return item;*/
            return null;
        }

        public void ClearItems()
        {
            //Items.Clear();
        }

        private void Item_Click()
        {
            if (fAction != null) {
                fAction(this);
            }
        }
    }
}
