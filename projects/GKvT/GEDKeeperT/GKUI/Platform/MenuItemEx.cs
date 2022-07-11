/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2022 by Sergey V. Zhdanovskih.
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

using BSLib.Design.Graphics;
using BSLib.Design.MVP.Controls;
using Terminal.Gui;

namespace GKUI.Platform
{
    /// <summary>
    /// 
    /// </summary>
    public class MenuItemEx : MenuItem, IMenuItem
    {
        private ItemAction fAction;

        public bool Enabled
        {
            get { return IsEnabled(); }
            set { /* TODO! */ }
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
            Title = text;
        }

        public MenuItemEx(string text, object tag)
        {
            Title = text;
            Data = tag;
        }

        public MenuItemEx(string text, object tag, IImage image, ItemAction action)
        {
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
