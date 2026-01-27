/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Linq;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Graphics;
using Terminal.Gui.Views;

namespace GKUI.Platform.Handlers
{
    internal class MenuSubItems : IMenuItems
    {
        private MenuItem fItem;

        public IMenuItem this[int index]
        {
            get {
                var barItem = fItem as MenuBarItem;
                if (barItem == null)
                    throw new Exception("Type mismatch");

                var subItems = barItem.PopoverMenu.Root.SubViews.ToArray();
                if (index < 0 || index >= subItems.Length)
                    throw new ArgumentOutOfRangeException(nameof(index));

                return new MenuItemHandler(subItems[index] as MenuItem);
            }
        }

        public int Count
        {
            get {
                return (fItem is MenuBarItem barItem) ? barItem.PopoverMenu.Root.SubViews.Count : 0;
            }
        }

        public MenuSubItems(MenuItem control)
        {
            fItem = control;
        }
    }


    public sealed class MenuItemHandler : ControlHandler<MenuItem, MenuItemHandler>, IMenuItem
    {
        private MenuSubItems fItems;

        public MenuItemHandler(MenuItem control) : base(control)
        {
            fItems = new MenuSubItems(control);
        }

        public bool Checked
        {
            get {
                return false/*Control.Checked*/;
            }
            set {
                //Control.Checked = value;
            }
        }

        public bool Enabled
        {
            get { return Control.Enabled; }
            set { Control.Enabled = value; }
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
            get { return Control.Data; }
            set { Control.Data = value; }
        }

        public string Text
        {
            get { return Control.Title.ToString(); }
            set { Control.Title = value; }
        }

        public int ItemsCount
        {
            get { return (Control is MenuBarItem barItem) ? barItem.PopoverMenu.Root.SubViews.Count : 0; }
        }

        public IMenuItem AddItem(string text, object tag, IImage image, ItemAction action)
        {
            if (Control is MenuBarItem barItem) {
                var item = new MenuItemEx(text, tag, image, action);
                barItem.PopoverMenu.Root.Add(item);
                return item;
            } else {
                return null;
            }
        }

        public void ClearItems()
        {
            if (Control is MenuBarItem barItem) {
                barItem.PopoverMenu.Root.RemoveAll();
            }
        }
    }
}
