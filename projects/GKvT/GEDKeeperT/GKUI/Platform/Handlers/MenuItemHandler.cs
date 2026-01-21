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
using Terminal.Gui;

namespace GKUI.Platform.Handlers
{
    internal class MenuSubItems : IMenuItems
    {
        private MenuItem fItem;

        public IMenuItem this[int index]
        {
            get {
                var btnItem = fItem as MenuBarItem;
                if (btnItem == null)
                    throw new Exception("Type mismatch");

                if (index < 0 || index >= btnItem.Children.Length)
                    throw new ArgumentOutOfRangeException("index");

                return new MenuItemHandler(btnItem.Children[index]);
            }
        }

        public int Count
        {
            get {
                return (fItem is MenuBarItem barItem && barItem.Children != null) ? barItem.Children.Length : 0;
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
                return Control.Checked;
            }
            set {
                Control.Checked = value;
            }
        }

        public bool Enabled
        {
            get { return Control.IsEnabled(); }
            set {
                if (value) {
                    Control.CanExecute = () => { return true; };
                } else {
                    Control.CanExecute = () => { return false; };
                }
            }
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
            get { return (Control is MenuBarItem) ? ((MenuBarItem)Control).Children.Length : 0; }
        }

        public IMenuItem AddItem(string text, object tag, IImage image, ItemAction action)
        {
            if (Control is MenuBarItem) {
                var item = new MenuItemEx(text, tag, image, action);
                var childrenList = ((MenuBarItem)Control).Children.ToList();
                childrenList.Add(item);
                ((MenuBarItem)Control).Children = childrenList.ToArray();
                return item;
            } else {
                return null;
            }
        }

        public void ClearItems()
        {
            if (Control is MenuBarItem) {
                ((MenuBarItem)Control).Children = null;
            }
        }
    }
}
