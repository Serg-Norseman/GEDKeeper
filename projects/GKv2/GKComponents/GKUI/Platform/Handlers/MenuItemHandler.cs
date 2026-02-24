/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Windows.Forms;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Graphics;

namespace GKUI.Platform.Handlers
{
    internal class MenuSubItems : IMenuItems
    {
        private ToolStripMenuItem fItem;

        public IMenuItem this[int index]
        {
            get {
                if (index < 0 || index >= fItem.DropDownItems.Count)
                    throw new ArgumentOutOfRangeException("index");

                return new MenuItemHandler((ToolStripMenuItem)fItem.DropDownItems[index]);
            }
        }

        public int Count
        {
            get { return fItem.DropDownItems.Count; }
        }

        public MenuSubItems(ToolStripMenuItem control)
        {
            fItem = control;
        }
    }


    public sealed class MenuItemHandler : ControlHandler<ToolStripMenuItem, MenuItemHandler>, IMenuItem
    {
        private IImage fGlyph;
        private MenuSubItems fItems;

        public MenuItemHandler(ToolStripMenuItem control) : base(control)
        {
            fItems = new MenuSubItems(control);
        }

        public bool Checked
        {
            get { return Control.Checked; }
            set { Control.Checked = value; }
        }

        public bool Enabled
        {
            get { return Control.Enabled; }
            set { Control.Enabled = value; }
        }

        public IImage Glyph
        {
            get { return fGlyph; }
            set {
                fGlyph = value;
                Control.Image = (fGlyph == null) ? null : ((ImageHandler)fGlyph).Handle;
            }
        }

        public IMenuItems SubItems
        {
            get { return fItems; }
        }

        public object Tag
        {
            get { return Control.Tag; }
            set { Control.Tag = value; }
        }

        public string Text
        {
            get { return Control.Text; }
            set { Control.Text = value; }
        }

        public bool Visible
        {
            get { return Control.Visible; }
            set { Control.Visible = value; }
        }

        public IMenuItem AddItem(string text, object tag, IImage image, ItemAction action)
        {
            var item = new MenuItemEx(text, tag, image, action);
            Control.DropDownItems.Add(item);
            return item;
        }

        public void ClearItems()
        {
            Control.DropDownItems.Clear();
        }
    }
}
