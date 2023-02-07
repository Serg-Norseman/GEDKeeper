/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
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

using Eto.Forms;
using GKCore.Design.Graphics;
using GKCore.Design;
using GKCore.Design.Controls;

namespace GKUI.Platform.Handlers
{
    public sealed class MenuItemHandler : ControlHandler<MenuItem, MenuItemHandler>, IMenuItem
    {
        private IImage fGlyph;
        private MenuSubItems fItems;

        public MenuItemHandler(MenuItem control) : base(control)
        {
            fItems = new MenuSubItems(control);
        }

        public bool Checked
        {
            get {
                if (Control is RadioMenuItem rmi) {
                    return rmi.Checked;
                } else if (Control is CheckMenuItem cmi) {
                    return cmi.Checked;
                } else {
                    return false;
                }
            }
            set {
                if (Control is RadioMenuItem rmi) {
                    rmi.Checked = value;
                } else if (Control is CheckMenuItem cmi) {
                    cmi.Checked = value;
                }
            }
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
                if (Control is ButtonMenuItem) {
                    fGlyph = value;
                    ((ButtonMenuItem)Control).Image = fGlyph == null ? null : ((ImageHandler)fGlyph).Handle;
                }
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

        public IMenuItem AddItem(string text, object tag, IImage image, ItemAction action)
        {
            if (Control is ButtonMenuItem) {
                var item = new MenuItemEx(text, tag, image, action);
                ((ButtonMenuItem)Control).Items.Add(item);
                return item;
            } else {
                return null;
            }
        }

        public void ClearItems()
        {
            if (Control is ButtonMenuItem) {
                ((ButtonMenuItem)Control).Items.Clear();
            }
        }
    }
}
