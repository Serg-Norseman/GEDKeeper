/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
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
                if (Control is ButtonMenuItem btnItem) {
                    fGlyph = value;
                    btnItem.Image = fGlyph == null ? null : ((ImageHandler)fGlyph).Handle;
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
            if (Control is ButtonMenuItem btnItem) {
                var item = new MenuItemEx(text, tag, image, action);
                btnItem.Items.Add(item);
                return item;
            } else {
                return null;
            }
        }

        public void ClearItems()
        {
            if (Control is ButtonMenuItem btnItem) {
                btnItem.Items.Clear();
            }
        }
    }
}
