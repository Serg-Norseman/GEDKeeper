/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using Eto.Forms;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Graphics;

namespace GKUI.Platform.Handlers
{
    public class ButtonToolItemHandler : ControlHandler<ToolItem, ButtonToolItemHandler>, IButtonToolItem
    {
        private IImage fGlyph;

        public ButtonToolItemHandler(ToolItem control) : base(control)
        {
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
                Control.Image = fGlyph == null ? null : ((ImageHandler)fGlyph).Handle;
            }
        }

        public string Text
        {
            get { return Control.Text; }
            set {
                // Bug in all windows where controllers use GetControl<IButtonToolItem>("???").Text = "..."
                // Console.WriteLine("ButtonToolItemHandler.SetText(): " + Control.Text + " -> " + value);
                // Debugging showed that real values ​​come here (Gtk)
                Control.Text = value;
            }
        }

        public bool Visible
        {
            get { return Control.Visible; }
            set { Control.Visible = value; }
        }
    }
}
