/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using Eto.Forms;
using GKCore.Design.Controls;
using GKCore.Design.Graphics;

namespace GKUI.Platform.Handlers
{
    public sealed class ButtonHandler : BaseControlHandler<Button, ButtonHandler>, IButton
    {
        private IImage fGlyph;


        public ButtonHandler(Button control) : base(control)
        {
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
            set { Control.Text = value; }
        }
    }
}
