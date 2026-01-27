/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore.Design.Controls;
using GKCore.Design.Graphics;
using Terminal.Gui.Views;

namespace GKUI.Platform.Handlers
{
    public sealed class ButtonHandler : BaseControlHandler<Button, ButtonHandler>, IButton
    {
        public ButtonHandler(Button control) : base(control)
        {
        }

        public string Text
        {
            get { return Control.Text.ToString(); }
            set { Control.Text = value; }
        }

        public IImage Glyph
        {
            get { return null; }
            set { }
        }
    }
}
