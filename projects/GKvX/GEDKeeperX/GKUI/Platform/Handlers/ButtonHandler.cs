/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore.Design.Controls;
using GKCore.Design.Graphics;
using Xamarin.Forms;

namespace GKUI.Platform
{
    public sealed class ButtonHandler : BaseControlHandler<Button, ButtonHandler>, IButton
    {
        public ButtonHandler(Button control) : base(control)
        {
        }

        public IImage Glyph { get; set; }

        public string Text
        {
            get { return Control.Text; }
            set { Control.Text = value; }
        }
    }
}
