/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore.Design.Controls;
using GKCore.Design.Graphics;
using GKUI.Components;
using Xamarin.Forms;

namespace GKUI.Platform
{
    public sealed class LabelHandler : BaseControlHandler<Label, LabelHandler>, ILabel
    {
        public LabelHandler(Label control) : base(control)
        {
        }

        public IColor BackColor
        {
            get { return new ColorHandler(Control.BackgroundColor); }
            set { Control.BackgroundColor = ((ColorHandler)value).Handle; }
        }

        public string Text
        {
            get { return Control.Text; }
            set { Control.Text = value; }
        }
    }
}
