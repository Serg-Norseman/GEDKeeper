/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using GKCore.Design.Controls;
using GKCore.Design.Graphics;
using Terminal.Gui;

namespace GKUI.Platform.Handlers
{
    public sealed class LabelHandler : BaseControlHandler<Label, LabelHandler>, ILabel
    {
        public LabelHandler(Label control) : base(control)
        {
        }

        public IColor BackColor
        {
            get { return new ColorHandler(Control.ColorScheme.Normal.Background); }
            set {
                //Control.ColorScheme.Normal.Background = ((ColorHandler)value).Handle;
            }
        }

        public string Text
        {
            get { return Control.Text.ToString(); }
            set { Control.Text = value; }
        }
    }
}
