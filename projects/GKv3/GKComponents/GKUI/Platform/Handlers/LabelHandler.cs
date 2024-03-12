/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2024 by Sergey V. Zhdanovskih.
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
using GKCore.Design.Controls;

namespace GKUI.Platform.Handlers
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

        public bool Visible
        {
            get { return Control.Visible; }
            set { Control.Visible = value; }
        }
    }
}
