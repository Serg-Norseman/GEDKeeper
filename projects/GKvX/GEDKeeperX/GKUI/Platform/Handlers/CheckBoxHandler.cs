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

using GKCore.Design.Controls;
using Xamarin.Forms;
using XFIKCheckBox = Plugin.InputKit.Shared.Controls.CheckBox;

namespace GKUI.Platform
{
    public sealed class CheckBoxHandler : BaseControlHandler<XFIKCheckBox, CheckBoxHandler>, ICheckBox
    {
        public CheckBoxHandler(XFIKCheckBox control) : base(control)
        {
            control.Color = Color.Black;
            control.Padding = new Thickness(0, 0);
        }

        public bool Checked
        {
            get { return Control.IsChecked; }
            set { Control.IsChecked = value; }
        }

        public string Text
        {
            get { return Control.Text; }
            set { Control.Text = value; }
        }

        public bool Visible
        {
            get { return Control.IsVisible; }
            set { Control.IsVisible = value; }
        }
    }
}
