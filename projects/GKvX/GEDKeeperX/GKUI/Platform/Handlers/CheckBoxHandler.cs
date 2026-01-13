/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
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
    }
}
