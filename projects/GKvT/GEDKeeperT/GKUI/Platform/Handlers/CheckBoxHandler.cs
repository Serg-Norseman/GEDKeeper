/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore.Design.Controls;
using Terminal.Gui.Views;

namespace GKUI.Platform.Handlers
{
    public sealed class CheckBoxHandler : BaseControlHandler<CheckBox, CheckBoxHandler>, ICheckBox
    {
        public CheckBoxHandler(CheckBox control) : base(control)
        {
        }

        public bool Checked
        {
            get { return Control.Value == CheckState.Checked; }
            set { Control.Value = (value) ? CheckState.Checked : CheckState.UnChecked; }
        }

        public string Text
        {
            get { return Control.Text.ToString(); }
            set { Control.Text = value; }
        }
    }
}
