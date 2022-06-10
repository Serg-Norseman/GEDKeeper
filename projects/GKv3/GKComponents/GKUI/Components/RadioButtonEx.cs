/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2022 by Sergey V. Zhdanovskih.
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

using System;
using Eto.Forms;

namespace GKUI.Components
{
    /// <summary>
    /// Extended RadioButton control, for purposes when it is impossible to set a Controller in xeto-file
    /// and it is impossible to use RadioButtonList (a set of interrelated RadioButtons located in different cells
    /// of the TableLayout or at different levels of nested StackLayouts, and interspersed with other dependent controls).
    /// </summary>
    public class RadioButtonEx : RadioButton
    {
        public int GroupIndex { get; set; }


        protected override void OnCheckedChanged(EventArgs e)
        {
            base.OnCheckedChanged(e);

            if (!HasFocus)
                return;

            if (GroupIndex != 0 && Checked) {
                CheckRecursive(ParentWindow);
            }
        }

        private void CheckRecursive(Container container)
        {
            if (container == null)
                return;

            foreach (var ctl in container.Controls) {
                var rbOther = ctl as RadioButtonEx;
                if (rbOther != null && rbOther != this && rbOther.GroupIndex == this.GroupIndex) {
                    // FIXME: don't work for Gtk
                    rbOther.Checked = false;
                }

                var childContainer = ctl as Container;
                if (childContainer != null) {
                    CheckRecursive(childContainer);
                }
            }
        }
    }
}
