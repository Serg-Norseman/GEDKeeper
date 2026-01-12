/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Windows.Forms;
using GKCore.Design.Controls;

namespace GKUI.Platform.Handlers
{
    public sealed class DateTimeBoxHandler : BaseControlHandler<DateTimePicker, DateTimeBoxHandler>, IDateTimeBox
    {
        public bool Checked
        {
            get {
                return Control.Checked;
            }
            set {
                Control.Checked = value;
            }
        }

        public DateTime Value
        {
            get {
                return Control.Value;
            }
            set {
                Control.Value = value;
            }
        }

        public DateTimeBoxHandler(DateTimePicker control) : base(control)
        {
        }
    }
}
