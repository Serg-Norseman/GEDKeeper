/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore.Design.Controls;
using GKUI.Components;

namespace GKUI.Platform
{
    public sealed class NumericBoxHandler : BaseControlHandler<NumericStepper, NumericBoxHandler>, INumericBox
    {
        public NumericBoxHandler(NumericStepper control) : base(control)
        {
        }

        public bool ReadOnly
        {
            get { return false; }
            set { }
        }

        public string Text
        {
            get { return Value.ToString(); }
            set { } // TODO
        }

        public double Value
        {
            get { return Control.Value; }
            set { Control.Value = (int)value; }
        }
    }
}
