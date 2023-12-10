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

namespace GKUI.Platform
{
    public sealed class ProgressBarHandler : BaseControlHandler<ProgressBar, ProgressBarHandler>, IProgressBar
    {
        private int fMin;
        private int fMax;
        private int fValue;

        public ProgressBarHandler(ProgressBar control) : base(control)
        {
        }

        public int Minimum
        {
            get { return fMin; }
            set { fMin = value; }
        }

        public int Maximum
        {
            get { return fMax; }
            set { fMax = value; }
        }

        public int Value
        {
            get { return fValue; }
            set {
                fValue = value;
                UpdateValue();
            }
        }

        public void Increment(int value)
        {
            fValue += value;
            UpdateValue();
        }

        private void UpdateValue()
        {
            int divider = fMax - fMin;
            Control.Progress = (divider == 0) ? 0 : fValue / divider;
        }
    }
}
