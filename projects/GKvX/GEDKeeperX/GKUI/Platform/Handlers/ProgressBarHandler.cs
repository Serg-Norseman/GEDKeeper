/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
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
