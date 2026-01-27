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
    public sealed class ProgressBarHandler : BaseControlHandler<ProgressBar, ProgressBarHandler>, IProgressBar
    {
        private int fMaximum;
        private int fMinimum;
        private int fValue;

        public ProgressBarHandler(ProgressBar control) : base(control)
        {
        }

        public int Minimum
        {
            get { return fMinimum; }
            set { fMinimum = value; }
        }

        public int Maximum
        {
            get { return fMaximum; }
            set { fMaximum = value; }
        }

        public int Value
        {
            get { return fValue; }
            set {
                fValue = value;
                var size = fMaximum - fMinimum;
                Control.Fraction = (size == 0) ? 0 : fValue / size;
            }
        }

        public void Increment(int value)
        {
            Value += value;
        }
    }
}
