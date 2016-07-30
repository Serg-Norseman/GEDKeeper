/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
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
using System.Drawing;
using System.Windows.Forms;

using GKCommon.GEDCOM;

namespace GKUI.Charts
{
    public delegate void ARootChangedEventHandler(object sender, GEDCOMIndividualRecord person);

    /// <summary>
    /// 
    /// </summary>
    public abstract class CircleChart : CustomChart
    {
        protected static readonly object EventRootChanged;

        public event ARootChangedEventHandler RootChanged
        {
            add { base.Events.AddHandler(CircleChart.EventRootChanged, value); }
            remove { base.Events.RemoveHandler(CircleChart.EventRootChanged, value); }
        }

        static CircleChart()
        {
            CircleChart.EventRootChanged = new object();
        }

        public CircleChart()
        {
        }

        public abstract void Changed();

        protected abstract void InternalDraw(Graphics gfx);

        protected void DoRootChanged(GEDCOMIndividualRecord person)
        {
            ARootChangedEventHandler eventHandler = (ARootChangedEventHandler)base.Events[AncestorsCircle.EventRootChanged];
            if (eventHandler == null) return;

            eventHandler(this, person);
        }

        protected override bool IsInputKey(Keys keyData)
        {
            switch (keyData) {
                case Keys.Add:
                case Keys.Subtract:
                case Keys.Left:
                case Keys.Right:
                case Keys.Back:
                    return true;
            }

            return base.IsInputKey(keyData);
        }

        protected override void OnPaint(PaintEventArgs e)
        {
            this.InternalDraw(e.Graphics);
        }

        protected override void OnResize(EventArgs e)
        {
            base.OnResize(e);
            this.Changed();
        }
    }
}
