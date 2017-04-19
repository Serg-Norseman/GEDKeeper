/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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

using System.Drawing;
using GKCommon;

namespace GKCore.Charts
{
    /// <summary>
    /// 
    /// </summary>
    public abstract class ITreeControl : BaseObject
    {
        protected readonly ITreeChartBox fChart;

        protected Rectangle fDestRect;
        protected bool fMouseCaptured;
        protected bool fVisible;

        public bool MouseCaptured
        {
            get { return fMouseCaptured; }
        }

        public bool Visible
        {
            get {
                return fVisible;
            }
            set {
                if (fVisible != value) {
                    fVisible = value;
                    fChart.Invalidate();
                }
            }
        }

        public abstract string Tip { get; }
        public abstract int Height { get; }
        public abstract int Width { get; }

        public abstract void UpdateState();
        public abstract void UpdateView();
        public abstract void Draw(Graphics gfx);
        public abstract void MouseDown(int x, int y);
        public abstract void MouseMove(int x, int y);
        public abstract void MouseUp(int x, int y);

        protected ITreeControl(ITreeChartBox chart)
        {
            fChart = chart;
        }

        public virtual bool Contains(int x, int y)
        {
            return fDestRect.Contains(x, y);
        }
    }
}
