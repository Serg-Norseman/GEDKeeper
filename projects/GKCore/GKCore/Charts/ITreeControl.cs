/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using BSLib;

namespace GKCore.Charts
{
    /// <summary>
    /// 
    /// </summary>
    public abstract class ITreeControl : BaseObject
    {
        protected readonly ITreeChart fChart;

        protected ExtRect fDestRect;
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

        protected ITreeControl(ITreeChart chart)
        {
            fChart = chart;
        }

        public virtual bool Contains(int x, int y)
        {
            return fDestRect.Contains(x, y);
        }

        public abstract void Draw(ChartRenderer gfx);
        public abstract void MouseDown(int x, int y);
        public abstract void MouseMove(int x, int y);
        public abstract void MouseUp(int x, int y);
        public abstract void UpdateState();
        public abstract void UpdateView();
    }
}
