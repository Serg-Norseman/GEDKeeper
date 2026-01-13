/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;

namespace GKCore.Charts
{
    public sealed class TreeControlsList<T> : List<T>, IDisposable where T : ITreeControl
    {
        public void Draw(ChartRenderer gfx)
        {
            if (gfx == null) return;

            for (int i = 0; i < Count; i++) {
                T ctl = this[i];
                if (ctl.Visible) ctl.Draw(gfx);
            }
        }

        public void UpdateState()
        {
            for (int i = 0; i < Count; i++) {
                this[i].UpdateState();
            }
        }

        public void UpdateView()
        {
            for (int i = 0; i < Count; i++) {
                this[i].UpdateView();
            }
        }

        public ITreeControl Contains(int x, int y)
        {
            for (int i = 0; i < Count; i++) {
                ITreeControl ctl = this[i];
                if (ctl.Contains(x, y)) return ctl;
            }

            return null;
        }

        public void MouseDown(int x, int y)
        {
            for (int i = 0; i < Count; i++) {
                T ctl = this[i];
                if (ctl.Visible) ctl.MouseDown(x, y);
            }
        }

        public void MouseMove(int x, int y, bool defaultChartMode)
        {
            for (int i = 0; i < Count; i++) {
                T ctl = this[i];
                if (ctl.Visible) ctl.MouseMove(x, y);
            }
        }

        public void MouseUp(int x, int y)
        {
            for (int i = 0; i < Count; i++) {
                T ctl = this[i];
                if (ctl.Visible) ctl.MouseUp(x, y);
            }
        }

        public void Dispose()
        {
            for (int i = 0; i < Count; i++) {
                this[i].Dispose();
            }
            Clear();
        }
    }
}
