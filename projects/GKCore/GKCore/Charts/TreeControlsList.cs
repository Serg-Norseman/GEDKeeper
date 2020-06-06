/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2020 by Sergey V. Zhdanovskih.
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
