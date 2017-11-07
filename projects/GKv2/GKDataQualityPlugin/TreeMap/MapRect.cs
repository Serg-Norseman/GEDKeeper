/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2017 by Sergey V. Zhdanovskih.
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

namespace GKCommon.TreeMap
{
    public sealed class MapRect
    {
        public double X, Y, W, H;

        public MapRect()
            : this(0, 0, 0, 0)
        {
        }

        public MapRect(double x, double y, double w, double h)
        {
            SetRect(x, y, w, h);
        }

        public double GetAspectRatio()
        {
            return Math.Max(W / H, H / W);
        }

        public void SetRect(double x, double y, double w, double h)
        {
            X = x;
            Y = y;
            W = w;
            H = h;
        }

        public bool Contains(int x, int y)
        {
            return x >= X && y >= Y && x < X + W && y < Y + H;
        }
    }
}
