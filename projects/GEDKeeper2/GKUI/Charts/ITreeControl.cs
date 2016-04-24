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

namespace GKUI.Charts
{
    /// <summary>
    /// 
    /// </summary>
    public interface ITreeControl : IDisposable
    {
        void Update();
        void Draw(Graphics gfx);
        bool Contains(int x, int y);
        void MouseDown(int x, int y);
        void MouseMove(int x, int y, ThumbMoved thumbMoved);
        void MouseUp(int x, int y);
    }
}
