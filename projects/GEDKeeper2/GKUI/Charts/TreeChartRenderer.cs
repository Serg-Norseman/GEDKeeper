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

using System.Drawing;
using GKCommon;

namespace GKUI.Charts
{
    /// <summary>
    /// 
    /// </summary>
    public abstract class TreeChartRenderer
    {
        protected TreeChartRenderer()
        {
        }

        public abstract void SetTarget(object target);

        public abstract void DrawImage(Image image, int x, int y);
        public abstract void DrawImage(Image image, ExtRect rect);

        public abstract int GetTextHeight(string text, Font font);
        public abstract int GetTextWidth(string text, Font font);

        public abstract void DrawString(string s, Font font, Brush brush, int x, int y);

        public abstract void DrawLine(Pen pen, int x1, int y1, int x2, int y2);

        public abstract void DrawRectangle(Pen pen, Color fillColor,
                                           int x, int y, int width, int height);
        public abstract void DrawRoundedRectangle(Pen pen, Color fillColor,
                                                  int x, int y, int width, int height, int radius);
    }
}
