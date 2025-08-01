/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2017-2025 by Sergey V. Zhdanovskih.
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

using BSLib;
using GKCore.Design;
using GKCore.Design.Graphics;

namespace GKCore.BBText
{
    public sealed class BBTextChunk
    {
        public int Line;
        public string Text;
        public int Width;
        public IColor Color;
        public float Size;
        public GKFontStyle Style;

        public string URL;
        public ExtRect LinkRect;

        private BBTextChunk()
        {
        }

        public BBTextChunk(int tokenLine, float fontSize, GKFontStyle fontStyle, IColor color)
        {
            Line = tokenLine - 1;
            Size = fontSize;
            Style = fontStyle;
            Color = color;

            Text = string.Empty;
            URL = string.Empty;
        }

        public bool HasCoord(int x, int y)
        {
            return LinkRect.Contains(x, y);
        }

        public BBTextChunk Clone()
        {
            var result = new BBTextChunk();

            result.Line = Line;
            result.Text = Text;
            result.Width = Width;
            result.Color = Color;
            result.Size = Size;
            result.Style = Style;
            result.URL = URL;

            return result;
        }

        public override string ToString()
        {
            return string.Format("[BBTextChunk Line={0}, Text={1}, Size={2}]", Line, Text, Size);
        }
    }
}
