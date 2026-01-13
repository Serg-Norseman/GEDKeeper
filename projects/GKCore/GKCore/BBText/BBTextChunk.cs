/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
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
