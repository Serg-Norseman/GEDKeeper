/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using BSLib;
using GKCore.Design.Graphics;

namespace GKCore.Charts
{
    public abstract class TextRenderer : ChartRenderer
    {
        public TextRenderer()
        {
        }

        public override void SetTarget(object target)
        {
            fLines.Clear();
        }

        public override ExtSizeF GetTextSize(string text, IFont font)
        {
            if (string.IsNullOrEmpty(text) || font == null)
                return ExtSizeF.Empty;

            return new ExtSizeF(text.Length, 1);
        }

        protected abstract void DrawChar(int x, int y, char chr);

        public override void DrawString(string text, IFont font, IBrush brush, float x, float y)
        {
            if (string.IsNullOrEmpty(text))
                return;

            int col = (int)x;
            int row = (int)y;
            for (int i = 0; i < text.Length; i++) {
                DrawChar(col + i, row, text[i]);
            }
        }

        private void DrawLine(int startX, int startY, int length, bool isVertical, bool optimize = true)
        {
            LineDir lineSegment;
            int dx, dy;
            if (isVertical) {
                lineSegment = LineDir.Vertical;
                dx = 0;
                dy = 1;
            } else {
                lineSegment = LineDir.Horizontal;
                dx = 1;
                dy = 0;
            }

            int x = startX, y = startY;
            for (int i = 0; i < length; i++) {
                if (!optimize) {
                    DrawChar(x, y, GetLineChar(lineSegment));
                } else {
                    LineDir next;
                    bool hasNext = (i < length - 1);
                    if (isVertical) {
                        next = hasNext ? LineDir.Down : LineDir.None;
                    } else {
                        next = hasNext ? LineDir.Right : LineDir.None;
                    }

                    LineDir existing = GetCross(x, y);
                    LineDir combined = existing | next;
                    if (combined == next) combined = lineSegment;

                    fLines[(x, y)] = combined;
                    DrawChar(x, y, GetLineChar(combined));
                }

                x += dx;
                y += dy;
            }
        }

        public override void DrawLine(IPen pen, float x1, float y1, float x2, float y2)
        {
            int ix1 = (int)x1, iy1 = (int)y1, ix2 = (int)x2, iy2 = (int)y2;

            if (ix1 == ix2) {
                int len = (iy2 - iy1) + 1;
                DrawLine(ix1, iy1, len, true);
            } else {
                int len = (ix2 - ix1) + 1;
                DrawLine(ix1, iy1, len, false);
            }
        }

        protected void DrawRect(int rX, int rY, int rWidth, int rHeight, bool fill, int borderStyle)
        {
            int fwidth = rWidth - 2;
            int fheight = rHeight - 2;
            int fleft = rX;
            int fright = fleft + fwidth + 1;
            int ftop = rY;
            int fbottom = ftop + fheight + 1;

            const char clr = ' ';
            char hLine = clr, vLine = clr, urCorner = clr, ulCorner = clr, llCorner = clr, lrCorner = clr;
            switch (borderStyle) {
                case 1:
                    hLine = '─';
                    vLine = '│';
                    urCorner = '┐';
                    ulCorner = '┌';
                    llCorner = '└';
                    lrCorner = '┘';
                    break;

                case 2:
                    hLine = '═';
                    vLine = '║';
                    urCorner = '╗';
                    ulCorner = '╔';
                    llCorner = '╚';
                    lrCorner = '╝';
                    break;

                case 3:
                    hLine = '─';
                    vLine = '│';
                    urCorner = '╮';
                    ulCorner = '╭';
                    llCorner = '╰';
                    lrCorner = '╯';
                    break;
            }

            DrawChar(fleft, ftop, ulCorner);
            for (int c = fleft + 1; c < fleft + 1 + fwidth; c++) DrawChar(c, ftop, hLine);
            DrawChar(fright, ftop, urCorner);

            for (int r = ftop + 1; r < fbottom; r++) {
                DrawChar(fleft, r, vLine);
                if (fill) {
                    for (int x = fleft + 1; x < fright; x++) DrawChar(x, r, clr);
                }
                DrawChar(fright, r, vLine);
            }

            DrawChar(fleft, fbottom, llCorner);
            for (int c = fleft + 1; c < fright; c++) DrawChar(c, fbottom, hLine);
            DrawChar(fright, fbottom, lrCorner);
        }

        public override void DrawRectangle(IPen pen, IColor fillColor, float x, float y, float width, float height, int cornersRadius = 0)
        {
            var borderStyle = (pen == null) ? 0 : (int)pen.Width;
            DrawRect((int)x, (int)y, (int)width, (int)height, true, borderStyle);
        }

        public override void FillRectangle(IBrush brush, float x, float y, float width, float height)
        {
            // *
        }

        #region Lines

        protected LineDir GetCross(int x, int y)
        {
            LineDir result = LineDir.None;
            if (fLines.ContainsKey((x - 1, y))) result |= LineDir.Left;
            if (fLines.ContainsKey((x + 1, y))) result |= LineDir.Right;
            if (fLines.ContainsKey((x, y - 1))) result |= LineDir.Up;
            if (fLines.ContainsKey((x, y + 1))) result |= LineDir.Down;
            return result;
        }

        protected Dictionary<(int x, int y), LineDir> fLines = new Dictionary<(int x, int y), LineDir>();

        [Flags]
        protected enum LineDir
        {
            None = 0,
            Up = 1,
            Down = 2,
            Left = 4,
            Right = 8,
            Vertical = Up | Down,
            Horizontal = Left | Right
        }

        protected static char GetLineChar(LineDir dir)
        {
            return dir switch {
                LineDir.Vertical => '│',
                LineDir.Horizontal => '─',
                (LineDir.Down | LineDir.Right) => '┌',
                (LineDir.Down | LineDir.Left) => '┐',
                (LineDir.Up | LineDir.Right) => '└',
                (LineDir.Up | LineDir.Left) => '┘',
                (LineDir.Vertical | LineDir.Right) => '├',
                (LineDir.Vertical | LineDir.Left) => '┤',
                (LineDir.Horizontal | LineDir.Down) => '┬',
                (LineDir.Horizontal | LineDir.Up) => '┴',
                (LineDir.Vertical | LineDir.Horizontal) => '┼',
                _ => ' '
            };
        }

        #endregion
    }


    public sealed class TXTRenderer : TextRenderer
    {
        private readonly string fFileName;
        private int fHeight, fWidth;
        private StreamWriter fWriter;
        private char[] fImage;

        public TXTRenderer(string fileName)
        {
            fFileName = fileName;
        }

        public void SetViewBox(int width, int height)
        {
            fWidth = width;
            fHeight = height;
        }

        public override void BeginDrawing()
        {
            fWriter = new StreamWriter(new FileStream(fFileName, FileMode.Create, FileAccess.Write), Encoding.UTF8);
            fImage = new char[fHeight * fWidth];
            Array.Fill(fImage, ' ');
        }

        public override void EndDrawing()
        {
            if (fWriter != null) {
                int start = 0;
                for (int i = 0; i < fHeight; i++) {
                    var line = new string(fImage, start, fWidth);
                    fWriter.WriteLine(line);
                    start += fWidth;
                }
                fWriter.Flush();
                fWriter.Close();
                fWriter = null;
            }
        }

        protected override void DrawChar(int x, int y, char chr)
        {
            if (chr == '\0') chr = ' ';

            int idx = y * fWidth + x;
            fImage[idx] = chr;
        }

        public override IPen CreatePen(IColor color, float width, float[] dashPattern = null)
        {
            return new FakePen(color, width);
        }

        public override IBrush CreateBrush(IColor color)
        {
            return new FakeBrush(color);
        }
    }
}
