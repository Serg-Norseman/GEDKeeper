/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;
using BSLib;
using GKCore.Charts;
using GKCore.Design.Graphics;
using GKUI.Platform.Handlers;
using Terminal.Gui;
using Attribute = Terminal.Gui.Attribute;

namespace GKUI.Platform
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class TGGfxRenderer : ChartRenderer
    {
        private View fTargetView;

        public TGGfxRenderer()
        {
        }

        public override void SetTarget(object target)
        {
            fTargetView = target as View;
            fLines.Clear();
        }

        public override ExtSizeF GetTextSize(string text, IFont font)
        {
            if (string.IsNullOrEmpty(text) || font == null)
                return ExtSizeF.Empty;

            return new ExtSizeF(text.Length, 1);
        }

        public override void DrawString(string text, IFont font, IBrush brush, float x, float y)
        {
            if (fTargetView == null || string.IsNullOrEmpty(text))
                return;

            var driver = Application.Driver;
            var textAttr = new Attribute(Color.Black, Color.Gray);
            driver.SetAttribute(textAttr);

            int col = (int)x;
            int row = (int)y;
            for (int i = 0; i < text.Length; i++) {
                fTargetView.AddRune(col + i, row, text[i]);
            }
        }

        public override void DrawLine(IPen pen, float x1, float y1, float x2, float y2)
        {
            int ix1 = (int)x1, iy1 = (int)y1, ix2 = (int)x2, iy2 = (int)y2;

            //var penColor = (pen == null) ? null : ((PenHandler)pen).Color;
            //var color = (penColor == null) ? Color.White : ((ColorHandler)penColor).Handle;

            var driver = Application.Driver;
            var lineAttr = new Attribute(Color.White, Color.Blue);
            driver.SetAttribute(lineAttr);

            if (ix1 == ix2) {
                int len = (iy2 - iy1) + 1;
                DrawLine(ix1, iy1, len, true);
            } else {
                int len = (ix2 - ix1) + 1;
                DrawLine(ix1, iy1, len, false);
            }
        }

        public void DrawLine(int startX, int startY, int length, bool isVertical, bool optimize = true)
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
                    fTargetView.AddRune(x, y, GetLineChar(lineSegment));
                } else {
                    LineDir next;
                    bool hasNext = (i < length - 1);

                    if (isVertical) {
                        next = hasNext ? LineDir.Down : LineDir.None;
                    } else {
                        next = hasNext ? LineDir.Right : LineDir.None;
                    }

                    LineDir existing = /*LineDir.None; // */ GetCross(x, y);
                    /*if (i > 0) existing |= isVertical ? LineDir.Up : LineDir.Left;
                    if (i < length - 1) existing |= isVertical ? LineDir.Down : LineDir.Right;

                    // adjoining
                    if (isVertical) {
                        if (fLines.TryGetValue((x - 1, y), out var l)) existing |= LineDir.Left;
                        if (fLines.TryGetValue((x + 1, y), out var r)) existing |= LineDir.Right;
                    } else {
                        if (fLines.TryGetValue((x, y - 1), out var u)) existing |= LineDir.Up;
                        if (fLines.TryGetValue((x, y + 1), out var d)) existing |= LineDir.Down;
                    }*/

                    LineDir combined = existing | next;
                    if (combined == next) combined = lineSegment;

                    fLines[(x, y)] = combined;
                    fTargetView.AddRune(x, y, GetLineChar(combined));
                }

                x += dx;
                y += dy;
            }
        }

        private LineDir GetCross(int x, int y)
        {
            LineDir result = LineDir.None;
            if (fLines.ContainsKey((x - 1, y))) result |= LineDir.Left;
            if (fLines.ContainsKey((x + 1, y))) result |= LineDir.Right;
            if (fLines.ContainsKey((x, y - 1))) result |= LineDir.Up;
            if (fLines.ContainsKey((x, y + 1))) result |= LineDir.Down;
            return result;
        }

        public override void DrawRectangle(IPen pen, IColor fillColor, float x, float y, float width, float height, int cornersRadius = 0)
        {
            //var penColor = (pen == null) ? null : ((PenHandler)pen).Color;
            //var color = (penColor == null) ? Color.Black : ((ColorHandler)penColor).Handle;

            var driver = Application.Driver;
            var rectColor = new Attribute(Color.Black, Color.Gray);
            driver.SetAttribute(rectColor);

            fTargetView.DrawFrame(new Rect((int)x, (int)y, (int)width, (int)height), 0, true);
        }

        public override void FillRectangle(IBrush brush, float x, float y, float width, float height)
        {
            // *
        }

        public override IPen CreatePen(IColor color, float width, float[] dashPattern = null)
        {
            return new PenHandler(color);
        }

        public override IBrush CreateBrush(IColor color)
        {
            // *
            return null;
        }

        #region Lines

        private Dictionary<(int x, int y), LineDir> fLines = new Dictionary<(int x, int y), LineDir>();

        [Flags]
        private enum LineDir
        {
            None = 0,
            Up = 1,
            Down = 2,
            Left = 4,
            Right = 8,
            Vertical = Up | Down,
            Horizontal = Left | Right
        }

        private static char GetLineChar(LineDir dir)
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
}
