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
using System.Drawing.Drawing2D;
using GKCore;

namespace GKUI.Charts
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class GenerationsControl : ITreeControl
    {
        /* Padding for this control within the owner client area. */
        private const int PADDING_X = 10;
        private const int PADDING_Y = 10;

        private const int RADIUS = 150;
        private const int SEGMENT_ANGLE = 45;

        private int fDCount = 10;
        private int fThumbPos = 9; /* Counts from zero to 9. */

        private static readonly Color BLANK_COLOR = Color.FromArgb(191, 191, 191);
        private static readonly Color SELECT_COLOR = Color.FromArgb(128, 128, 128);

        public override string Tip
        {
            get {
                string gen = (fThumbPos < 9) ? fThumbPos.ToString() : LangMan.LS(LSID.LSID_Unlimited);
                return "Generations: " + gen;
            }
        }

        public override int Height
        {
            get { return fDestRect.Height; }
        }

        public override int Width
        {
            get { return fDestRect.Width; }
        }

        public GenerationsControl(TreeChartBox chart) : base(chart)
        {
        }

        public override void UpdateState()
        {
            fThumbPos = (fChart.DepthLimit >= -1) ? fChart.DepthLimit - 1 : 9;
        }

        private float GetChordLength(float radius, float radianAngle)
        {
            return (float)(2.0f * radius * Math.Sin(radianAngle / 2.0f));
        }

        public override void UpdateView()
        {
            Rectangle cr = fChart.ClientRectangle;

            int height = Math.Min(cr.Height - (PADDING_Y << 1), RADIUS);
            int width = (int)GetChordLength(height, (float)(SEGMENT_ANGLE * (Math.PI / 180.0f)));

            fDestRect = new Rectangle(cr.Left + PADDING_X,
                                      Math.Max(PADDING_Y, (cr.Height - height) >> 1),
                                      width, height);
        }

        public override void Draw(Graphics gfx)
        {
            if (gfx == null) return;

            gfx.InterpolationMode = InterpolationMode.HighQualityBicubic;
            gfx.SmoothingMode = SmoothingMode.HighQuality;
            gfx.PixelOffsetMode = PixelOffsetMode.HighQuality;
            gfx.CompositingQuality = CompositingQuality.HighQuality;

            int intervalHeight = 2;
            int segmentHeight = (int)((fDestRect.Height - (9 /* intervals */ * intervalHeight)) / 10);
            int ctX = fDestRect.Left + (fDestRect.Width / 2);
            int ctY = fDestRect.Top;
            int inRad = 0;
            float ang1 = (180 - SEGMENT_ANGLE) / 2;
            float ang2 = ang1 + SEGMENT_ANGLE;

            for (int i = 0; i <= 9; i++) {
                int extRad = inRad + segmentHeight;

                Color color = (i <= fThumbPos) ? SELECT_COLOR : BLANK_COLOR;

                using (var brush = new SolidBrush(color)) {
                    using (var path = new GraphicsPath()) {
                        CustomChart.CreateCircleSegment(path, ctX, ctY, inRad, extRad, SEGMENT_ANGLE, ang1, ang2);
                        gfx.FillPath(brush, path);
                    }
                }

                inRad = extRad + intervalHeight;
            }
        }

        public override void MouseDown(int x, int y)
        {
            fMouseCaptured = (GetDRect(fThumbPos).Contains(x, y) && !fMouseCaptured);
        }

        private Rectangle GetDRect(int stepIndex)
        {
            int step = fDestRect.Height / fDCount;
            int thumbTop = fDestRect.Top + stepIndex * step;
            return new Rectangle(fDestRect.Left, thumbTop, fDestRect.Width, step);
        }

        public override void MouseMove(int x, int y)
        {
            if (!fMouseCaptured) return;

            /* The thumb is drawn on top of a "step", therefore to take the last
             * step into account I have to check non-existent step under the
             * last one. */
            for (int i = 0; fDCount >= i; ++i) {
                Rectangle r = GetDRect(i);
                if ((r.Top <= y) && (r.Bottom > y)) {
                    if (i != fThumbPos) {
                        fThumbPos = i;

                        fChart.Invalidate();

                        int depthLimit = (fThumbPos < 9) ? fThumbPos + 1 : -1;
                        fChart.DepthLimit = depthLimit;
                        // bad! very bad! very bad architecture!!!
                        fChart.GenChart(fChart.Root.Rec, fChart.Kind, true);
                    }
                    break;
                }
            }
        }

        public override void MouseUp(int x, int y)
        {
            fMouseCaptured = false;
        }
    }
}
