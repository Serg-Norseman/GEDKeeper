/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
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
using BSLib;
using GKCore.Design;
using GKCore.Design.Graphics;

namespace GKCore.Charts
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class TCGenerationsControl : ITreeControl
    {
        /* Padding for this control within the owner client area. */
        private const int PADDING_X = 10;
        private const int PADDING_Y = 10;

        private const int RADIUS = 150;
        private const int SEGMENT_ANGLE = 45;
        private const int D_COUNT = 10;

        private int fThumbPos = 9; /* Counts from zero to 9; where 9 is infinite. */

        private readonly TreeChartKind fControlMode;
        private readonly IColor fBlankColor;
        private readonly IColor fSelectColor;

        public override string Tip
        {
            get {
                int depthLimit = GetDepthLimit();
                string gen = (depthLimit == -1) ? LangMan.LS(LSID.LSID_Unlimited) : depthLimit.ToString();
                return LangMan.LS(LSID.LSID_Generations) + ": " + gen;
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

        public TCGenerationsControl(ITreeChart chart, TreeChartKind controlMode) : base(chart)
        {
            fControlMode = controlMode;
            fBlankColor = AppHost.GfxProvider.CreateColor(BSDConsts.Colors.Silver);
            fSelectColor = AppHost.GfxProvider.CreateColor(BSDConsts.Colors.Gray);
        }

        public override void UpdateState()
        {
            int depth;
            if (!fChart.Options.SeparateDepth || fControlMode == TreeChartKind.ckAncestors) {
                depth = fChart.DepthLimitAncestors;
            } else {
                depth = fChart.DepthLimitDescendants;
            }

            fThumbPos = (depth >= -1) ? depth - 1 : 9;
        }

        private int GetDepthLimit()
        {
            return (fThumbPos < 9) ? fThumbPos + 1 : -1;
        }

        private void SetDepthLimit()
        {
            int depth = GetDepthLimit();
            if (!fChart.Options.SeparateDepth || fControlMode == TreeChartKind.ckAncestors) {
                fChart.DepthLimitAncestors = depth;
            } else {
                fChart.DepthLimitDescendants = depth;
            }

            fChart.GenChart(true);
        }

        private static float GetChordLength(float radius, float radianAngle)
        {
            return (float)(2.0f * radius * Math.Sin(radianAngle / 2.0f));
        }

        public override void UpdateView()
        {
            ExtRect cr = fChart.GetClientRect();

            int height = Math.Min(cr.GetHeight() - (PADDING_Y << 1), RADIUS);
            int width = (int)GetChordLength(height, (float)(SEGMENT_ANGLE * (Math.PI / 180.0f)));

            fDestRect = ExtRect.CreateBounds(cr.Left + PADDING_X,
                                      cr.Top + Math.Max(PADDING_Y, (cr.GetHeight() - height) >> 1),
                                      width, height);
        }

        public override void Draw(ChartRenderer gfx)
        {
            if (gfx == null) return;

            var drawOrigin = fChart.GetDrawOrigin();
            var drawRect = fDestRect;
            drawRect.Offset(drawOrigin.X, drawOrigin.Y);

            int intervalHeight = 2;
            int segmentHeight = ((drawRect.Height - (9 /* intervals */ * intervalHeight)) / 10);
            int ctX = drawRect.Left + (drawRect.Width / 2);
            int ctY = drawRect.Top;
            int inRad = 0;
            float ang1 = (180 - SEGMENT_ANGLE) / 2.0f;
            float ang2 = ang1 + SEGMENT_ANGLE;

            for (int i = 0; i <= 9; i++) {
                int extRad = inRad + segmentHeight;

                IColor color = (i <= fThumbPos) ? fSelectColor : fBlankColor;

                using (var brush = AppHost.GfxProvider.CreateBrush(color)) {
                    using (var path = gfx.CreateCircleSegmentPath(ctX, ctY, inRad, extRad, SEGMENT_ANGLE, ang1, ang2)) {
                        gfx.DrawPath(null, brush, path);
                    }
                }

                inRad = extRad + intervalHeight;
            }
        }

        public override void MouseDown(int x, int y)
        {
            fMouseCaptured = (GetDRect(fThumbPos).Contains(x, y) && !fMouseCaptured);
        }

        private ExtRect GetDRect(int stepIndex)
        {
            int step = fDestRect.Height / D_COUNT;
            int thumbTop = fDestRect.Top + stepIndex * step;
            return ExtRect.CreateBounds(fDestRect.Left, thumbTop, fDestRect.Width, step);
        }

        public override void MouseMove(int x, int y)
        {
            if (!fMouseCaptured) return;

            for (int i = 0; D_COUNT >= i; ++i) {
                ExtRect r = GetDRect(i);
                if ((r.Top <= y) && (r.Bottom > y)) {
                    if (i != fThumbPos) {
                        fThumbPos = i;
                        SetDepthLimit();
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
