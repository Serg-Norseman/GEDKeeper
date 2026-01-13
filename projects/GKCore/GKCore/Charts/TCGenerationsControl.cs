/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using BSLib;
using GKCore.Design.Graphics;
using GKCore.Locales;

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
                string gen = (depthLimit == -1) ? LangMan.LS(LSID.Unlimited) : depthLimit.ToString();
                return LangMan.LS(LSID.Generations) + ": " + gen;
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
            fBlankColor = AppHost.GfxProvider.CreateColor(GKColors.Silver);
            fSelectColor = AppHost.GfxProvider.CreateColor(GKColors.Gray);
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

                using (var brush = gfx.CreateBrush(color)) {
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
