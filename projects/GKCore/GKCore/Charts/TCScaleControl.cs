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
using GKCore.Design.Graphics;

namespace GKCore.Charts
{
    /// <summary>
    /// Non-windowed scaling control.
    /// Uses a resource bitmap to draw itself on the parent's context.
    /// </summary>
    public sealed class TCScaleControl : ITreeControl
    {
        #region Private fields
        
        /* Define areas within the resource bitmap. */
        private static readonly ExtRect SCALE_RECT = ExtRect.CreateBounds(0, 0, 26, 320);
        private static readonly ExtRect THUMB_RECT = ExtRect.CreateBounds(0, 322, 26, 11);
        /* Range [`SCALE_Y1`, `SCALE_Y2`) is available range for the thumb. */
        private const int SCALE_Y1 = 22;
        private const int SCALE_Y2 = 297;
        /* Padding for this control within the owner client area. */
        private const int PADDING_X = 10;
        private const int PADDING_Y = 10;
        /* Shadow spaces after/before `SCALE_Y1` and `SCALE_Y2`. */
        private const int SHADOW_TOP = 4;
        private const int SHADOW_BOTTOM = 1;

        private readonly IImage fControlsImage;

        private int fDCount = 10;
        private int fThumbPos = 5; /* Counts from zero. */
        /* Set member `fGrowOver` or property `GrowOver` to `true` if you want
         * to have the control with height exceeds height of `SCALE_RECT`
         * (i.e. height of the original image). */
        private bool fGrowOver;

        #endregion

        #region Public properties

        public override int Width
        {
            get { return SCALE_RECT.Width; }
        }

        public override int Height
        {
            get { return SCALE_RECT.Height; }
        }

        public int DCount
        {
            get { return fDCount; }
            set { fDCount = value; }
        }

        public bool GrowOver
        {
            get { return fGrowOver; }
            set { fGrowOver = value; }
        }

        public override string Tip
        {
            get { return LangMan.LS(LSID.Scale); }
        }

        #endregion

        public TCScaleControl(ITreeChart chart) : base(chart)
        {
            fControlsImage = AppHost.GfxProvider.LoadResourceImage("Resources.chart_controls.png", ImageTarget.Chart, false);
        }

        public override void UpdateView()
        {
            ExtRect cr = fChart.GetClientRect();
            if (fGrowOver) {
                int height = cr.GetHeight() - (PADDING_Y << 1);
                fDestRect = ExtRect.CreateBounds(cr.Right - (PADDING_X + Width), cr.Top + PADDING_Y, Width, height);
            } else {
                int height = Math.Min(cr.GetHeight() - (PADDING_Y << 1), Height);
                fDestRect = ExtRect.CreateBounds(cr.Right - (PADDING_X + Width),
                                          cr.Top + Math.Max(PADDING_Y, (cr.GetHeight() - height) >> 1),
                                          Width, height);
            }
        }

        public override void UpdateState()
        {
            fThumbPos = (int)Math.Round((fChart.Scale - 0.5f) * fDCount);
        }

        public override void Draw(ChartRenderer gfx)
        {
            if (gfx == null) return;

            var drawOrigin = fChart.GetDrawOrigin();
            var drawRect = fDestRect;
            drawRect.Offset(drawOrigin.X, drawOrigin.Y);

            /* Render the top icon without scaling. */
            ExtRect sourceRect = ExtRect.CreateBounds(0, 0, Width, SCALE_Y1 + SHADOW_TOP);
            ExtRect destinationRect = ExtRect.CreateBounds(drawRect.Left, drawRect.Top,
                                                      Width, SCALE_Y1 + SHADOW_TOP);
            gfx.DrawImage(fControlsImage, destinationRect, sourceRect);
            /* Render the bottom icon without scaling. */
            sourceRect = ExtRect.CreateBounds(0, SCALE_Y2, Width, Height - (SCALE_Y2 + SHADOW_BOTTOM));
            destinationRect = ExtRect.CreateBounds(drawRect.Left,
                                            drawRect.Bottom - (Height - (SCALE_Y2 + SHADOW_BOTTOM)),
                                            Width, Height - (SCALE_Y2 + SHADOW_BOTTOM));
            gfx.DrawImage(fControlsImage, destinationRect, sourceRect);
            /* Render the vertical bar with scaling of Y's (there's still no
             * scaling for X's). Image source must ignore some shadows at the
             * top and bottom. */
            sourceRect = ExtRect.CreateBounds(0, SCALE_Y1 + SHADOW_TOP, Width, Height - (SCALE_Y2 + SHADOW_BOTTOM));
            destinationRect = ExtRect.CreateBounds(drawRect.Left, drawRect.Top + SCALE_Y1 + SHADOW_TOP, Width,
                                            drawRect.Bottom - (Height - (SCALE_Y2 + SHADOW_BOTTOM)) - (drawRect.Top + SCALE_Y1 + SHADOW_TOP));
            gfx.DrawImage(fControlsImage, destinationRect, sourceRect);
            /* Render the thumb without scaling. */
            if (fDCount > 0) {
                var thumbRect = GetDRect(fThumbPos);
                thumbRect.Offset(drawOrigin.X, drawOrigin.Y);
                gfx.DrawImage(fControlsImage, thumbRect, THUMB_RECT);
            }
        }

        private ExtRect GetDRect(int stepIndex)
        {
            int availableHeight = fDestRect.Height - (SCALE_Y1 + (Height - SCALE_Y2));
            int step = availableHeight / fDCount;
            int thumpTop = Math.Min(fDestRect.Top + SCALE_Y1 + stepIndex * step,
                                    fDestRect.Bottom - (Height - SCALE_Y2) - THUMB_RECT.Height);
            return ExtRect.CreateBounds(fDestRect.Left, thumpTop, fDestRect.Width, THUMB_RECT.Height);
        }

        public override void MouseDown(int x, int y)
        {
            fMouseCaptured = (GetDRect(fThumbPos).Contains(x, y) && !fMouseCaptured);
        }

        public override void MouseMove(int x, int y)
        {
            if (!fMouseCaptured) return;

            /* The thumb is drawn on top of a "step", therefore to take the last
             * step into account I have to check non-existent step under the
             * last one. */
            for (int i = 0; fDCount >= i; ++i) {
                ExtRect r = GetDRect(i);
                if ((r.Top <= y) && (r.Bottom > y)) {
                    if (i != fThumbPos) {
                        fThumbPos = i;
                        fChart.SetScale(0.5f + (((float)(i)) / fDCount));
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
