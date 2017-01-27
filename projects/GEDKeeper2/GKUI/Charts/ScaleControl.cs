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
using System.Drawing.Drawing2D;

namespace GKUI.Charts
{
    /// <summary>
    /// Non-windowed scaling control.
    /// Uses a resource bitmap to draw itself on the parent's context.
    /// </summary>
    public sealed class ScaleControl : ITreeControl
    {
        #region Private fields
        
        /* Define areas within the resource bitmap. */
        private static readonly Rectangle SCALE_RECT = new Rectangle(0, 0, 26, 320);
        private static readonly Rectangle THUMB_RECT = new Rectangle(0, 322, 26, 11);
        /* Range [`SCALE_Y1`, `SCALE_Y2`) is available range for the thumb. */
        private const int SCALE_Y1 = 22;
        private const int SCALE_Y2 = 297;
        /* Padding for this control within the owner client area. */
        private const int PADDING_X = 10;
        private const int PADDING_Y = 10;
        /* Shadow spaces after/before `SCALE_Y1` and `SCALE_Y2`. */
        private const int SHADOW_TOP = 4;
        private const int SHADOW_BOTTOM = 1;

        private readonly Bitmap fControlsImage;
        private readonly TreeChartBox fChart;

        private int fDCount = 10;
        private int fThumbPos = 5; /* Counts from zero. */
        private bool fVisible;
        private bool fThumbCaptured;
        /* Set member `fGrowOver` or property `GrowOver` to `true` if you want
         * to have the control with height exceeds height of `SCALE_RECT`
         * (i.e. height of the original image). */
        private bool fGrowOver = false;
        private string fTip;
        private Rectangle fDestRect;

        #endregion
        
        #region Public properties
        
        public static int Width
        {
            get { return SCALE_RECT.Width; }
        }

        public static int Height
        {
            get { return SCALE_RECT.Height; }
        }

        public int DCount
        {
            get { return fDCount; }
            set { fDCount = value; }
        }

        public bool ThumbCaptured
        {
            get { return fThumbCaptured; }
        }

        public bool GrowOver
        {
            get { return fGrowOver; }
            set { fGrowOver = value; }
        }

        public int ThumbPos
        {
            get { return fThumbPos; }
            set { fThumbPos = value; }
        }
        
        public string Tip
        {
            get { return fTip; }
            set { fTip = value; }
        }
        
        public bool Visible
        {
            get { return fVisible; }
            set {
                fVisible = value;
                fChart.Invalidate();
            }
        }

        #endregion
        
        public ScaleControl(TreeChartBox chart)
        {
            fChart = chart;
            fControlsImage = GKResources.iChartControls;
        }

        public void Dispose()
        {
            // dummy
        }

        public void Update()
        {
            Rectangle cr = fChart.ClientRectangle;
            if (fGrowOver) {
                int height = cr.Height - (PADDING_Y << 1);
                fDestRect = new Rectangle(cr.Right - (PADDING_X + Width), PADDING_Y, Width, height);
            } else {
                int height = System.Math.Min(cr.Height - (PADDING_Y << 1), Height);
                fDestRect = new Rectangle(cr.Right - (PADDING_X + Width),
                                          System.Math.Max(PADDING_Y, (cr.Height - height) >> 1),
                                          Width, height);
            }
        }

        public void Draw(Graphics gfx)
        {
            if (gfx == null) return;

            gfx.InterpolationMode = InterpolationMode.HighQualityBicubic;
            gfx.SmoothingMode = SmoothingMode.HighQuality;
            gfx.PixelOffsetMode = PixelOffsetMode.HighQuality;
            gfx.CompositingQuality = CompositingQuality.HighQuality;
            /* Render the top icon without scaling. */
            Rectangle sourceRect = new Rectangle(0, 0, Width, SCALE_Y1 + SHADOW_TOP);
            Rectangle destinationRect = new Rectangle(fDestRect.Left, fDestRect.Top,
                                                      Width, SCALE_Y1 + SHADOW_TOP);
            gfx.DrawImage(fControlsImage, destinationRect, sourceRect,
                          GraphicsUnit.Pixel);
            /* Render the bottom icon without scaling. */
            sourceRect = new Rectangle(0, SCALE_Y2, Width, Height - (SCALE_Y2 + SHADOW_BOTTOM));
            destinationRect = new Rectangle(fDestRect.Left,
                                            fDestRect.Bottom - (Height - (SCALE_Y2 + SHADOW_BOTTOM)),
                                            Width, Height - (SCALE_Y2 + SHADOW_BOTTOM));
            gfx.DrawImage(fControlsImage, destinationRect, sourceRect, GraphicsUnit.Pixel);
            /* Render the vertical bar with scaling of Y's (there's still no
             * scaling for X's). Image source must ignore some shadows at the
             * top and bottom. */
            sourceRect = new Rectangle(0, SCALE_Y1 + SHADOW_TOP, Width, Height - (SCALE_Y2 + SHADOW_BOTTOM));
            destinationRect = new Rectangle(fDestRect.Left, fDestRect.Top + SCALE_Y1 + SHADOW_TOP, Width,
                fDestRect.Bottom - (Height - (SCALE_Y2 + SHADOW_BOTTOM)) - (fDestRect.Top + SCALE_Y1 + SHADOW_TOP));
            gfx.DrawImage(fControlsImage, destinationRect, sourceRect, GraphicsUnit.Pixel);
            if (0 < fDCount)
            {
                gfx.DrawImage(fControlsImage, GetDRect(fThumbPos), THUMB_RECT, GraphicsUnit.Pixel);
            }
        }

        private Rectangle GetDRect(int stepIndex)
        {
            int availableHeight = fDestRect.Height - (SCALE_Y1 + (Height - SCALE_Y2));
            int step = availableHeight / fDCount;
            int thumpTop = System.Math.Min(fDestRect.Top + SCALE_Y1 + stepIndex * step,
                                           fDestRect.Bottom - (Height - SCALE_Y2) - THUMB_RECT.Height);
            return new Rectangle(fDestRect.Left, thumpTop, fDestRect.Width, THUMB_RECT.Height);
        }

        public bool Contains(int x, int y)
        {
            return fDestRect.Contains(x, y);
        }

        public void MouseDown(int x, int y)
        {
            fThumbCaptured = (GetDRect(fThumbPos).Contains(x, y) && !fThumbCaptured);
        }

        public void MouseMove(int x, int y, ThumbMoved thumbMoved)
        {
            if (!fThumbCaptured) return;
            /* The thumb is drawn on top of a "step", therefore to take the last
             * step into account I have to check non-existent step under the
             * last one. */
            for (int i = 0; fDCount >= i; ++i) {
                Rectangle r = GetDRect(i);
                if ((r.Top <= y) && (r.Bottom > y)) {
                    if (i != fThumbPos) {
                        fThumbPos = i;
                        fChart.Invalidate();
                        if (thumbMoved != null) thumbMoved(i);
                    }
                    break;
                }
            }
        }

        public void MouseUp(int x, int y)
        {
            fThumbCaptured = false;
        }
    }
}
