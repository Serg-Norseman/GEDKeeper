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
    /// 
    /// </summary>
    public sealed class ScaleControl : ITreeControl
    {
        #region Private fields
        
        private static readonly Rectangle SCALE_RECT = new Rectangle(0, 0, 26, 320);
        private static readonly Rectangle THUMB_RECT = new Rectangle(0, 322, 26, 11);

        private const int SCALE_Y1 = 22;
        private const int SCALE_Y2 = 297;

        private readonly Bitmap fControlsImage;
        private readonly TreeChartBox fChart;

        private int fDCount = 11;
        private int fThumbPos = 6;
        private bool fVisible;
        private bool fThumbCaptured;
        private string fTip;
        private Rectangle fDestRect;

        #endregion
        
        #region Public properties
        
        public int Width
        {
            get { return 26; }
        }

        public int Height
        {
            get { return 320; }
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
            fDestRect = new Rectangle(cr.Right - (10 + Width), 10, Width, Height);
        }

        public void Draw(Graphics gfx)
        {
            if (gfx == null) return;

            gfx.InterpolationMode = InterpolationMode.HighQualityBicubic;
            gfx.SmoothingMode = SmoothingMode.HighQuality;
            gfx.PixelOffsetMode = PixelOffsetMode.HighQuality;
            gfx.CompositingQuality = CompositingQuality.HighQuality;
            gfx.DrawImage(fControlsImage, fDestRect, SCALE_RECT, GraphicsUnit.Pixel);

            if (fDCount == 0) return;
            gfx.DrawImage(fControlsImage, GetDRect(fThumbPos), THUMB_RECT, GraphicsUnit.Pixel);
        }

        private Rectangle GetDRect(int d)
        {
            int dH = ((SCALE_Y2 - SCALE_Y1) - THUMB_RECT.Height) / (fDCount - 1);
            int thumbY = fDestRect.Top + SCALE_Y1 + (d - 1) * dH;
            return new Rectangle(fDestRect.Left, thumbY, fDestRect.Width, THUMB_RECT.Height);
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
            
            for (int i = 1; i <= fDCount; i++) {
                Rectangle r = GetDRect(i);
                if ((r.Top <= y) && (r.Bottom > y)) {
                    fThumbPos = i;
                    fChart.Invalidate();
                    if (thumbMoved != null) thumbMoved(i);
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
