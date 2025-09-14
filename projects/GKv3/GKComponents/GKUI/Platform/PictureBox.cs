/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
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
using Eto.Drawing;
using Eto.Forms;
using GKUI.Components;

namespace GKUI.Platform
{
    public class PictureBox : Drawable
    {
        private bool fBorder;
        private Image fImage;

        public bool Border
        {
            get { return fBorder; }
            set {
                fBorder = value;
                Invalidate();
            }
        }

        public Image Image
        {
            get { return fImage; }
            set {
                if (fImage != null && !fImage.IsDisposed) {
                    fImage.Dispose();
                }
                fImage = value;
                Invalidate();
            }
        }

        public PictureBox()
        {
            UIHelper.FixControlBackground(this);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                if (fImage != null && !fImage.IsDisposed) {
                    fImage.Dispose();
                }
            }
            base.Dispose(disposing);
        }

        protected override void OnPaint(PaintEventArgs e)
        {
            Graphics gfx = e.Graphics;
            gfx.Clear(BackgroundColor);

            int viewportWidth = Width;
            int viewportHeight = Height;

            int offset = 0;
            if (fBorder) {
                using (var borderPen = new Pen(Colors.Black))
                    gfx.DrawRectangle(borderPen, 0, 0, viewportWidth - 1, viewportHeight - 1);
                offset = 1;
            }

            viewportWidth -= offset * 2;
            viewportHeight -= offset * 2;

            if (fImage == null || fImage.IsDisposed) return;

            float aspectRatio = GfxHelper.ZoomToFit(fImage.Width, fImage.Height, viewportWidth, viewportHeight);

            gfx.ImageInterpolation = ImageInterpolation.High;
            gfx.PixelOffsetMode = PixelOffsetMode.Half;

            RectangleF sourRect = new RectangleF(0, 0, fImage.Width, fImage.Height);

            float newWidth = fImage.Width * aspectRatio;
            float newHeight = fImage.Height * aspectRatio;
            float destX = offset + (viewportWidth - newWidth) / 2;
            float destY = offset + (viewportHeight - newHeight) / 2;
            RectangleF destRect = new RectangleF(destX, destY, newWidth, newHeight);

            gfx.DrawImage(fImage, sourRect, destRect);
        }
    }
}
