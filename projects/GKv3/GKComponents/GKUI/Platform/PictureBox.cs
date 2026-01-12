/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
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
