/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using BSLib;
using GKCore.Charts;
using GKCore.Design.Graphics;
using Terminal.Gui;

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
        }

        public override void DrawImage(IImage image, float x, float y, float width, float height, string imName)
        {
            // implementation is impossible
        }

        public override void DrawImage(IImage image, ExtRect destinationRect, ExtRect sourceRect)
        {
            // implementation is impossible
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
                //driver.Move(col + i, row);
                fTargetView.AddRune(col + i, row, text[i]);
            }
        }

        public override void DrawLine(IPen pen, float x1, float y1, float x2, float y2)
        {
            int ix1 = (int)x1, iy1 = (int)y1, ix2 = (int)x2, iy2 = (int)y2;

            var driver = Application.Driver;
            var lineAttr = new Attribute(Color.White, Color.Blue);
            driver.SetAttribute(lineAttr);

            if (ix1 == ix2) {
                for (int y = iy1; y <= iy2; y++) {
                    //driver.Move(ix1, y);
                    //driver.AddRune(driver.VLine);
                    fTargetView.AddRune(ix1, y, driver.VLine);
                }
            } else {
                for (int x = ix1; x <= ix2; x++) {
                    //driver.Move(x, iy1);
                    //driver.AddRune(driver.HLine);
                    fTargetView.AddRune(x, iy1, driver.HLine);
                }
            }
        }

        public override void DrawRectangle(IPen pen, IColor fillColor, float x, float y, float width, float height, int cornersRadius = 0)
        {
            var driver = Application.Driver;
            var rectColor = new Attribute(Color.Blue, Color.Gray);
            driver.SetAttribute(rectColor);
            //driver.DrawWindowFrame(new Rect((int)x, (int)y, (int)width, (int)height), 1, 1, 1, 1, true, true);
            fTargetView.DrawFrame(new Rect((int)x, (int)y, (int)width, (int)height), 0, true);

            /*int ix1 = (int)x, iy1 = (int)y, ix2 = (int)x + (int)width - 1, iy2 = (int)y + (int)height - 1;

            for (int iy = iy1 + 1; iy <= iy2 - 1; iy++) {
                driver.Move(ix1, iy); driver.AddRune(driver.VLine);
                driver.Move(ix2, iy); driver.AddRune(driver.VLine);
            }
            for (int ix = ix1 + 1; ix <= ix2 - 1; ix++) {
                driver.Move(ix, iy1); driver.AddRune(driver.HLine);
                driver.Move(ix, iy2); driver.AddRune(driver.HLine);
            }
            driver.Move(ix1, iy1); driver.AddRune(driver.ULCorner);
            driver.Move(ix2, iy1); driver.AddRune(driver.URCorner);
            driver.Move(ix1, iy2); driver.AddRune(driver.LLCorner);
            driver.Move(ix2, iy2); driver.AddRune(driver.LRCorner);*/
        }

        public override void FillRectangle(IBrush brush, float x, float y, float width, float height)
        {
            // *
        }

        public override void DrawPath(IPen pen, IBrush brush, IGfxPath path)
        {
            // implementation is impossible
        }

        public override IPen CreatePen(IColor color, float width, float[] dashPattern = null)
        {
            // *
            return null;
        }

        public override IBrush CreateBrush(IColor color)
        {
            // *
            return null;
        }

        public override IGfxPath CreateCirclePath(float x, float y, float width, float height)
        {
            // implementation is impossible
            return null;
        }

        public override IGfxPath CreateCircleSegmentPath(int ctX, int ctY, float inRad, float extRad, float wedgeAngle, float ang1, float ang2)
        {
            // implementation is impossible
            return null;
        }

        public override void SetTranslucent(float value)
        {
            // implementation is impossible
        }

        public override void ScaleTransform(float sx, float sy)
        {
            // implementation is impossible
        }

        public override void TranslateTransform(float dx, float dy)
        {
            // implementation is impossible
        }

        public override void RotateTransform(float angle)
        {
            // implementation is impossible
        }

        public override void RestoreTransform()
        {
            // implementation is impossible
        }

        public override void SaveTransform()
        {
            // implementation is impossible
        }
    }
}
