/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2017-2022 by Sergey V. Zhdanovskih.
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
using System.Drawing.Imaging;
using System.IO;
using BSLib;
using BSLib.Design.Graphics;
using BSLib.Design.Handlers;
using GKCore;
using GKCore.Interfaces;
using GKUI.Components;

namespace GKUI.Platform
{
    /// <summary>
    /// The main implementation of the platform-specific graphics provider for WinForms.
    /// </summary>
    public class WFGfxProvider : IGraphicsProviderEx
    {
        public WFGfxProvider()
        {
        }

        public IImage LoadImage(string fileName)
        {
            if (string.IsNullOrEmpty(fileName))
                throw new ArgumentNullException("fileName");

            if (!File.Exists(fileName))
                return null;

            try {
                using (Bitmap bmp = new Bitmap(fileName)) {
                    UIHelper.NormalizeOrientation(bmp);

                    // cloning is necessary to release the resource
                    // loaded from the image stream
                    Bitmap resImage = (Bitmap)bmp.Clone();

                    return new ImageHandler(resImage);
                }
            } catch (Exception ex) {
                Logger.WriteError(string.Format("WFGfxProvider.LoadImage({0})", fileName), ex);
                return null;
            }
        }

        public void SaveImage(IImage image, string fileName)
        {
            if (image == null)
                throw new ArgumentNullException("image");

            if (fileName == null)
                throw new ArgumentNullException("fileName");

            ((ImageHandler)image).Handle.Save(fileName, ImageFormat.Bmp);
        }

        public IImage CreateImage(Stream stream)
        {
            if (stream == null)
                throw new ArgumentNullException("stream");

            using (Bitmap bmp = new Bitmap(stream))
            {
                UIHelper.NormalizeOrientation(bmp);

                // cloning is necessary to release the resource
                // loaded from the image stream
                Bitmap resImage = (Bitmap)bmp.Clone();

                return new ImageHandler(resImage);
            }
        }

        public IImage CreateImage(Stream stream, int thumbWidth, int thumbHeight, ExtRect cutoutArea)
        {
            if (stream == null)
                throw new ArgumentNullException("stream");

            using (Bitmap bmp = new Bitmap(stream))
            {
                UIHelper.NormalizeOrientation(bmp);

                bool cutoutIsEmpty = cutoutArea.IsEmpty();
                int imgWidth = (cutoutIsEmpty) ? bmp.Width : cutoutArea.GetWidth();
                int imgHeight = (cutoutIsEmpty) ? bmp.Height : cutoutArea.GetHeight();

                if (thumbWidth > 0 && thumbHeight > 0) {
                    float ratio = GfxHelper.ZoomToFit(imgWidth, imgHeight, thumbWidth, thumbHeight);
                    imgWidth = (int)(imgWidth * ratio);
                    imgHeight = (int)(imgHeight * ratio);
                }

                Bitmap newImage = new Bitmap(imgWidth, imgHeight, PixelFormat.Format24bppRgb);
                using (Graphics graphic = Graphics.FromImage(newImage)) {
                    graphic.InterpolationMode = InterpolationMode.HighQualityBicubic;
                    graphic.SmoothingMode = SmoothingMode.HighQuality;
                    graphic.PixelOffsetMode = PixelOffsetMode.HighQuality;
                    graphic.CompositingQuality = CompositingQuality.HighQuality;

                    if (cutoutIsEmpty) {
                        graphic.DrawImage(bmp, 0, 0, imgWidth, imgHeight);
                    } else {
                        Rectangle destRect = new Rectangle(0, 0, imgWidth, imgHeight);
                        //Rectangle srcRect = cutoutArea.ToRectangle();
                        graphic.DrawImage(bmp, destRect,
                                          cutoutArea.Left, cutoutArea.Top,
                                          cutoutArea.GetWidth(), cutoutArea.GetHeight(),
                                          GraphicsUnit.Pixel);
                    }
                }

                return new ImageHandler(newImage);
            }
        }

        public IImage LoadResourceImage(string resName, bool makeTransp)
        {
            Bitmap img = UIHelper.LoadResourceImage("Resources." + resName);

            if (makeTransp) {
                img = (Bitmap)img.Clone();

                #if MONO
                img.MakeTransparent();
                #else
                img.MakeTransparent(img.GetPixel(0, 0));
                #endif
            }

            return new ImageHandler(img);
        }

        public IGfxPath CreatePath()
        {
            return new GfxPathHandler(new GraphicsPath());
        }

        public IGfxPath CreateCirclePath(float x, float y, float width, float height)
        {
            var path = new GraphicsPath();
            var result = new GfxCirclePathHandler(path);

            result.X = x;
            result.Y = y;
            result.Width = width;
            result.Height = height;

            path.StartFigure();
            path.AddEllipse(x, y, width, height);
            path.CloseFigure();

            return result;
        }

        public IGfxPath CreateCircleSegmentPath(float inRad, float extRad, float wedgeAngle, float ang1, float ang2)
        {
            var path = new GraphicsPath();
            var result = new GfxCircleSegmentPathHandler(path);

            result.InRad = inRad;
            result.ExtRad = extRad;
            result.WedgeAngle = wedgeAngle;
            result.Ang1 = ang1;
            result.Ang2 = ang2;

            CreateCircleSegment(path, 0, 0, inRad, extRad, wedgeAngle, ang1, ang2);

            return result;
        }

        public IGfxPath CreateCircleSegmentPath(int ctX, int ctY, float inRad, float extRad, float wedgeAngle,
            float ang1, float ang2)
        {
            var path = new GraphicsPath();
            var result = new GfxCircleSegmentPathHandler(path);

            result.InRad = inRad;
            result.ExtRad = extRad;
            result.WedgeAngle = wedgeAngle;
            result.Ang1 = ang1;
            result.Ang2 = ang2;

            CreateCircleSegment(path, ctX, ctY, inRad, extRad, wedgeAngle, ang1, ang2);

            return result;
        }

        private static void CreateCircleSegment(GraphicsPath path, int ctX, int ctY,
                                               float inRad, float extRad, float wedgeAngle,
                                               float ang1, float ang2)
        {
            float angCos, angSin;

            float angval1 = (float)(ang1 * Math.PI / 180.0f);
            angCos = (float)Math.Cos(angval1);
            angSin = (float)Math.Sin(angval1);
            float px1 = ctX + (inRad * angCos);
            float py1 = ctY + (inRad * angSin);
            float px2 = ctX + (extRad * angCos);
            float py2 = ctY + (extRad * angSin);

            float angval2 = (float)(ang2 * Math.PI / 180.0f);
            angCos = (float)Math.Cos(angval2);
            angSin = (float)Math.Sin(angval2);
            float nx1 = ctX + (inRad * angCos);
            float ny1 = ctY + (inRad * angSin);
            float nx2 = ctX + (extRad * angCos);
            float ny2 = ctY + (extRad * angSin);

            float ir2 = inRad * 2.0f;
            float er2 = extRad * 2.0f;

            path.StartFigure();
            path.AddLine(px2, py2, px1, py1);
            if (ir2 > 0) path.AddArc(ctX - inRad, ctY - inRad, ir2, ir2, ang1, wedgeAngle);
            path.AddLine(nx1, ny1, nx2, ny2);
            path.AddArc(ctX - extRad, ctY - extRad, er2, er2, ang2, -wedgeAngle);
            path.CloseFigure();
        }

        public IFont CreateFont(string fontName, float size, bool bold)
        {
            FontStyle style = (!bold) ? FontStyle.Regular : FontStyle.Bold;
            var sdFont = new Font(fontName, size, style, GraphicsUnit.Point);
            return new FontHandler(sdFont);
        }

        public IColor CreateColor(int argb)
        {
            // FIXME: Dirty hack!
            //argb = (int)unchecked((long)argb & (long)((ulong)-1));
            //argb = (int)unchecked((ulong)argb & (uint)0xFF000000);
            int red = (argb >> 16) & 0xFF;
            int green = (argb >> 8) & 0xFF;
            int blue = (argb >> 0) & 0xFF;

            Color color = Color.FromArgb(red, green, blue);
            return new ColorHandler(color);
        }

        public IColor CreateColor(int r, int g, int b)
        {
            Color color = Color.FromArgb(r, g, b);
            return new ColorHandler(color);
        }

        public IColor CreateColor(int a, int r, int g, int b)
        {
            Color color = Color.FromArgb(a, r, g, b);
            return new ColorHandler(color);
        }

        public IColor CreateColor(string signature)
        {
            return null;
        }

        public IBrush CreateSolidBrush(IColor color)
        {
            Color sdColor = ((ColorHandler)color).Handle;

            return new BrushHandler(new SolidBrush(sdColor));
        }

        public IPen CreatePen(IColor color, float width)
        {
            Color sdColor = ((ColorHandler)color).Handle;

            return new PenHandler(new Pen(sdColor, width));
        }

        public ExtSizeF GetTextSize(string text, IFont font, object target)
        {
            Graphics gfx = target as Graphics;
            if (gfx != null && font != null) {
                Font sdFnt = ((FontHandler)font).Handle;
                var size = gfx.MeasureString(text, sdFnt);
                return new ExtSizeF(size.Width, size.Height);
            } else {
                return new ExtSizeF();
            }
        }

        public string GetDefaultFontName()
        {
            string fontName;
            #if MONO
            fontName = "Noto Sans";
            #else
            fontName = "Verdana"; // "Tahoma";
            #endif
            return fontName;
        }
    }
}
