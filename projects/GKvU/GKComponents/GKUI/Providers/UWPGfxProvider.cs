/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2017-2018 by Sergey V. Zhdanovskih.
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
using System.IO;
using BSLib;
using BSLib.Design.Graphics;
using GKCore.Interfaces;
using GKUI.Components;
using Windows.UI;

namespace GKUI.Providers
{
    /// <summary>
    /// 
    /// </summary>
    public class UWPGfxProvider : IGraphicsProviderEx
    {
        public UWPGfxProvider()
        {
        }

        public IImage LoadImage(string fileName)
        {
            /*if (fileName == null)
                throw new ArgumentNullException("fileName");

            using (Bitmap bmp = new Bitmap(fileName))
            {
                // cloning is necessary to release the resource
                // loaded from the image stream
                Bitmap resImage = (Bitmap)bmp.Clone();

                return new ImageHandler(resImage);
            }*/
            throw new NotImplementedException();
        }

        public void SaveImage(IImage image, string fileName)
        {
            /*if (image == null)
                throw new ArgumentNullException("image");

            if (fileName == null)
                throw new ArgumentNullException("fileName");

            ((ImageHandler)image).Handle.Save(fileName, ImageFormat.Bmp);*/
            throw new NotImplementedException();
        }

        public IImage CreateImage(Stream stream)
        {
            /*if (stream == null)
                throw new ArgumentNullException("stream");

            using (Bitmap bmp = new Bitmap(stream))
            {
                // cloning is necessary to release the resource
                // loaded from the image stream
                Bitmap resImage = (Bitmap)bmp.Clone();

                return new ImageHandler(resImage);
            }*/
            throw new NotImplementedException();
        }

        public IImage CreateImage(Stream stream, int thumbWidth, int thumbHeight, ExtRect cutoutArea)
        {
            /*if (stream == null)
                throw new ArgumentNullException("stream");

            using (Bitmap bmp = new Bitmap(stream))
            {
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
            }*/
            throw new NotImplementedException();
        }

        public IImage LoadResourceImage(string resName, bool makeTransp)
        {
            /*Bitmap img = (Bitmap)UIHelper.LoadResourceImage("Resources." + resName);

            if (makeTransp) {
                img = (Bitmap)img.Clone();

                #if __MonoCS__
                img.MakeTransparent();
                #else
                img.MakeTransparent(img.GetPixel(0, 0));
                #endif
            }

            return new ImageHandler(img);*/
            throw new NotImplementedException();
        }

        public IGfxPath CreatePath()
        {
            //return new GfxPathHandler(new GraphicsPath());
            throw new NotImplementedException();
        }

        public IGfxPath CreateCirclePath(float x, float y, float width, float height)
        {
            /*var path = new GraphicsPath();
            var result = new GfxCirclePathHandler(path);

            result.X = x;
            result.Y = y;
            result.Width = width;
            result.Height = height;

            path.StartFigure();
            path.AddEllipse(x, y, width, height);
            path.CloseFigure();

            return result;*/
            throw new NotImplementedException();
        }

        public IGfxPath CreateCircleSegmentPath(float inRad, float extRad, float wedgeAngle, float ang1, float ang2)
        {
            /*var path = new GraphicsPath();
            var result = new GfxCircleSegmentPathHandler(path);

            result.InRad = inRad;
            result.ExtRad = extRad;
            result.WedgeAngle = wedgeAngle;
            result.Ang1 = ang1;
            result.Ang2 = ang2;

            UIHelper.CreateCircleSegment(path, 0, 0, inRad, extRad, wedgeAngle, ang1, ang2);

            return result;*/
            throw new NotImplementedException();
        }

        public IGfxPath CreateCircleSegmentPath(int ctX, int ctY, float inRad, float extRad, float wedgeAngle,
            float ang1, float ang2)
        {
            /*var path = new GraphicsPath();
            var result = new GfxCircleSegmentPathHandler(path);

            result.InRad = inRad;
            result.ExtRad = extRad;
            result.WedgeAngle = wedgeAngle;
            result.Ang1 = ang1;
            result.Ang2 = ang2;

            UIHelper.CreateCircleSegment(path, ctX, ctY, inRad, extRad, wedgeAngle, ang1, ang2);

            return result;*/
            throw new NotImplementedException();
        }

        public IFont CreateFont(string fontName, float size, bool bold)
        {
            /*FontStyle style = (!bold) ? FontStyle.Regular : FontStyle.Bold;
            var sdFont = new Font(fontName, size, style, GraphicsUnit.Point);
            return new FontHandler(sdFont);*/
            throw new NotImplementedException();
        }

        public IColor CreateColor(int argb)
        {
            // Dirty hack!
            //argb = (int)unchecked((long)argb & (long)((ulong)-1));
            //argb = (int)unchecked((ulong)argb & (uint)0xFF000000);
            byte red = (byte)((argb >> 16) & 0xFF);
            byte green = (byte)((argb >> 8) & 0xFF);
            byte blue = (byte)((argb >> 0) & 0xFF);

            Color color = Color.FromArgb(0, red, green, blue);
            return new ColorHandler(color);
        }

        public IColor CreateColor(int r, int g, int b)
        {
            Color color = Color.FromArgb(0, (byte)r, (byte)g, (byte)b);
            return new ColorHandler(color);
        }

        public IColor CreateColor(int a, int r, int g, int b)
        {
            Color color = Color.FromArgb((byte)a, (byte)r, (byte)g, (byte)b);
            return new ColorHandler(color);
        }

        public IColor CreateColor(string signature)
        {
            return null;
        }

        public IBrush CreateSolidBrush(IColor color)
        {
            //Color sdColor = ((ColorHandler)color).Handle;
            //return new BrushHandler(new SolidBrush(sdColor));
            throw new NotImplementedException();
        }

        public IPen CreatePen(IColor color, float width)
        {
            //Color sdColor = ((ColorHandler)color).Handle;
            //return new PenHandler(new Pen(sdColor, width));
            throw new NotImplementedException();
        }

        public ExtSizeF GetTextSize(string text, IFont font, object target)
        {
            /*Graphics gfx = target as Graphics;
            if (gfx != null && font != null) {
                Font sdFnt = ((FontHandler)font).Handle;
                var size = gfx.MeasureString(text, sdFnt);
                return new ExtSizeF(size.Width, size.Height);
            } else {
                return new ExtSizeF();
            }*/
            throw new NotImplementedException();
        }

        public string GetDefaultFontName()
        {
            string fontName;
            #if __MonoCS__
            fontName = "Noto Sans";
            #else
            fontName = "Verdana"; // "Tahoma";
            #endif
            return fontName;
        }
    }
}
