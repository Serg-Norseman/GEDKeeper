/* 
 * Copyright 2009 Alexander Curtis <alex@logicmill.com>
 * This file is part of GEDmill - A family history website creator
 * 
 * GEDmill is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * GEDmill is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with GEDmill.  If not, see <http://www.gnu.org/licenses/>.
 */

using System;
using System.Collections.Generic;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Drawing.Imaging;
using System.IO;
using System.Threading.Tasks;
using BSLib;
using GEDmill.HTML;
using GEDmill.MiniTree;
using GKCore;
using GKUI.Components;
using GKUI.Platform.Handlers;
using GKL = GKCore.Logging;

namespace GEDmill
{
    /// <summary>
    /// Class that calculates and draws a mini tree diagram
    /// </summary>
    public class TreeDrawer : BaseObject, ITreeDrawer
    {
        private static readonly GKL.ILogger fLogger = GKL.LogManager.GetLogger(GMConfig.LOG_FILE, GMConfig.LOG_LEVEL, typeof(TreeDrawer).Name);

        private SolidBrush fBrushBg;
        private SolidBrush fBrushBox;
        private SolidBrush fBrushBoxHighlight;
        private SolidBrush fBrushBoxConcealed;
        private SolidBrush fBrushBoxShade;
        private SolidBrush fBrushText;
        private SolidBrush fBrushTextLink;
        private SolidBrush fBrushTextConcealed;
        private Pen fPenConnector;
        private Pen fPenConnectorDotted;
        private Pen fPenBox;
        private Font fFont;
        private TextureBrush fBrushFakeTransparency;

        private Bitmap fTempBmp;
        private Graphics fTempGfx;


        public TreeDrawer(GMConfig config)
        {
            fBrushBg = new SolidBrush(Color.FromArgb(config.MiniTreeColorBackground));
            fBrushBox = new SolidBrush(Color.FromArgb(config.MiniTreeColorIndiBackground));
            fBrushBoxHighlight = new SolidBrush(Color.FromArgb(config.MiniTreeColorIndiHighlight));
            fBrushBoxConcealed = new SolidBrush(Color.FromArgb(config.MiniTreeColorIndiBgConcealed));
            fBrushBoxShade = new SolidBrush(Color.FromArgb(config.MiniTreeColorIndiShade));
            fBrushText = new SolidBrush(Color.FromArgb(config.MiniTreeColorIndiText));
            fBrushTextLink = new SolidBrush(Color.FromArgb(config.MiniTreeColorIndiLink));
            fBrushTextConcealed = new SolidBrush(Color.FromArgb(config.MiniTreeColorIndiFgConcealed));

            fPenConnector = new Pen(Color.FromArgb(config.MiniTreeColorBranch), 1.0f);
            fPenConnectorDotted = new Pen(Color.FromArgb(config.MiniTreeColorBranch), 1.0f);
            fPenConnectorDotted.DashStyle = DashStyle.Dot;
            fPenBox = new Pen(Color.FromArgb(config.MiniTreeColorIndiBorder), 1.0f);
            fBrushFakeTransparency = null;
            fFont = new Font(config.TreeFontName, config.TreeFontSize);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                fBrushBg.Dispose();
                fBrushBox.Dispose();
                fBrushBoxHighlight.Dispose();
                fBrushBoxConcealed.Dispose();
                fBrushBoxShade.Dispose();
                fBrushText.Dispose();
                fBrushTextLink.Dispose();
                fBrushTextConcealed.Dispose();
                fPenConnector.Dispose();
                fPenConnectorDotted.Dispose();
                fPenBox.Dispose();
                fFont.Dispose();
                if (fBrushFakeTransparency != null) {
                    fBrushFakeTransparency.Dispose();
                }
            }
            base.Dispose(disposing);
        }

        // Sets the brush used to fill the background to the graphics image provided.
        public void SetBackgroundImage(string filename)
        {
            if (!string.IsNullOrEmpty(filename)) {
                try {
                    Image bgImage = Image.FromFile(filename);
                    fBrushFakeTransparency = new TextureBrush(bgImage);
                } catch (Exception e) {
                    // e.g. System.IO.FileNotFoundException
                    fLogger.WriteError("SetBackgroundImage()", e);
                    fBrushFakeTransparency = null;
                }
            }
        }

        public ExtSizeF MeasureString(string text)
        {
            var sz = fTempGfx.MeasureString(text, fFont);
            return new ExtSizeF(sz.Width, sz.Height);
        }

        public void InitTempGfx(int width, int height, int pixelFormat, bool background)
        {
            fTempBmp = new Bitmap(width, height, (pixelFormat == 24) ? PixelFormat.Format24bppRgb : PixelFormat.Format32bppArgb);
            fTempGfx = Graphics.FromImage(fTempBmp);

            // Record what font windows actually used, in case it chose a different one
            GMConfig.Instance.TreeFontName = fFont.Name;
            GMConfig.Instance.TreeFontSize = fFont.Size;

            // Do background fill
            if (background && GMConfig.Instance.FakeMiniTreeTransparency && fBrushFakeTransparency != null) {
                fTempGfx.FillRectangle(fBrushFakeTransparency, 0, 0, width, height);
            }
        }

        public void DoneTempGfx()
        {
            fTempGfx.Dispose();
            fTempBmp.Dispose();
        }

        public void SaveTempGfx(Stream fileStream)
        {
            fTempBmp.Save(fileStream, ImageFormat.Png);
        }

        // Draws the group to the graphics instance.
        public void DrawGroup(MTGroup group, List<MTMap> map)
        {
            if (group.Members == null) {
                // Empty group
                return;
            }

            foreach (var obj in group.Members) {
                if (obj is MTGroup) {
                    var mtg = (MTGroup)obj;
                    if (mtg.LeftBox != null && mtg.RightBox != null) {
                        // Draw crossbar
                        float crossbarLeft = mtg.LeftBox.TeeRight;
                        float crossbarRight = mtg.RightBox.TeeLeft;
                        float crossbarLeftGap = mtg.LeftBox.Right;
                        float crossbarRightGap = mtg.RightBox.Left;
                        float crossbarY = (mtg.LeftBox.TeeCentreVert + mtg.RightBox.TeeCentreVert) / 2f;
                        switch (mtg.fCrossbar) {
                            case ECrossbar.Solid:
                                fTempGfx.DrawLine(fPenConnector, crossbarLeft, crossbarY, crossbarRight, crossbarY);
                                break;

                            case ECrossbar.DottedLeft:
                                fTempGfx.DrawLine(fPenConnectorDotted, crossbarLeft, crossbarY, crossbarRightGap, crossbarY);
                                break;

                            case ECrossbar.DottedRight:
                                fTempGfx.DrawLine(fPenConnectorDotted, crossbarLeftGap, crossbarY, crossbarRight, crossbarY);
                                break;
                        }

                        if (mtg.StalkedIndividuals > 0) {
                            // Draw down to individuals
                            // Use y coord of first individual, assuming all are at the same y coord
                            float individualY = 0f;
                            bool haveIndividuals = false;
                            foreach (MTObject groupObj in mtg.Members) {
                                if (groupObj is MTIndividual) {
                                    individualY = ((MTIndividual)groupObj).Top;
                                    haveIndividuals = true;
                                    break;
                                }
                            }
                            float crossbarCentre = (crossbarLeft + crossbarRight) / 2f;
                            if (haveIndividuals) {
                                fTempGfx.DrawLine(fPenConnector, crossbarCentre, crossbarY, crossbarCentre, individualY);

                                // Connect individuals
                                var stalkMinMax = mtg.StalkMinMax;

                                // Width irrelevant, using SizeF simply as a way to pass 2 floats:
                                float stalkMin = stalkMinMax.Width;

                                // Height irrelevant, using SizeF simply as a way to pass 2 floats
                                float stalkMax = stalkMinMax.Height;

                                if (crossbarCentre < stalkMin) {
                                    stalkMin = crossbarCentre;
                                } else if (crossbarCentre > stalkMax) {
                                    stalkMax = crossbarCentre;
                                }
                                fTempGfx.DrawLine(fPenConnector, stalkMin, individualY, stalkMax, individualY);
                            }
                        }
                    }

                    DrawGroup(mtg, map);
                } else if (obj is MTIndividual) {
                    // Draw individual box
                    DrawIndividual((MTIndividual)obj, map);
                }
            }
        }

        // Draws the actual box, and adds the region of the box to the image alMap list.
        public void DrawIndividual(MTIndividual indi, List<MTMap> map)
        {
            SolidBrush solidbrushBg, solidbrushText;

            if (indi.Concealed) {
                solidbrushBg = fBrushBoxConcealed;
            } else if (indi.Highlight) {
                solidbrushBg = fBrushBoxHighlight;
            } else if (indi.Shade) {
                solidbrushBg = fBrushBoxShade;
            } else {
                solidbrushBg = fBrushBox;
            }

            if (indi.Linkable) {
                solidbrushText = fBrushTextLink;
            } else if (indi.Concealed) {
                solidbrushText = fBrushTextConcealed;
            } else {
                solidbrushText = fBrushText;
            }

            fTempGfx.FillRectangle(solidbrushBg, indi.Left + MTIndividual.MARGIN_HORIZ, indi.Top + MTIndividual.MARGIN_VERT, indi.SizeText.Width, indi.SizeText.Height - 1f);
            fTempGfx.DrawRectangle(fPenBox, indi.Left + MTIndividual.MARGIN_HORIZ, indi.Top + MTIndividual.MARGIN_VERT, indi.SizeText.Width, indi.SizeText.Height - 1f);

            float fTextX = indi.Left + MTIndividual.MARGIN_HORIZ + MTIndividual.PADDING_HORIZ;
            float fTextY = indi.Top + MTIndividual.MARGIN_VERT + MTIndividual.PADDING_VERT;
            if (indi.ConserveWidth) {
                fTempGfx.DrawString(indi.Firstnames, fFont, solidbrushText, fTextX + indi.FirstnamesPad, fTextY);
                fTextY += indi.SizeFirstnames.Height;
                fTempGfx.DrawString(indi.Surname, fFont, solidbrushText, fTextX + indi.SurnamePad, fTextY);
                fTextY += indi.SizeSurname.Height;
            } else {
                fTempGfx.DrawString(indi.Name, fFont, solidbrushText, fTextX + indi.SurnamePad, fTextY);
                fTextY += indi.SizeSurname.Height;
            }

            fTempGfx.DrawString(indi.Date, fFont, solidbrushText, fTextX + indi.DatePad, fTextY);

            if (indi.Child) {
                fTempGfx.DrawLine(fPenConnector,
                    indi.Left + MTIndividual.MARGIN_HORIZ + (indi.SizeText.Width / 2f), indi.Top,
                    indi.Left + MTIndividual.MARGIN_HORIZ + (indi.SizeText.Width / 2f), indi.Top + MTIndividual.MARGIN_VERT/* -1f*/ );
            }

            if (indi.IndiRec != null) {
                map.Add(new MTMap(indi.Name, indi.IndiRec, indi.Linkable,
                    (int)(indi.Left + MTIndividual.MARGIN_HORIZ), (int)(indi.Top + MTIndividual.MARGIN_VERT),
                    (int)(indi.Left + MTIndividual.MARGIN_HORIZ + indi.SizeText.Width), (int)(indi.Top + MTIndividual.MARGIN_VERT + indi.SizeText.Height - 1f)));
            }
        }


        public static async Task<int> SelectColor(int color)
        {
            var colorHandle = await AppHost.StdDialogs.SelectColor(new ColorHandler(Color.FromArgb(color)));
            var newColor = colorHandle.ToArgb();
            return newColor;
        }

        // Crops the specified image file to the given size. Also converts non-standard formats to standard ones.
        // Returns sFilename in case extension has changed.
        // sArea is changed to reflect new image size
        public static string ConvertAndCropImage(string folder, string fileName, ref ExtRect rectArea, int maxWidth, int maxHeight)
        {
            fLogger.WriteInfo(string.Format("ConvertAndCropImage( {0}, {1} )", folder != null ? folder : "null", fileName != null ? fileName : "null"));

            string absFilename = string.Concat(folder, fileName);

            Image image = null;
            try {
                image = Image.FromFile(absFilename);
            } catch (OutOfMemoryException) {
                // Image is not a GDI compatible format
                image = null;
            }

            if (image == null) {
                throw (new HTMLException("Unknown image format for file " + absFilename)); // Let caller sort it out.
            }

            ExtRect rectNewArea;
            if (rectArea.Width <= 0 || rectArea.Height <= 0) {
                SizeF s = image.PhysicalDimension;
                if (s.Width <= maxWidth && s.Height <= maxHeight) {
                    maxWidth = (int)s.Width;
                    maxHeight = (int)s.Height;
                    // Nothing needs to be done, bitmap already correct size.
                    // Carry on with conversion.
                }
                rectNewArea = new ExtRect(0, 0, (int)s.Width, (int)s.Height);
                rectArea = new ExtRect(0, 0, rectNewArea.Width, rectNewArea.Height);
            } else {
                rectNewArea = new ExtRect(0, 0, rectArea.Width, rectArea.Height);
            }

            if (maxWidth != 0 && maxHeight != 0) {
                // If image is too big then shrink it. (Can't always use GetThumbnailImage because that might use embedded thumbnail).
                GMHelper.ScaleAreaToFit(ref rectNewArea, maxWidth, maxHeight);
            }

            Bitmap bitmapNew = new Bitmap(rectNewArea.Width, rectNewArea.Height, PixelFormat.Format24bppRgb);
            Graphics graphicsNew = Graphics.FromImage(bitmapNew);

            graphicsNew.DrawImage(image, UIHelper.Rt2Rt(rectNewArea), UIHelper.Rt2Rt(rectArea), GraphicsUnit.Pixel);
            image.Dispose();

            // Find which format to save in. TODO: There must be a more elegant way!!
            string extn = Path.GetExtension(fileName);
            string filepart = Path.GetDirectoryName(fileName);
            filepart += "\\" + Path.GetFileNameWithoutExtension(fileName);
            ImageFormat imageFormat;
            switch (extn.ToLower()) {
                case ".jpg":
                case ".jpeg":
                    extn = ".jpg";
                    imageFormat = ImageFormat.Jpeg;
                    break;
                case ".gif":
                    imageFormat = ImageFormat.Gif;
                    break;
                case ".bmp":
                    imageFormat = ImageFormat.Bmp;
                    break;
                case ".tif":
                case ".tiff":
                    // Tif's don't display in browsers, so convert to png.
                    imageFormat = ImageFormat.Png;
                    extn = ".png";
                    break;
                case ".exif":
                    imageFormat = ImageFormat.Exif;
                    break;
                case ".png":
                    imageFormat = ImageFormat.Png;
                    break;
                default:
                    imageFormat = ImageFormat.Jpeg;
                    break;
            }

            string filenameNew = filepart + extn;
            string absFilenameNew = string.Concat(folder, filenameNew);
            try {
                if (File.Exists(absFilename)) {
                    // Delete the old file (e.g. if converting from tif to png)
                    File.Delete(absFilename);
                }
            } catch (Exception e) {
                fLogger.WriteError(string.Format("Caught exception while removing old bitmap file {0}", absFilename), e);
            }
            try {
                if (File.Exists(absFilenameNew)) {
                    // Delete any existing file
                    File.SetAttributes(absFilenameNew, FileAttributes.Normal);
                    File.Delete(absFilenameNew);
                }
                bitmapNew.Save(absFilenameNew, imageFormat);
            } catch (Exception e) {
                fLogger.WriteError(string.Format("Caught exception while writing bitmap file {0}", filenameNew), e);
                filenameNew = "";
            }
            graphicsNew.Dispose();
            bitmapNew.Dispose();

            rectArea = rectNewArea;
            return filenameNew;
        }

        public static Bitmap LoadResourceImage(string name)
        {
            string resName = "GEDmill.res." + name;
            Stream resStream = GMHelper.LoadResourceStream(resName);
            return new Bitmap(resStream);
        }
    }
}
