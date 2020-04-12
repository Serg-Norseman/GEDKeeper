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
using System.Drawing;
using System.Drawing.Imaging;
using BSLib;
using GKCore.Logging;

namespace GEDmill.MiniTree
{
    /// <summary>
    /// A data structure to contain the graphics drawing elements in the appropriate colours for the tree diagram.
    /// </summary>
    public class Paintbox : BaseObject
    {
        private static readonly GKCore.Logging.ILogger fLogger = LogManager.GetLogger(CConfig.LOG_FILE, CConfig.LOG_LEVEL, typeof(Paintbox).Name);

        public SolidBrush BrushBg;
        public SolidBrush BrushBox;
        public SolidBrush BrushBoxHighlight;
        public SolidBrush BrushBoxConcealed;
        public SolidBrush BrushBoxShade;
        public SolidBrush BrushText;
        public SolidBrush BrushTextLink;
        public SolidBrush BrushTextConcealed;
        public Pen PenConnector;
        public Pen PenConnectorDotted;
        public Pen PenBox;
        public Font Font;
        public ColorPalette Palette;
        public TextureBrush BrushFakeTransparency;
        public SolidBrush BrushBgGif;
        public Color ColourOutline;
        public Color ColourBox;
        public Color ColourHighlight;
        public Color ColourBgConcealed;
        public Color ColourShade;
        public Color ColourText;
        public Color ColourLink;
        public Color ColourFgConcealed;
        public Color ColourConnector;
        public Color ColourGifTransparent;
        public Color ColourBg;


        // Construct the paintbox, reading values for the colours etc from the config.
        public Paintbox(CConfig config)
        {
            ColourBg = GMHelper.ConvertColour(config.MiniTreeColourBackground);
            ColourOutline = GMHelper.ConvertColour(config.MiniTreeColourIndiBorder);
            ColourBox = GMHelper.ConvertColour(config.MiniTreeColourIndiBackground);
            ColourHighlight = GMHelper.ConvertColour(config.MiniTreeColourIndiHighlight);
            ColourBgConcealed = GMHelper.ConvertColour(config.MiniTreeColourIndiBgConcealed);
            ColourShade = GMHelper.ConvertColour(config.MiniTreeColourIndiShade);
            ColourText = GMHelper.ConvertColour(config.MiniTreeColourIndiText);
            ColourLink = GMHelper.ConvertColour(config.MiniTreeColourIndiLink);
            ColourFgConcealed = GMHelper.ConvertColour(config.MiniTreeColourIndiFgConcealed);
            ColourConnector = GMHelper.ConvertColour(config.MiniTreeColourBranch);
            ColourGifTransparent = Color.Magenta;
            BrushBgGif = new SolidBrush(ColourGifTransparent);
            BrushBg = new SolidBrush(ColourBg);
            BrushBox = new SolidBrush(ColourBox);
            BrushBoxHighlight = new SolidBrush(ColourHighlight);
            BrushBoxConcealed = new SolidBrush(ColourBgConcealed);
            BrushBoxShade = new SolidBrush(ColourShade);
            BrushText = new SolidBrush(ColourText);
            BrushTextLink = new SolidBrush(ColourLink);
            BrushTextConcealed = new SolidBrush(ColourFgConcealed);
            PenConnector = new Pen(ColourConnector, 1.0f);
            PenConnectorDotted = new Pen(ColourConnector, 1.0f);
            PenConnectorDotted.DashStyle = System.Drawing.Drawing2D.DashStyle.Dot;
            PenBox = new Pen(ColourOutline, 1.0f);
            Font = new Font("Microsoft Sans Serif", 10f);
            BrushFakeTransparency = null;
            Font = new Font(config.TreeFontName, config.TreeFontSize);
            BrushFakeTransparency = null;
        }

        // Clean up any resources being used.
        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                BrushBgGif.Dispose();
                BrushBg.Dispose();
                BrushBox.Dispose();
                BrushBoxHighlight.Dispose();
                BrushBoxConcealed.Dispose();
                BrushBoxShade.Dispose();
                BrushText.Dispose();
                BrushTextLink.Dispose();
                BrushTextConcealed.Dispose();
                PenConnector.Dispose();
                PenConnectorDotted.Dispose();
                PenBox.Dispose();
                Font.Dispose();
                if (BrushFakeTransparency != null) {
                    BrushFakeTransparency.Dispose();
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
                    BrushFakeTransparency = new TextureBrush(bgImage);
                } catch (Exception e) {
                    // e.g. System.IO.FileNotFoundException
                    fLogger.WriteInfo(string.Format("SetBackgroundImage() Caught exception {0}", e.ToString()));
                    BrushFakeTransparency = null;
                }
            }
        }
    }
}
