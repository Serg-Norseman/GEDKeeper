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
        private static readonly GKCore.Logging.ILogger fLogger = LogManager.GetLogger(GMConfig.LOG_FILE, GMConfig.LOG_LEVEL, typeof(Paintbox).Name);

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


        // Construct the paintbox, reading values for the colours etc from the config.
        public Paintbox(GMConfig config)
        {
            BrushBg = new SolidBrush(config.MiniTreeColourBackground);
            BrushBox = new SolidBrush(config.MiniTreeColourIndiBackground);
            BrushBoxHighlight = new SolidBrush(config.MiniTreeColourIndiHighlight);
            BrushBoxConcealed = new SolidBrush(config.MiniTreeColourIndiBgConcealed);
            BrushBoxShade = new SolidBrush(config.MiniTreeColourIndiShade);
            BrushText = new SolidBrush(config.MiniTreeColourIndiText);
            BrushTextLink = new SolidBrush(config.MiniTreeColourIndiLink);
            BrushTextConcealed = new SolidBrush(config.MiniTreeColourIndiFgConcealed);

            PenConnector = new Pen(config.MiniTreeColourBranch, 1.0f);
            PenConnectorDotted = new Pen(config.MiniTreeColourBranch, 1.0f);
            PenConnectorDotted.DashStyle = System.Drawing.Drawing2D.DashStyle.Dot;
            PenBox = new Pen(config.MiniTreeColourIndiBorder, 1.0f);
            BrushFakeTransparency = null;
            Font = new Font(config.TreeFontName, config.TreeFontSize);
        }

        // Clean up any resources being used.
        protected override void Dispose(bool disposing)
        {
            if (disposing) {
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
                    fLogger.WriteError("SetBackgroundImage()", e);
                    BrushFakeTransparency = null;
                }
            }
        }
    }
}
