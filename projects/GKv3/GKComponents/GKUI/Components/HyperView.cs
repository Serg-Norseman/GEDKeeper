/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2011, 2017 by Sergey V. Zhdanovskih.
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
using System.Collections.Generic;
using BSLib;
using Eto.Drawing;
using Eto.Forms;
using GKCommon;
using GKCore;
using GKCore.Interfaces;

namespace GKUI.Components
{
    using sdFontStyle = Eto.Drawing.FontStyle;

    public delegate void LinkEventHandler(object sender, string linkName);

    /// <summary>
    /// 
    /// </summary>
    public class HyperView : CustomPanel
    {
        private readonly List<BBTextChunk> fChunks;
        private readonly List<int> fHeights;
        private readonly StringList fLines;

        private bool fAcceptFontChange;
        private int fBorderWidth;
        private BBTextChunk fCurrentLink;
        private Color fLinkColor;
        private ExtSize fTextSize;

        public event LinkEventHandler OnLink;

        public int BorderWidth
        {
            get { return fBorderWidth; }
            set {
                if (fBorderWidth != value) {
                    fBorderWidth = value;
                    Invalidate();
                }
            }
        }

        public StringList Lines
        {
            get { return fLines; }
        }

        public Color LinkColor
        {
            get { return fLinkColor; }
            set {
                if (fLinkColor != value) {
                    fLinkColor = value;
                    Invalidate();
                }
            }
        }


        public HyperView() : base()
        {
            CenteredImage = false;

            fAcceptFontChange = true;
            fChunks = new List<BBTextChunk>();
            fCurrentLink = null;
            fHeights = new List<int>();
            fLines = new StringList();
            fLines.OnChange += LinesChanged;
            fLinkColor = Colors.Blue;
            fTextSize = ExtSize.Empty;
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                fChunks.Clear();
                fHeights.Clear();
                fLines.Dispose();
            }
            base.Dispose(disposing);
        }

        private void LinesChanged(object sender)
        {
            UpdateScrollPosition(0, 0);
            ArrangeText();
        }

        private void ArrangeText()
        {
            try
            {
                fAcceptFontChange = false;
                fHeights.Clear();

                try
                {
                    int xPos = 0;
                    int yPos = 0;
                    int xMax = 0;
                    int lineHeight = 0;

                    string text = fLines.Text.Trim();
                    if (!string.IsNullOrEmpty(text)) {
                        Font defFont = this.Font;
                        var parser = new BBTextParser(AppHost.GfxProvider, defFont.Size,
                                                      new ColorHandler(fLinkColor),
                                                      new ColorHandler(TextColor));

                        parser.ParseText(fChunks, text);

                        int line = -1;
                        int chunksCount = fChunks.Count;
                        for (int k = 0; k < chunksCount; k++)
                        {
                            BBTextChunk chunk = fChunks[k];

                            if (line != chunk.Line) {
                                line = chunk.Line;

                                if (line > 0) {
                                    yPos += lineHeight;
                                    fHeights.Add(lineHeight);
                                }

                                xPos = 0;
                                lineHeight = 0;
                            }

                            int prevX = xPos;
                            int prevY = yPos;

                            if (!string.IsNullOrEmpty(chunk.Text)) {
                                using (var font = new Font(defFont.FamilyName, chunk.Size, (sdFontStyle)chunk.Style)) {
                                    SizeF strSize = font.MeasureString(chunk.Text);
                                    chunk.Width = (int)strSize.Width;

                                    xPos += chunk.Width;
                                    if (xMax < xPos) xMax = xPos;

                                    int h = (int)strSize.Height;
                                    if (lineHeight < h) lineHeight = h;
                                }

                                if (!string.IsNullOrEmpty(chunk.URL)) {
                                    chunk.LinkRect = ExtRect.CreateBounds(prevX, prevY, xPos - prevX, lineHeight);
                                }
                            }
                        }
                    }

                    fTextSize = new ExtSize(xMax + 2 * fBorderWidth, yPos + 2 * fBorderWidth);
                }
                finally
                {
                    fAcceptFontChange = true;
                    SetImageSize(fTextSize);
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("HyperView.ArrangeText(): " + ex.Message);
            }
        }

        private void DoPaint(Graphics gfx)
        {
            try
            {
                fAcceptFontChange = false;
                try
                {
                    gfx.FillRectangle(new SolidBrush(BackgroundColor), Viewport);
                    Font defFont = this.Font;

                    int xOffset = fBorderWidth + ImageViewport.Left;
                    int yOffset = fBorderWidth + ImageViewport.Top;
                    int lineHeight = 0;

                    int line = -1;
                    int chunksCount = fChunks.Count;
                    for (int k = 0; k < chunksCount; k++)
                    {
                        BBTextChunk chunk = fChunks[k];

                        if (line != chunk.Line) {
                            line = chunk.Line;

                            xOffset = fBorderWidth + ImageViewport.Left;
                            yOffset += lineHeight;

                            // this condition is dirty hack
                            if (line >= 0 && line < fHeights.Count) {
                                lineHeight = fHeights[line];
                            }
                        }

                        string ct = chunk.Text;
                        if (!string.IsNullOrEmpty(ct)) {
                            // FIXME: null?!
                            IColor clr = chunk.Color;
                            var chunkColor = (clr == null) ? TextColor : ((ColorHandler)chunk.Color).Handle;
                            using (var brush = new SolidBrush(chunkColor)) {
                                using (var font = new Font(defFont.FamilyName, chunk.Size, (sdFontStyle)chunk.Style)) {
                                    gfx.DrawText(font, brush, xOffset, yOffset, ct);
                                }
                            }

                            xOffset += chunk.Width;
                        }
                    }
                }
                finally
                {
                    fAcceptFontChange = true;
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("HyperView.DoPaint(): " + ex.Message);
            }
        }

        private void DoLink(string linkName)
        {
            LinkEventHandler eventHandler = (LinkEventHandler)OnLink;
            if (eventHandler != null) eventHandler(this, linkName);
        }

        #region Protected methods

        protected override void OnFontChanged(EventArgs e)
        {
            if (fAcceptFontChange) {
                ArrangeText();
            }

            base.OnFontChanged(e);
        }

        // FIXME: move call to fCanvas handler
        protected override void OnKeyDown(KeyEventArgs e)
        {
            switch (e.Key)
            {
                case Keys.PageUp:
                    AdjustScroll(0, -LargeChange);
                    break;

                case Keys.PageDown:
                    AdjustScroll(0, LargeChange);
                    break;

                case Keys.Home:
                    AdjustScroll(-fTextSize.Width, -fTextSize.Height);
                    break;

                case Keys.End:
                    AdjustScroll(-fTextSize.Width, fTextSize.Height);
                    break;

                case Keys.Left:
                    AdjustScroll(-(e.Modifiers == Keys.None ? SmallChange : LargeChange), 0);
                    break;

                case Keys.Right:
                    AdjustScroll(e.Modifiers == Keys.None ? SmallChange : LargeChange, 0);
                    break;

                case Keys.Up:
                    AdjustScroll(0, -(e.Modifiers == Keys.None ? SmallChange : LargeChange));
                    break;

                case Keys.Down:
                    AdjustScroll(0, e.Modifiers == Keys.None ? SmallChange : LargeChange);
                    break;
            }

            e.Handled = true;
            base.OnKeyDown(e);
        }

        protected override void OnMouseDown(MouseEventArgs e)
        {
            if (fCurrentLink != null) DoLink(fCurrentLink.URL);

            e.Handled = true;
            base.OnMouseDown(e);
        }

        protected override void OnMouseMove(MouseEventArgs e)
        {
            Point mpt = GetImageRelativeLocation(e.Location);
            mpt.Offset(-fBorderWidth, -fBorderWidth);
            fCurrentLink = null;

            int num = fChunks.Count;
            for (int i = 0; i < num; i++)
            {
                BBTextChunk chunk = fChunks[i];
                if (string.IsNullOrEmpty(chunk.URL)) continue;

                if (chunk.HasCoord(mpt.X, mpt.Y))
                {
                    fCurrentLink = chunk;
                    break;
                }
            }

            Cursor = (fCurrentLink == null) ? Cursors.Default : Cursors.Pointer;

            e.Handled = true;
            base.OnMouseMove(e);
        }

        protected override void OnPaint(PaintEventArgs e)
        {
            DoPaint(e.Graphics);
        }

        #endregion
    }
}
