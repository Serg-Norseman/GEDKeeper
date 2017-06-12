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
using Eto.Drawing;
using Eto.Forms;

using GKCommon;
using GKCore;

namespace GKUI.Components
{
    using sdFontStyle = Eto.Drawing.FontStyle;

    public delegate void LinkEventHandler(object sender, string linkName);

    /// <summary>
    /// 
    /// </summary>
    public class HyperView : ScrollablePanel
    {
        private bool fAcceptFontChange;
        private int fBorderWidth;
        private List<int> fHeights;
        private ExtSize fTextSize;
        private readonly StringList fLines;
        private BBTextChunk fCurrentLink;
        private Color fLinkColor;
        private List<BBTextChunk> fChunks;
        private int fHorizontalMaximum;
        private int fVerticalMaximum;

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
            // FIXME: GKv3 DevRestriction
            /*SetStyle(ControlStyles.AllPaintingInWmPaint | ControlStyles.UserPaint |
                     ControlStyles.OptimizedDoubleBuffer | ControlStyles.ResizeRedraw, true);
            UpdateStyles();

            BorderStyle = BorderStyle.Fixed3D;
            DoubleBuffered = true;
            TabStop = true;*/

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
            AutoScrollPosition = new Point(0, 0);
            ArrangeText();
        }

        private void ArrangeText()
        {
            try
            {
                fAcceptFontChange = false;
                fHeights.Clear();

                Graphics gfx = CreateGraphics();
                try
                {
                    int xPos = 0;
                    int yPos = 0;
                    int xMax = 0;
                    int lineHeight = 0;

                    string text = fLines.Text;
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

                        if (!string.IsNullOrEmpty(chunk.Text)) {
                            using (var font = new Font(defFont.FamilyName, chunk.Size, (sdFontStyle)chunk.Style)) {
                                SizeF strSize = gfx.MeasureString(font, chunk.Text);
                                chunk.Width = (int)strSize.Width;

                                xPos += chunk.Width;
                                if (xMax < xPos) xMax = xPos;

                                int h = (int)strSize.Height;
                                if (lineHeight < h) lineHeight = h;
                            }
                        }
                    }

                    fTextSize = new ExtSize(xMax + 2 * fBorderWidth, yPos + 2 * fBorderWidth);
                }
                finally
                {
                    gfx.Dispose();
                    fAcceptFontChange = true;
                    AdjustViewPort(fTextSize);
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
                    Rectangle clientRect = Bounds; //ClientRectangle;
                    gfx.FillRectangle(new SolidBrush(BackgroundColor), clientRect);
                    Font defFont = this.Font;

                    int xOffset = fBorderWidth - -AutoScrollPosition.X;
                    int yOffset = fBorderWidth - -AutoScrollPosition.Y;
                    int lineHeight = 0;

                    int line = -1;
                    int chunksCount = fChunks.Count;
                    for (int k = 0; k < chunksCount; k++)
                    {
                        BBTextChunk chunk = fChunks[k];

                        if (line != chunk.Line) {
                            line = chunk.Line;

                            xOffset = fBorderWidth - -AutoScrollPosition.X;
                            yOffset += lineHeight;

                            // this condition is dirty hack
                            if (line >= 0 && line < fHeights.Count) {
                                lineHeight = fHeights[line];
                            }
                        }

                        int prevX = xOffset;
                        int prevY = yOffset;

                        string ct = chunk.Text;
                        if (!string.IsNullOrEmpty(ct)) {
                            var chunkColor = ((ColorHandler)chunk.Color).Handle;
                            using (var brush = new SolidBrush(chunkColor)) {
                                using (var font = new Font(defFont.FamilyName, chunk.Size, (sdFontStyle)chunk.Style)) {
                                    gfx.DrawText(font, brush, xOffset, yOffset, ct);
                                }
                            }

                            xOffset += chunk.Width;

                            if (!string.IsNullOrEmpty(chunk.URL)) {
                                chunk.LinkRect = ExtRect.CreateBounds(prevX, prevY, xOffset - prevX, lineHeight);
                            }
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

        // FIXME: GKv3 DevRestriction
        /*protected override void OnFontChanged(EventArgs e)
        {
            if (fAcceptFontChange) {
                ArrangeText();
            }

            base.OnFontChanged(e);
        }*/

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
                    AdjustScroll(-fHorizontalMaximum, -fVerticalMaximum);
                    break;

                case Keys.End:
                    AdjustScroll(-fHorizontalMaximum, fVerticalMaximum);
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

            base.OnKeyDown(e);
        }

        // FIXME: GKv3 DevRestriction
        /*protected override bool IsInputKey(Keys keyData)
        {
            bool result;

            if ((keyData & Keys.Right) == Keys.Right || (keyData & Keys.Left) == Keys.Left ||
                (keyData & Keys.Up) == Keys.Up || (keyData & Keys.Down) == Keys.Down ||
                (keyData & Keys.Prior) == Keys.Prior || (keyData & Keys.Next) == Keys.Next ||
                (keyData & Keys.End) == Keys.End || (keyData & Keys.Home) == Keys.Home)
                result = true;
            else
                result = base.IsInputKey(keyData);

            return result;
        }*/

        protected override void OnMouseDown(MouseEventArgs e)
        {
            base.OnMouseDown(e);

            if (fCurrentLink != null) DoLink(fCurrentLink.URL);
        }

        protected override void OnMouseMove(MouseEventArgs e)
        {
            base.OnMouseMove(e);

            Point mpt = new Point(e.Location);
            int xOffset = (fBorderWidth - -AutoScrollPosition.X);
            int yOffset = (fBorderWidth - -AutoScrollPosition.Y);
            fCurrentLink = null;

            int num = fChunks.Count;
            for (int i = 0; i < num; i++)
            {
                BBTextChunk chunk = fChunks[i];
                if (string.IsNullOrEmpty(chunk.URL)) continue;

                if (chunk.HasCoord(mpt.X, mpt.Y, xOffset, yOffset))
                {
                    fCurrentLink = chunk;
                    break;
                }
            }

            Cursor = (fCurrentLink == null) ? Cursors.Default : Cursors.Pointer;
        }

        protected override void OnPaint(PaintEventArgs e)
        {
            DoPaint(e.Graphics);
            base.OnPaint(e);
        }

        #endregion
    }
}
