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
using System.Drawing;
using System.Drawing.Text;
using System.Windows.Forms;
using BSLib;
using GKCore;
using GKCore.MVP.Controls;

namespace GKUI.Components
{
    using sdFontStyle = System.Drawing.FontStyle;

    public delegate void LinkEventHandler(object sender, string linkName);

    /// <summary>
    /// 
    /// </summary>
    public class HyperView : ScrollablePanel, IHyperView
    {
        private readonly List<BBTextChunk> fChunks;
        private readonly List<int> fHeights;
        private readonly StringList fLines;

        private bool fAcceptFontChange;
        private int fBorderWidth;
        private ExtSize fTextSize;
        private BBTextChunk fCurrentLink;
        private Color fLinkColor;
        private StringFormat fStrFormat;
        private bool fWordWrap;

        private static readonly object EventLink;

        static HyperView()
        {
            EventLink = new object();
        }

        public event LinkEventHandler OnLink
        {
            add { Events.AddHandler(EventLink, value); }
            remove { Events.RemoveHandler(EventLink, value); }
        }

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

        public bool WordWrap
        {
            get { return fWordWrap; }
            set { fWordWrap = value; }
        }


        public HyperView() : base()
        {
            SetStyle(ControlStyles.AllPaintingInWmPaint | ControlStyles.UserPaint |
                     ControlStyles.OptimizedDoubleBuffer | ControlStyles.ResizeRedraw, true);
            UpdateStyles();

            BorderStyle = BorderStyle.Fixed3D;
            DoubleBuffered = true;
            TabStop = true;

            fAcceptFontChange = true;
            fChunks = new List<BBTextChunk>();
            fCurrentLink = null;
            fHeights = new List<int>();
            fLines = new StringList();
            fLines.OnChange += LinesChanged;
            fLinkColor = Color.Blue;
            fTextSize = ExtSize.Empty;
            fStrFormat = new StringFormat(StringFormatFlags.MeasureTrailingSpaces | StringFormatFlags.NoWrap | StringFormatFlags.NoClip);
            fWordWrap = true;
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                fChunks.Clear();
                fHeights.Clear();
                fLines.Dispose();
                fStrFormat.Dispose();
            }
            base.Dispose(disposing);
        }

        public void Activate()
        {
            Select();
        }

        private void LinesChanged(object sender)
        {
            AutoScrollPosition = new Point(0, 0);
            ArrangeText();
        }

        private void ArrangeText()
        {
            try {
                fAcceptFontChange = false;
                fHeights.Clear();

                Graphics gfx = CreateGraphics();
                gfx.TextRenderingHint = TextRenderingHint.AntiAlias;
                try {
                    int xPos = 0;
                    int yPos = 0;
                    int xMax = 0;
                    int lineHeight = 0;

                    string text = fLines.Text;
                    Font defFont = this.Font;
                    SizeF csz = this.ClientSize;
                    SizeF zerosz = new SizeF(0f, 0f);

                    var parser = new BBTextParser(AppHost.GfxProvider, defFont.SizeInPoints,
                                                  new ColorHandler(fLinkColor), new ColorHandler(ForeColor));

                    parser.ParseText(fChunks, text);

                    int line = -1;
                    int chunksCount = fChunks.Count;
                    int k = 0;
                    while (k < chunksCount) {
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

                        string chunkStr = chunk.Text;
                        if (!string.IsNullOrEmpty(chunkStr)) {
                            using (var font = new Font(defFont.Name, chunk.Size, (sdFontStyle)chunk.Style, defFont.Unit)) {
                                SizeF strSize = gfx.MeasureString(chunkStr, font, zerosz, fStrFormat);

                                if (fWordWrap) {
                                    int wBound = xPos + 2 * fBorderWidth;
                                    if (wBound + strSize.Width > csz.Width) {
                                        int lastIndex = chunkStr.Length - 1;
                                        while (true) {
                                            int spPos = chunkStr.LastIndexOf(' ', lastIndex);
                                            if (spPos <= 0) {
                                                chunk.Text = chunkStr.Substring(0, lastIndex + 1);
                                                strSize = gfx.MeasureString(chunkStr, font, zerosz, fStrFormat);
                                                break;
                                            }

                                            string newChunk = chunkStr.Substring(0, spPos);
                                            strSize = gfx.MeasureString(newChunk, font, zerosz, fStrFormat);
                                            if (wBound + strSize.Width < csz.Width) {
                                                var secondPart = chunk.Clone();
                                                secondPart.Text = chunkStr.Substring(spPos + 1);
                                                secondPart.Line += 1;
                                                fChunks.Insert(k + 1, secondPart);
                                                chunksCount += 1;

                                                // shift next chunks
                                                for (int m = k + 2; m < chunksCount; m++) {
                                                    BBTextChunk mChunk = fChunks[m];
                                                    mChunk.Line += 1;
                                                }

                                                chunk.Text = newChunk;
                                                break;
                                            } else {
                                                lastIndex = spPos - 1;
                                            }
                                        }
                                    }
                                }

                                chunk.Width = (int)strSize.Width;

                                xPos += chunk.Width;
                                if (xMax < xPos) xMax = xPos;

                                int h = (int)strSize.Height;
                                if (lineHeight < h) lineHeight = h;
                            }
                        }

                        k++;
                    }

                    fTextSize = new ExtSize(xMax + 2 * fBorderWidth, yPos + 2 * fBorderWidth);
                } finally {
                    gfx.Dispose();
                    fAcceptFontChange = true;
                    AdjustViewport(fTextSize);
                }
            } catch (Exception ex) {
                Logger.LogWrite("HyperView.ArrangeText(): " + ex.Message);
            }
        }

        private void DoPaint(Graphics gfx)
        {
            try {
                gfx.TextRenderingHint = TextRenderingHint.AntiAlias;
                fAcceptFontChange = false;
                SolidBrush brush = new SolidBrush(this.ForeColor);
                Font font = null;
                try {
                    Rectangle clientRect = ClientRectangle;
                    gfx.FillRectangle(new SolidBrush(BackColor), clientRect);

                    int xOffset = fBorderWidth - -AutoScrollPosition.X;
                    int yOffset = fBorderWidth - -AutoScrollPosition.Y;
                    int lineHeight = 0;

                    int line = -1;
                    int chunksCount = fChunks.Count;
                    for (int k = 0; k < chunksCount; k++) {
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
                            brush.Color = ((ColorHandler)chunk.Color).Handle;
                            font = ProcessFont(font, chunk.Size, (sdFontStyle)chunk.Style);
                            gfx.DrawString(ct, font, brush, xOffset, yOffset, fStrFormat);

                            xOffset += chunk.Width;

                            if (!string.IsNullOrEmpty(chunk.URL)) {
                                chunk.LinkRect = ExtRect.CreateBounds(prevX, prevY, xOffset - prevX, lineHeight);
                            }
                        }
                    }
                } finally {
                    fAcceptFontChange = true;
                    if (brush != null) brush.Dispose();
                    if (font != null) font.Dispose();
                }
            } catch (Exception ex) {
                Logger.LogWrite("HyperView.DoPaint(): " + ex.Message);
            }
        }

        private Font ProcessFont(Font prevFont, float emSize, FontStyle style)
        {
            Font result;
            if (prevFont == null) {
                var defFont = this.Font;
                result = new Font(defFont.Name, emSize, style, defFont.Unit);
            } else {
                if (prevFont.Size == emSize && prevFont.Style == style) {
                    result = prevFont;
                } else {
                    result = new Font(prevFont.Name, emSize, style, prevFont.Unit);
                    prevFont.Dispose();
                }
            }
            return result;
        }

        private void DoLink(string linkName)
        {
            LinkEventHandler eventHandler = (LinkEventHandler)Events[EventLink];
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

        protected override void OnResize(EventArgs e)
        {
            ArrangeText();

            base.OnResize(e);
        }

        protected override void OnKeyDown(KeyEventArgs e)
        {
            switch (e.KeyCode)
            {
                case Keys.Prior:
                    AdjustScroll(0, -VerticalScroll.LargeChange);
                    break;

                case Keys.Next:
                    AdjustScroll(0, VerticalScroll.LargeChange);
                    break;

                case Keys.Home:
                    AdjustScroll(-HorizontalScroll.Maximum, -VerticalScroll.Maximum);
                    break;

                case Keys.End:
                    AdjustScroll(-HorizontalScroll.Maximum, VerticalScroll.Maximum);
                    break;

                case Keys.Left:
                    AdjustScroll(-(e.Modifiers == Keys.None ? HorizontalScroll.SmallChange : HorizontalScroll.LargeChange), 0);
                    break;

                case Keys.Right:
                    AdjustScroll(e.Modifiers == Keys.None ? HorizontalScroll.SmallChange : HorizontalScroll.LargeChange, 0);
                    break;

                case Keys.Up:
                    AdjustScroll(0, -(e.Modifiers == Keys.None ? VerticalScroll.SmallChange : VerticalScroll.LargeChange));
                    break;

                case Keys.Down:
                    AdjustScroll(0, e.Modifiers == Keys.None ? VerticalScroll.SmallChange : VerticalScroll.LargeChange);
                    break;
            }

            base.OnKeyDown(e);
        }

        protected override bool IsInputKey(Keys keyData)
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
        }

        protected override void OnMouseDown(MouseEventArgs e)
        {
            base.OnMouseDown(e);

            if (fCurrentLink != null) DoLink(fCurrentLink.URL);
        }

        protected override void OnMouseMove(MouseEventArgs e)
        {
            base.OnMouseMove(e);

            int xOffset = (fBorderWidth - -AutoScrollPosition.X);
            int yOffset = (fBorderWidth - -AutoScrollPosition.Y);
            fCurrentLink = null;

            int num = fChunks.Count;
            for (int i = 0; i < num; i++)
            {
                BBTextChunk chunk = fChunks[i];
                if (string.IsNullOrEmpty(chunk.URL)) continue;

                if (chunk.HasCoord(e.X, e.Y, xOffset, yOffset))
                {
                    fCurrentLink = chunk;
                    break;
                }
            }

            Cursor = (fCurrentLink == null) ? Cursors.Default : Cursors.Hand;
        }

        protected override void OnPaint(PaintEventArgs e)
        {
            DoPaint(e.Graphics);
            base.OnPaint(e);
        }

        #endregion
    }
}
