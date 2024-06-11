/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2011-2024 by Sergey V. Zhdanovskih.
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
using System.Windows.Forms;
using BSLib;
using GKCore;
using GKCore.BBText;
using GKCore.Design.Controls;
using GKUI.Platform.Handlers;

namespace GKUI.Components
{
    using SDFontStyle = System.Drawing.FontStyle;

    public delegate void LinkEventHandler(object sender, string linkName);

    /// <summary>
    ///
    /// </summary>
    public class HyperView : ScrollablePanel, IHyperView
    {
        private readonly List<BBTextChunk> fChunks;
        private readonly List<int> fHeights;
        private readonly StringList fLines;
        private readonly StringFormat fStrFormat;

        private bool fAcceptFontChange;
        private int fBorderWidth;
        private BBTextChunk fCurrentLink;
        private Color fLinkColor;
        private ExtSize fTextSize;
        private bool fWordWrap;

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

        public bool WordWrap
        {
            get { return fWordWrap; }
            set { fWordWrap = value; }
        }


        public HyperView()
        {
            SetStyle(ControlStyles.AllPaintingInWmPaint | ControlStyles.UserPaint |
                     ControlStyles.OptimizedDoubleBuffer | ControlStyles.ResizeRedraw, true);
            UpdateStyles();

            CenteredImage = false;
            BorderStyle = BorderStyle.Fixed3D;
            DoubleBuffered = true;
            TabStop = true;

            fAcceptFontChange = true;
            fChunks = new List<BBTextChunk>();
            fCurrentLink = null;
            fHeights = new List<int>();
            fLines = new StringList();
            fLines.OnChange += new NotifyEventHandler(LinesChanged);
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
            UpdateScrollPosition(0, 0);
            ArrangeText();
        }

        private void ArrangeText()
        {
            if (fLines.Count == 0)
                return;

            float maxWidth = this.ClientSize.Width - (2 * fBorderWidth);
            if (maxWidth < 0)
                return;

            try {
                SuspendLayout();

                fAcceptFontChange = false;
                fHeights.Clear();

                Graphics gfx = CreateGraphics();
                //gfx.TextRenderingHint = TextRenderingHint.AntiAlias;
                try {
                    int xPos = 0;
                    int yPos = 0;
                    int xMax = 0;
                    int lineHeight = 0;
                    Font defFont = this.Font;
                    SizeF zerosz = new SizeF(0f, 0f);

                    var parser = new BBTextParser(AppHost.GfxProvider, defFont.SizeInPoints,
                                                  new ColorHandler(fLinkColor), new ColorHandler(ForeColor));

                    string text = fLines.Text;
                    text = SysUtils.StripHTML(text);
                    parser.ParseText(fChunks, text);

                    int line = -1;
                    int chunksCount = fChunks.Count;
                    int k = 0;
                    while (k < chunksCount) {
                        BBTextChunk chunk = fChunks[k];
                        bool recalcChunk = false;

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

                        string chunkStr = chunk.Text;
                        if (!string.IsNullOrEmpty(chunkStr)) {
                            using (var font = new Font(defFont.Name, chunk.Size, (SDFontStyle)chunk.Style, defFont.Unit)) {
                                SizeF strSize = gfx.MeasureString(chunkStr, font, zerosz, fStrFormat);

                                if (fWordWrap && xPos + strSize.Width > maxWidth) {
                                    int lastPos = 0, prevPos = 0;
                                    string tempStr, prevStr = string.Empty;
                                    int sliceType = -1;
                                    while (true) {
                                        tempStr = GetSlice(chunkStr, ref lastPos, ref sliceType);
                                        strSize = gfx.MeasureString(tempStr, font, zerosz, fStrFormat);
                                        if (xPos + strSize.Width <= maxWidth) {
                                            prevStr = tempStr;
                                            prevPos = lastPos;
                                        } else {
                                            if (sliceType == 0) {
                                                // first word
                                                if (xPos == 0) {
                                                    string tail = chunkStr.Substring(lastPos);
                                                    SplitChunk(chunk, k, tempStr, tail, ref chunksCount);
                                                } else {
                                                    ShiftChunks(k, chunksCount);
                                                    recalcChunk = true;
                                                }
                                                break;
                                            } else if (sliceType == 1 || sliceType == 2) {
                                                // middle or tail word
                                                string tail = chunkStr.Substring(prevPos);
                                                SplitChunk(chunk, k, prevStr, tail, ref chunksCount);
                                                break;
                                            } else if (sliceType == 3) {
                                                // one first and last word, nothing to do
                                                break;
                                            }
                                        }
                                    }
                                }

                                strSize = gfx.MeasureString(chunk.Text, font, zerosz, fStrFormat);
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

                        if (!recalcChunk) {
                            k++;
                        }
                    }

                    fTextSize = new ExtSize(xMax + 2 * fBorderWidth, yPos + 2 * fBorderWidth);
                } finally {
                    gfx.Dispose();
                    fAcceptFontChange = true;
                    SetImageSize(fTextSize);

                    ResumeLayout(true);
                }
            } catch (Exception ex) {
                Logger.WriteError("HyperView.ArrangeText()", ex);
            }
        }

        private static string GetSlice(string str, ref int lastPos, ref int type)
        {
            // type: -1 initial none, 0 first word, 1 any middle, 2 last word, 3 only one word in str
            int pos = str.IndexOf(' ', lastPos);
            string result;
            if (pos >= 0) {
                result = str.Substring(0, pos);
                lastPos = pos + 1;
                type = (type == -1) ? 0 : 1;
            } else {
                result = str;
                if (lastPos > 0) {
                    lastPos = -1;
                    type = 2;
                } else {
                    lastPos = -1;
                    type = 3;
                }
            }
            return result;
        }

        private void SplitChunk(BBTextChunk chunk, int index, string head, string tail, ref int chunksCount)
        {
            chunk.Text = head;

            if (!string.IsNullOrEmpty(tail)) {
                var newChunk = chunk.Clone();
                newChunk.Text = tail;
                fChunks.Insert(index + 1, newChunk);
                chunksCount += 1;

                ShiftChunks(index + 1, chunksCount);
            }
        }

        private void ShiftChunks(int startIndex, int chunksCount)
        {
            for (int m = startIndex; m < chunksCount; m++) {
                fChunks[m].Line += 1;
            }
        }

        private void DoPaint(Graphics gfx)
        {
            try {
                //gfx.TextRenderingHint = TextRenderingHint.AntiAlias;
                fAcceptFontChange = false;
                SolidBrush brush = new SolidBrush(this.ForeColor);
                Font font = null;
                try {
                    Rectangle clientRect = ClientRectangle;
                    gfx.FillRectangle(new SolidBrush(BackColor), clientRect);

                    var scrollPos = AutoScrollPosition;
                    int xOffset = fBorderWidth + scrollPos.X;
                    int yOffset = fBorderWidth + scrollPos.Y;
                    int lineHeight = 0;

                    int line = -1;
                    int chunksCount = fChunks.Count;
                    for (int k = 0; k < chunksCount; k++) {
                        BBTextChunk chunk = fChunks[k];

                        if (line != chunk.Line) {
                            line = chunk.Line;

                            xOffset = fBorderWidth + scrollPos.X;
                            yOffset += lineHeight;

                            // this condition is dirty hack
                            if (line >= 0 && line < fHeights.Count) {
                                lineHeight = fHeights[line];
                            }
                        }

                        string ct = chunk.Text;
                        if (!string.IsNullOrEmpty(ct)) {
                            Color chunkColor = (chunk.Color == null) ? ForeColor : ((ColorHandler)chunk.Color).Handle;
                            brush.Color = chunkColor;
                            font = ProcessFont(font, chunk.Size, (SDFontStyle)chunk.Style);
                            gfx.DrawString(ct, font, brush, xOffset, yOffset, fStrFormat);

                            xOffset += chunk.Width;
                        }
                    }
                } finally {
                    fAcceptFontChange = true;
                    if (brush != null) brush.Dispose();
                    if (font != null) font.Dispose();
                }
            } catch (Exception ex) {
                Logger.WriteError("HyperView.DoPaint()", ex);
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
            var eventHandler = OnLink;
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
            if (fAcceptFontChange) {
                ArrangeText();
            }

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

            Point mpt = GetImageRelativeLocation(e.Location);
            mpt.Offset(-fBorderWidth, -fBorderWidth);
            fCurrentLink = null;

            int num = fChunks.Count;
            for (int i = 0; i < num; i++) {
                BBTextChunk chunk = fChunks[i];
                if (string.IsNullOrEmpty(chunk.URL)) continue;

                if (chunk.HasCoord(mpt.X, mpt.Y)) {
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
