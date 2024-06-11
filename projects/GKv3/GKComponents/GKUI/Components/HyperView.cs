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
using System.Runtime.InteropServices;
using BSLib;
using Eto.Drawing;
using Eto.Forms;
using GKCore;
using GKCore.BBText;
using GKCore.Design;
using GKCore.Design.Controls;
using GKUI.Platform.Handlers;

namespace GKUI.Components
{
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
            CenteredImage = false;

            if (RuntimeInformation.IsOSPlatform(OSPlatform.Linux)) {
                BackgroundColor = SystemColors.WindowBackground;
            } else if (RuntimeInformation.IsOSPlatform(OSPlatform.Windows)) {
                BackgroundColor = SystemColors.Control;
            }

            fAcceptFontChange = true;
            fChunks = new List<BBTextChunk>();
            fCurrentLink = null;
            fHeights = new List<int>();
            fLines = new StringList();
            fLines.OnChange += new NotifyEventHandler(LinesChanged);
            fLinkColor = SystemColors.LinkText;
            fTextSize = ExtSize.Empty;
            fWordWrap = true;
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                fChunks.Clear();
                fHeights.Clear();
                fLines.Dispose();
            }
            base.Dispose(disposing);
        }

        public void Activate()
        {
            Focus();
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
                //SuspendLayout();

                fAcceptFontChange = false;
                fHeights.Clear();

                try {
                    int xPos = 0;
                    int yPos = 0;
                    int xMax = 0;
                    int lineHeight = 0;
                    Font defFont = this.Font;

                    var parser = new BBTextParser(AppHost.GfxProvider, defFont.Size,
                                                  new ColorHandler(fLinkColor), new ColorHandler(TextColor));

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

                            var font = new Font(defFont.FamilyName, chunk.Size, GetFontStyle(chunk.Style), GetFontDecoration(chunk.Style));

                            SizeF strSize = font.MeasureString(chunkStr);

                            if (fWordWrap && xPos + strSize.Width > maxWidth) {
                                int lastPos = 0, prevPos = 0;
                                string tempStr, prevStr = string.Empty;
                                int sliceType = -1;
                                while (true) {
                                    tempStr = GetSlice(chunkStr, ref lastPos, ref sliceType);
                                    strSize = font.MeasureString(tempStr);
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

                            strSize = font.MeasureString(chunk.Text);
                            chunk.Width = (int)strSize.Width;

                            xPos += chunk.Width;
                            if (xMax < xPos) xMax = xPos;

                            int h = (int)strSize.Height;
                            if (lineHeight < h) lineHeight = h;

                            if (!string.IsNullOrEmpty(chunk.URL)) {
                                chunk.LinkRect = ExtRect.CreateBounds(prevX, prevY, xPos - prevX, lineHeight);
                            }
                        }

                        if (!recalcChunk) {
                            k++;
                        }
                    }

                    fTextSize = new ExtSize(xMax + 2 * fBorderWidth, yPos + 2 * fBorderWidth);
                    SetImageSize(fTextSize);
                } finally {
                    //ResumeLayout();

                    fAcceptFontChange = true;

                    InvalidateContent();
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
                fAcceptFontChange = false;
                SolidBrush brush = new SolidBrush(this.TextColor);
                Font font = null;
                try {
                    gfx.FillRectangle(new SolidBrush(BackgroundColor), UIHelper.Rt2Rt(Viewport));

                    var scrollPos = ImageViewport;
                    int xOffset = fBorderWidth + scrollPos.Left;
                    int yOffset = fBorderWidth + scrollPos.Top;
                    int lineHeight = 0;

                    int line = -1;
                    int chunksCount = fChunks.Count;
                    for (int k = 0; k < chunksCount; k++) {
                        BBTextChunk chunk = fChunks[k];

                        if (line != chunk.Line) {
                            line = chunk.Line;

                            xOffset = fBorderWidth + scrollPos.Left;
                            yOffset += lineHeight;

                            // this condition is dirty hack
                            if (line >= 0 && line < fHeights.Count) {
                                lineHeight = fHeights[line];
                            }
                        }

                        string ct = chunk.Text;
                        if (!string.IsNullOrEmpty(ct)) {
                            Color chunkColor = (chunk.Color == null) ? TextColor : ((ColorHandler)chunk.Color).Handle;
                            if (brush != null) brush.Dispose();
                            brush = new SolidBrush(chunkColor);
                            font = ProcessFont(font, chunk.Size, GetFontStyle(chunk.Style), GetFontDecoration(chunk.Style));
                            gfx.DrawText(font, brush, xOffset, yOffset, ct);

                            xOffset += chunk.Width;
                        }
                    }
                } finally {
                    fAcceptFontChange = true;
                    if (brush != null) brush.Dispose();
                }
            } catch (Exception ex) {
                Logger.WriteError("HyperView.DoPaint()", ex);
            }
        }

        private Font ProcessFont(Font prevFont, float emSize, FontStyle style, FontDecoration decoration)
        {
            Font result;
            if (prevFont == null) {
                var defFont = this.Font;
                result = new Font(defFont.FamilyName, emSize, style, decoration);
            } else {
                if (prevFont.Size == emSize && prevFont.FontStyle == style && prevFont.FontDecoration == decoration) {
                    result = prevFont;
                } else {
                    result = new Font(prevFont.FamilyName, emSize, style, decoration);
                }
            }

            return result;
        }

        private static FontDecoration GetFontDecoration(BSDTypes.FontStyle style)
        {
            if ((style & BSDTypes.FontStyle.Underline) == BSDTypes.FontStyle.Underline) {
                return FontDecoration.Underline;
            }

            if ((style & BSDTypes.FontStyle.Strikeout) == BSDTypes.FontStyle.Strikeout) {
                return FontDecoration.Strikethrough;
            }

            return FontDecoration.None;
        }

        private static FontStyle GetFontStyle(BSDTypes.FontStyle style)
        {
            if ((style & BSDTypes.FontStyle.Bold) == BSDTypes.FontStyle.Bold) {
                return FontStyle.Bold;
            }

            if ((style & BSDTypes.FontStyle.Italic) == BSDTypes.FontStyle.Italic) {
                return FontStyle.Italic;
            }

            return FontStyle.None;
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

        protected override void OnSizeChanged(EventArgs e)
        {
            if (fAcceptFontChange) {
                ArrangeText();
            }

            base.OnSizeChanged(e);
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
            Point mpt = GetImageRelativeLocation(e.Location, e.Buttons != MouseButtons.None);
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
