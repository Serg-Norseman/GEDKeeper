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
using System.Windows.Forms;

namespace GKCommon.Controls
{
    public delegate void LinkEventHandler(object sender, string linkName);

    public enum RuleStyle
    {
        rsLowered,
        rsRaised
    }

    /// <summary>
    /// 
    /// </summary>
    public class HyperView : GKScrollableControl
    {
        private sealed class TextChunk : BaseObject
        {
            public int Line;
            public Font Font;
            public string Text;
            public Color Color;

            public string URL;
            public ExtRect LinkRect;

            public TextChunk(int line)
            {
                Line = line;
                Font = null;
                Text = string.Empty;
                Color = Color.Black;
                URL = string.Empty;
            }

            public TextChunk(int line, Font font)
            {
                Line = line;
                Font = font;
                Text = string.Empty;
                Color = Color.Black;
                URL = string.Empty;
            }

            protected override void Dispose(bool disposing)
            {
                if (disposing) {
                    if (Font != null) {
                        Font.Dispose();
                        Font = null;
                    }
                }
                base.Dispose(disposing);
            }

            public bool HasCoord(int x, int y, int xOffset, int yOffset)
            {
                return x >= LinkRect.Left + xOffset && x <= LinkRect.Right + xOffset
                    && y >= LinkRect.Top + yOffset && y <= LinkRect.Bottom + yOffset;
            }
        }

        private bool fAcceptFontChange;
        private int fBorderWidth;
        private Color fColor;
        private SolidBrush fDefBrush;
        private int[] fHeights;
        private Size fTextSize;
        private readonly StringList fLines;
        private TextChunk fCurrentLink;
        private Color fLinkColor;
        private Font fTextFont;
        private RuleStyle fRuleStyle;
        private List<TextChunk> fChunks;

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
                if (fBorderWidth == value) return;

                fBorderWidth = value;
                Invalidate();
            }
        }

        public Color Color
        {
            get { return fColor; }
            set {
                if (fColor == value) return;

                fColor = value;
                Invalidate();
            }
        }

        public StringList Lines
        {
            get { return fLines; }
            set { fLines.Assign(value); }
        }

        public Color LinkColor
        {
            get { return fLinkColor; }
            set { fLinkColor = value; }
        }

        public RuleStyle RuleStyle
        {
            get { return fRuleStyle; }
            set {
                if (fRuleStyle == value) return;

                fRuleStyle = value;
                Invalidate();
            }
        }


        public HyperView() : base()
        {
            SetStyle(ControlStyles.AllPaintingInWmPaint | ControlStyles.UserPaint | ControlStyles.OptimizedDoubleBuffer | ControlStyles.ResizeRedraw, true);
            UpdateStyles();

            BorderStyle = BorderStyle.Fixed3D;
            DoubleBuffered = true;
            TabStop = true;

            fAcceptFontChange = true;
            fChunks = new List<TextChunk>();
            fColor = SystemColors.Control;
            fCurrentLink = null;
            fHeights = new int[0];
            fLines = new StringList();
            fLines.OnChange += LinesChanged;
            fLinkColor = Color.Blue;
            fTextSize = Size.Empty;
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                fChunks.Clear();
                fHeights = null;
                fLines.Dispose();

                if (fDefBrush != null) fDefBrush.Dispose();
                if (fTextFont != null) fTextFont.Dispose();
            }
            base.Dispose(disposing);
        }

        private void LinesChanged(object sender)
        {
            AutoScrollPosition = new Point(0, 0);
            ArrangeText();
        }

        private void MeasureText(Graphics grx, string ss, Font font, ref int xPos, ref int yPos, ref int hMax, ref int xMax)
        {
            if (yPos >= -hMax && ss != "") {
                SizeF strSize = grx.MeasureString(ss, font);
                xPos += (int)strSize.Width;

                if (xPos > xMax) xMax = xPos;
                int h = (int)strSize.Height;
                if (h > hMax) hMax = h;
            }
        }

        private void OutText(Graphics gfx, string ss, Font font, Color color, ref int xPos, ref int yPos, ref int hMax)
        {
            if (yPos >= -hMax && ss != "") {
                using (var brush = new SolidBrush(color)) {
                    gfx.DrawString(ss, font, brush, xPos, yPos);
                }

                SizeF strSize = gfx.MeasureString(ss, font);
                xPos += (int)strSize.Width;
            }
        }

        private void SetChunkColor(int line, ref TextChunk chunk, Font font, Color color)
        {
            if (chunk == null || chunk.Text.Length != 0) {
                chunk = new TextChunk(line, font);
                fChunks.Add(chunk);
            }

            chunk.Font = font;
            chunk.Color = color;
        }

        private void SetChunkFont(int line, ref TextChunk chunk, Font font)
        {
            if (chunk == null || chunk.Text.Length != 0) {
                chunk = new TextChunk(line, font);
                fChunks.Add(chunk);
            }

            chunk.Font = font;
        }

        private void SetChunkText(int line, ref TextChunk chunk, Font font, string text)
        {
            if (chunk == null) {
                chunk = new TextChunk(line, font);
                fChunks.Add(chunk);
            }

            chunk.Text += text;
        }

        private static Font SetFontSize(Font font, float size)
        {
            return new Font(font.Name, size, font.Style, font.Unit, font.GdiCharSet, font.GdiVerticalFont);
        }

        private static Font SetFontStyle(Font font, FontStyle style)
        {
            FontStyle fontStyle = font.Style;
            if ((fontStyle & style) == FontStyle.Regular) {
                fontStyle |= style;
            } else {
                fontStyle &= ~style;
            }
            return new Font(font, fontStyle);
        }

        private void ParseLine(int line, string lineText, Font currentFont)
        {
            TextChunk lastChunk = null;
            Font lastFont = (Font)currentFont.Clone();

            if (string.IsNullOrEmpty(lineText)) {
                lineText = " ";
                SetChunkText(line, ref lastChunk, currentFont, lineText);
                return;
            }

            StringTokenizer strTok = new StringTokenizer(lineText);
            strTok.IgnoreWhiteSpace = false;
            strTok.RecognizeDecimals = false;

            Token tok = strTok.Next();
            while (tok.Kind != TokenKind.EOF) {
                if (tok.Kind == TokenKind.Symbol && tok.Value == "[") {
                    string temp = tok.Value;
                    tok = strTok.Next();

                    bool closeTag;
                    // closed tag
                    if (tok.Kind == TokenKind.Symbol && tok.Value == "/") {
                        closeTag = true;
                        temp += tok.Value;
                        tok = strTok.Next();
                    } else {
                        closeTag = false;
                    }

                    if (tok.Kind != TokenKind.Word) {
                        // not tag
                        SetChunkText(line, ref lastChunk, lastFont, temp + tok.Value);
                    } else {
                        string tag = tok.Value;
                        bool skipTag = false;

                        if (tag == "size") {
                            // [size={+/-x}]
                            tok = strTok.Next();
                            if (tok.Kind == TokenKind.Symbol && tok.Value == "=") {
                                tok = strTok.Next();
                                int factor = 0;
                                if (tok.Kind == TokenKind.Symbol) {
                                    if (tok.Value == "+") {
                                        factor = +1;
                                    } else if (tok.Value == "-") {
                                        factor = -1;
                                    }
                                    tok = strTok.Next();
                                }
                                if (tok.Kind == TokenKind.Number) {
                                    lastFont = SetFontSize(lastFont, (lastFont.Size + factor * SysUtils.ParseInt(tok.Value, 0)));
                                    tok = strTok.Next();
                                }
                            }
                            SetChunkFont(line, ref lastChunk, lastFont);
                            skipTag = true;
                        }
                        else if (tag == "b") {
                            // [b][/b], but now it's switcher
                            lastFont = SetFontStyle(lastFont, FontStyle.Bold);
                            SetChunkFont(line, ref lastChunk, lastFont);
                        }
                        else if (tag == "i") {
                            // [i][/i], but now it's switcher
                            lastFont = SetFontStyle(lastFont, FontStyle.Italic);
                            SetChunkFont(line, ref lastChunk, lastFont);
                        }
                        else if (tag == "s") {
                            // [s][/s], but now it's switcher
                            lastFont = SetFontStyle(lastFont, FontStyle.Strikeout);
                            SetChunkFont(line, ref lastChunk, lastFont);
                        }
                        else if (tag == "u") {
                            // [u][/u], but now it's switcher
                            lastFont = SetFontStyle(lastFont, FontStyle.Underline);
                            SetChunkFont(line, ref lastChunk, lastFont);
                        }
                        else if (tag == "r") {
                            // bad implementation
                            lastChunk = null;
                            SetChunkText(line, ref lastChunk, lastFont, "[rule]");
                        }
                        else if (tag == "url") {
                            // bad impementation
                            // [url][/url] and [url=...][/url], but now only [url=...][/url]
                            string url = "";

                            tok = strTok.Next();
                            if (tok.Kind == TokenKind.Symbol && tok.Value == "=") {
                                tok = strTok.Next();
                                do {
                                    url += tok.Value;
                                    tok = strTok.Next();
                                } while (tok.Kind != TokenKind.Symbol || tok.Value != "]");
                            } else {
                                //
                            }

                            lastFont = SetFontStyle(lastFont, FontStyle.Underline);
                            Color color = (closeTag) ? Color.Black : fLinkColor;
                            SetChunkColor(line, ref lastChunk, lastFont, color);
                            if (!closeTag) {
                                lastChunk.URL = url;
                            }
                            skipTag = true;
                        }
                        else {
                            // not tag
                            SetChunkText(line, ref lastChunk, lastFont, temp + tok.Value);
                        }

                        if (!skipTag) tok = strTok.Next();

                        //if (tok.Kind != TokenKind.Symbol || tok.Value != "]") throw new Exception("not bracket");
                    }
                } else {
                    SetChunkText(line, ref lastChunk, lastFont, tok.Value);
                }

                tok = strTok.Next();
            }
        }

        private void ArrangeText()
        {
            try {
                fAcceptFontChange = false;

                fTextFont = (Parent.Font.Clone() as Font);
                fDefBrush = new SolidBrush(Color.Black);
                fHeights = new int[fLines.Count];
                fChunks.Clear();

                Font currentFont = fTextFont.Clone() as Font;

                Graphics gfx = CreateGraphics();
                try
                {
                    int yPos = 0;
                    int xMax = 0;

                    int num = fLines.Count;
                    for (int line = 0; line < num; line++)
                    {
                        int xPos = 0;
                        int lineHeight = (int)gfx.MeasureString("A", fTextFont).Height;

                        string str = fLines[line];

                        int firstChunk = fChunks.Count;
                        ParseLine(line, str, currentFont);
                        int lastChunk = fChunks.Count - 1;

                        for (int i = firstChunk; i <= lastChunk; i++) {
                            TextChunk chunk = fChunks[i];
                            MeasureText(gfx, chunk.Text, chunk.Font, ref xPos, ref yPos, ref lineHeight, ref xMax);
                        }

                        yPos += lineHeight;
                        fHeights[line] = lineHeight;
                    }

                    fTextSize = new Size(xMax + 2 * fBorderWidth, yPos + 2 * fBorderWidth);
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
            try {
                if (fHeights.Length != fLines.Count) return;

                fAcceptFontChange = false;
                try
                {
                    Rectangle clientRect = ClientRectangle;
                    gfx.FillRectangle(new SolidBrush(SystemColors.Control), clientRect);

                    int yOffset = fBorderWidth - -AutoScrollPosition.Y;

                    int num = fLines.Count;
                    for (int line = 0; line < num; line++)
                    {
                        int xOffset = fBorderWidth - -AutoScrollPosition.X;
                        int lineHeight = fHeights[line];

                        int chunksCount = fChunks.Count;
                        for (int k = 0; k < chunksCount; k++) {
                            TextChunk chunk = fChunks[k];
                            if (chunk.Line != line) continue;

                            if (chunk.Text == "[rule]") {
                                int rulerWidth = clientRect.Width - (xOffset * 2);
                                ControlPaint.DrawBorder3D(gfx,
                                                          xOffset, yOffset + (lineHeight - 3) / 2,
                                                          rulerWidth, 3, Border3DStyle.Bump, Border3DSide.All);
                                continue;
                            }

                            int oldXOffset = xOffset;
                            int oldYOffset = yOffset;
                            OutText(gfx, chunk.Text, chunk.Font, chunk.Color, ref xOffset, ref yOffset, ref lineHeight);

                            if (!string.IsNullOrEmpty(chunk.URL)) {
                                chunk.LinkRect = ExtRect.CreateBounds(oldXOffset, oldYOffset, xOffset - oldXOffset, lineHeight);
                            }
                        }

                        yOffset += lineHeight;
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

            int yOffset = (fBorderWidth - -AutoScrollPosition.Y);
            int xOffset = (fBorderWidth - -AutoScrollPosition.X);
            fCurrentLink = null;

            int num = fChunks.Count;
            for (int i = 0; i < num; i++) {
                TextChunk chunk = fChunks[i];
                if (string.IsNullOrEmpty(chunk.URL)) continue;

                if (chunk.HasCoord(e.X, e.Y, xOffset, yOffset)) {
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
