/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2017 by Sergey V. Zhdanovskih.
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

//#define HV_DEBUG

using System;
using System.Collections.Generic;
using System.Drawing;

namespace GKCommon
{
    public sealed class BBTextChunk
    {
        public int Line;
        public string Text;
        public int Width;
        public Color Color;
        public float Size;
        public FontStyle Style;

        public string URL;
        public ExtRect LinkRect;

        public BBTextChunk(int tokenLine, float fontSize, FontStyle fontStyle)
        {
            Line = tokenLine - 1;
            Text = string.Empty;
            URL = string.Empty;

            Color = Color.Black;
            Size = fontSize;
            Style = fontStyle;
        }

        public bool HasCoord(int x, int y, int xOffset, int yOffset)
        {
            return x >= LinkRect.Left + xOffset && x <= LinkRect.Right + xOffset
                && y >= LinkRect.Top + yOffset && y <= LinkRect.Bottom + yOffset;
        }
    }

    /// <summary>
    /// 
    /// </summary>
    public class BBTextParser
    {
        #if HV_DEBUG
        private const string EMPTY_CHUNK = "+";
        #else
        private const string EMPTY_CHUNK = " ";
        #endif

        private sealed class SizeChange
        {
            public readonly float PrevSize;
            public readonly float NextSize;

            public SizeChange(float prevSize, float nextSize)
            {
                PrevSize = prevSize;
                NextSize = nextSize;
            }
        }

        private List<BBTextChunk> fChunks;
        private float fDefaultFontSize;
        private Color fLinkColor;
        private Color fTextColor;

        public BBTextParser(float defaultFontSize, Color linkColor, Color textColor)
        {
            fChunks = new List<BBTextChunk>();
            fDefaultFontSize = defaultFontSize;
            fLinkColor = linkColor;
            fTextColor = textColor;
        }

        private void GetPrevFontParams(BBTextChunk chunk, out float fntSize, out FontStyle fntStyle)
        {
            if (chunk != null) {
                fntSize = chunk.Size;
                fntStyle = chunk.Style;
            } else {
                fntSize = fDefaultFontSize;
                fntStyle = FontStyle.Regular;
            }
        }

        private void SetChunkColor(int tokenLine, ref BBTextChunk chunk, Color color)
        {
            float fntSize;
            FontStyle fntStyle;
            GetPrevFontParams(chunk, out fntSize, out fntStyle);

            if (chunk == null || chunk.Text.Length != 0) {
                chunk = new BBTextChunk(tokenLine, fntSize, fntStyle);
                fChunks.Add(chunk);
            }

            chunk.Color = color;
        }

        private void SetChunkFontSize(int tokenLine, ref BBTextChunk chunk, float newSize)
        {
            float fntSize;
            FontStyle fntStyle;
            GetPrevFontParams(chunk, out fntSize, out fntStyle);

            if (chunk == null || chunk.Text.Length != 0) {
                chunk = new BBTextChunk(tokenLine, newSize, fntStyle);
                fChunks.Add(chunk);
            }

            chunk.Size = newSize;
        }

        private void SetChunkFontStyle(int tokenLine, ref BBTextChunk chunk, FontStyle style, bool active)
        {
            float fntSize;
            FontStyle fntStyle;
            GetPrevFontParams(chunk, out fntSize, out fntStyle);

            FontStyle newStyle = fntStyle;
            if (active) {
                newStyle |= style;
            } else {
                newStyle &= ~style;
            }

            if (chunk == null || chunk.Text.Length != 0) {
                chunk = new BBTextChunk(tokenLine, fntSize, newStyle);
                fChunks.Add(chunk);
            }

            chunk.Style = newStyle;
        }

        private void SetChunkText(int tokenLine, ref BBTextChunk chunk, string text)
        {
            float fntSize;
            FontStyle fntStyle;
            GetPrevFontParams(chunk, out fntSize, out fntStyle);

            if (chunk == null) {
                chunk = new BBTextChunk(tokenLine, fntSize, fntStyle);
                fChunks.Add(chunk);
            }

            chunk.Text += text;
        }

        public void ParseText(List<BBTextChunk> chunksList, string text)
        {
            fChunks = chunksList;
            fChunks.Clear();

            float lastFontSize = fDefaultFontSize;
            BBTextChunk lastChunk = null;
            Stack<SizeChange> stackSizes = new Stack<SizeChange>();

            //SetChunkFontSize(0, ref lastChunk, fDefaultFontSize);

            if (string.IsNullOrEmpty(text)) {
                text = EMPTY_CHUNK;
                SetChunkText(0, ref lastChunk, text);
                return;
            }

            StringTokenizer strTok = new StringTokenizer(text);
            strTok.IgnoreWhiteSpace = false;
            strTok.RecognizeDecimals = false;

            Token tok = strTok.Next();
            while (tok.Kind != TokenKind.EOF) {
                if (tok.Kind == TokenKind.Symbol && tok.Value == "[") {
                    string temp = tok.Value;
                    tok = strTok.Next();

                    bool closedTag;
                    if (tok.Kind == TokenKind.Symbol && tok.Value == "/") {
                        closedTag = true;
                        temp += tok.Value;
                        tok = strTok.Next();
                    } else {
                        closedTag = false;
                    }

                    if (tok.Kind != TokenKind.Word) {
                        // not tag
                        SetChunkText(tok.Line, ref lastChunk, temp + tok.Value);
                    } else {
                        string tag = tok.Value;
                        //bool skipTag = false;

                        if (tag == "color") {
                            // [color="{red|#ff0000}"][/color]
                            Color color = fTextColor;
                            if (!closedTag) {
                                tok = strTok.Next();
                                if (tok.Kind == TokenKind.Symbol && tok.Value == "=") {
                                    tok = strTok.Next();
                                    if (tok.Kind == TokenKind.Word) {
                                        color = Color.FromName(tok.Value);
                                        SetChunkColor(tok.Line, ref lastChunk, color);
                                    }
                                }
                            } else {
                                // TODO: colorStack
                                color = fTextColor;
                                SetChunkColor(tok.Line, ref lastChunk, color);
                            }
                        }
                        else if (tag == "size") {
                            // [size={+/-x}][/size]
                            if (!closedTag) {
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
                                        float newSize = lastFontSize + factor * SysUtils.ParseInt(tok.Value, 0);
                                        stackSizes.Push(new SizeChange(lastFontSize, newSize));
                                        SetChunkFontSize(tok.Line, ref lastChunk, newSize);
                                        lastFontSize = newSize;
                                    }
                                }
                            } else {
                                if (stackSizes.Count > 0) {
                                    SizeChange sizeChange = stackSizes.Pop();
                                    SetChunkFontSize(tok.Line, ref lastChunk, sizeChange.PrevSize);
                                    lastFontSize = sizeChange.PrevSize;
                                }
                            }
                        }
                        else if (tag == "b") {
                            // [b][/b]
                            SetChunkFontStyle(tok.Line, ref lastChunk, FontStyle.Bold, !closedTag);
                        }
                        else if (tag == "i") {
                            // [i][/i]
                            SetChunkFontStyle(tok.Line, ref lastChunk, FontStyle.Italic, !closedTag);
                        }
                        else if (tag == "s") {
                            // [s][/s]
                            SetChunkFontStyle(tok.Line, ref lastChunk, FontStyle.Strikeout, !closedTag);
                        }
                        else if (tag == "u") {
                            // [u][/u]
                            SetChunkFontStyle(tok.Line, ref lastChunk, FontStyle.Underline, !closedTag);
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

                            SetChunkFontStyle(tok.Line, ref lastChunk, FontStyle.Underline, !closedTag);
                            Color color = (closedTag) ? fTextColor : fLinkColor;
                            SetChunkColor(tok.Line, ref lastChunk, color);
                            if (!closedTag) {
                                lastChunk.URL = url;
                            }
                        }
                        else {
                            // not tag
                            SetChunkText(tok.Line, ref lastChunk, temp + tok.Value);
                        }

                        if (tok.Kind != TokenKind.Symbol || tok.Value != "]") {
                            // Possible syntax error?
                            strTok.Next();
                        }
                    }
                } else if (tok.Kind == TokenKind.EOL) {
                    lastChunk = null;
                    SetChunkText(tok.Line, ref lastChunk, EMPTY_CHUNK);
                    lastChunk = null;
                } else {
                    SetChunkText(tok.Line, ref lastChunk, tok.Value);
                }

                tok = strTok.Next();
            }

            // eof
            lastChunk = null;
            SetChunkText(tok.Line, ref lastChunk, EMPTY_CHUNK);
        }
    }
}
