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

using System.Collections.Generic;
using BSLib;
using GKCore.Interfaces;

namespace GKCore
{
    public sealed class BBTextChunk
    {
        public int Line;
        public string Text;
        public int Width;
        public IColor Color;
        public float Size;
        public ExtFontStyle Style;

        public string URL;
        public ExtRect LinkRect;

        public BBTextChunk(int tokenLine, float fontSize, ExtFontStyle fontStyle, IColor color)
        {
            Line = tokenLine - 1;
            Text = string.Empty;
            URL = string.Empty;

            Color = color;
            Size = fontSize;
            Style = fontStyle;
        }

        public bool HasCoord(int x, int y, int xOffset, int yOffset)
        {
            return x >= LinkRect.Left + xOffset && x <= LinkRect.Right + xOffset
                && y >= LinkRect.Top + yOffset && y <= LinkRect.Bottom + yOffset;
        }

        public bool HasCoord(int x, int y)
        {
            return x >= LinkRect.Left && x <= LinkRect.Right
                && y >= LinkRect.Top && y <= LinkRect.Bottom;
        }
    }

    /// <summary>
    /// The parser of BB-markup text.
    /// 
    /// It is located in this package, with the support of cross-platform wrappers
    /// because of the future use when exporting text notes to the program's output
    /// reports.
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
        private readonly float fDefaultFontSize;
        private readonly IGraphicsProvider fGfxProvider;
        private readonly IColor fLinkColor;
        private readonly IColor fTextColor;

        public BBTextParser(IGraphicsProvider gfxProvider, float defaultFontSize,
                            IColor linkColor, IColor textColor)
        {
            fGfxProvider = gfxProvider;
            fChunks = new List<BBTextChunk>();
            fDefaultFontSize = defaultFontSize;
            fLinkColor = linkColor;
            fTextColor = textColor;
        }

        private BBTextChunk SetChunkColor(int tokenLine, BBTextChunk chunk, IColor color)
        {
            float fntSize;
            ExtFontStyle fntStyle;
            if (chunk != null) {
                fntSize = chunk.Size;
                fntStyle = chunk.Style;
            } else {
                fntSize = fDefaultFontSize;
                fntStyle = ExtFontStyle.None;
            }

            if (chunk == null || chunk.Text.Length != 0) {
                chunk = new BBTextChunk(tokenLine, fntSize, fntStyle, color);
                fChunks.Add(chunk);
            }

            chunk.Color = color;

            return chunk;
        }

        private BBTextChunk SetChunkFontSize(int tokenLine, BBTextChunk chunk, float newSize)
        {
            ExtFontStyle fntStyle = (chunk != null) ? chunk.Style : ExtFontStyle.None;

            if (chunk == null || chunk.Text.Length != 0) {
                chunk = new BBTextChunk(tokenLine, newSize, fntStyle, fTextColor);
                fChunks.Add(chunk);
            }

            chunk.Size = newSize;

            return chunk;
        }

        private BBTextChunk SetChunkFontStyle(int tokenLine, BBTextChunk chunk, ExtFontStyle style, bool active)
        {
            float fntSize;
            ExtFontStyle fntStyle;
            if (chunk != null) {
                fntSize = chunk.Size;
                fntStyle = chunk.Style;
            } else {
                fntSize = fDefaultFontSize;
                fntStyle = ExtFontStyle.None;
            }

            if (active) {
                fntStyle |= style;
            } else {
                fntStyle &= ~style;
            }

            if (chunk == null || chunk.Text.Length != 0) {
                chunk = new BBTextChunk(tokenLine, fntSize, fntStyle, fTextColor);
                fChunks.Add(chunk);
            }

            chunk.Style = fntStyle;

            return chunk;
        }

        private BBTextChunk SetChunkText(int tokenLine, BBTextChunk chunk, string text)
        {
            float fntSize;
            ExtFontStyle fntStyle;
            if (chunk != null) {
                fntSize = chunk.Size;
                fntStyle = chunk.Style;
            } else {
                fntSize = fDefaultFontSize;
                fntStyle = ExtFontStyle.None;
            }

            if (chunk == null) {
                chunk = new BBTextChunk(tokenLine, fntSize, fntStyle, fTextColor);
                fChunks.Add(chunk);
            }

            chunk.Text += text;

            return chunk;
        }

        public void ParseText(List<BBTextChunk> chunksList, string text)
        {
            fChunks = chunksList;
            fChunks.Clear();

            float lastFontSize = fDefaultFontSize;
            BBTextChunk lastChunk = null;
            Stack<SizeChange> stackSizes = new Stack<SizeChange>();

            //lastChunk = SetChunkFontSize(0, lastChunk, fDefaultFontSize);

            if (string.IsNullOrEmpty(text)) {
                text = EMPTY_CHUNK;
                lastChunk = SetChunkText(0, lastChunk, text);
                return;
            }

            StringTokenizer strTok = new StringTokenizer(text);
            strTok.IgnoreWhiteSpace = false;
            strTok.RecognizeDecimals = false;
            strTok.RecognizeQuotedStrings = false;

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
                        lastChunk = SetChunkText(tok.Line, lastChunk, temp + tok.Value);
                    } else {
                        string tag = tok.Value;
                        //bool skipTag = false;

                        if (tag == "color") {
                            // [color="{red|#ff0000}"][/color]
                            IColor color = fTextColor;
                            if (!closedTag) {
                                tok = strTok.Next();
                                if (tok.Kind == TokenKind.Symbol && tok.Value == "=") {
                                    tok = strTok.Next();
                                    if (tok.Kind == TokenKind.Word) {
                                        color = fGfxProvider.CreateColor(tok.Value);
                                        lastChunk = SetChunkColor(tok.Line, lastChunk, color);
                                    }
                                }
                            } else {
                                // TODO: colorStack
                                color = fTextColor;
                                lastChunk = SetChunkColor(tok.Line, lastChunk, color);
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
                                        float newSize = lastFontSize + factor * ConvertHelper.ParseInt(tok.Value, 0);
                                        stackSizes.Push(new SizeChange(lastFontSize, newSize));
                                        lastChunk = SetChunkFontSize(tok.Line, lastChunk, newSize);
                                        lastFontSize = newSize;
                                    }
                                }
                            } else {
                                if (stackSizes.Count > 0) {
                                    SizeChange sizeChange = stackSizes.Pop();
                                    lastChunk = SetChunkFontSize(tok.Line, lastChunk, sizeChange.PrevSize);
                                    lastFontSize = sizeChange.PrevSize;
                                }
                            }
                        }
                        else if (tag == "b") {
                            // [b][/b]
                            lastChunk = SetChunkFontStyle(tok.Line, lastChunk, ExtFontStyle.Bold, !closedTag);
                        }
                        else if (tag == "i") {
                            // [i][/i]
                            lastChunk = SetChunkFontStyle(tok.Line, lastChunk, ExtFontStyle.Italic, !closedTag);
                        }
                        else if (tag == "s") {
                            // [s][/s]
                            lastChunk = SetChunkFontStyle(tok.Line, lastChunk, ExtFontStyle.Strikeout, !closedTag);
                        }
                        else if (tag == "u") {
                            // [u][/u]
                            lastChunk = SetChunkFontStyle(tok.Line, lastChunk, ExtFontStyle.Underline, !closedTag);
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

                            lastChunk = SetChunkFontStyle(tok.Line, lastChunk, ExtFontStyle.Underline, !closedTag);
                            IColor color = (closedTag) ? fTextColor : fLinkColor;
                            lastChunk = SetChunkColor(tok.Line, lastChunk, color);
                            if (!closedTag) {
                                lastChunk.URL = url;
                            }
                        }
                        else {
                            // not tag
                            lastChunk = SetChunkText(tok.Line, lastChunk, temp + tok.Value);
                        }

                        if (tok.Kind != TokenKind.Symbol || tok.Value != "]") {
                            // Possible syntax error?
                            strTok.Next();
                        }
                    }
                } else if (tok.Kind == TokenKind.EOL) {
                    lastChunk = SetChunkText(tok.Line, null, EMPTY_CHUNK);
                    lastChunk = null;
                } else {
                    lastChunk = SetChunkText(tok.Line, lastChunk, tok.Value);
                }

                tok = strTok.Next();
            }

            // eof
            lastChunk = SetChunkText(tok.Line + 1, null, EMPTY_CHUNK);
        }
    }
}
