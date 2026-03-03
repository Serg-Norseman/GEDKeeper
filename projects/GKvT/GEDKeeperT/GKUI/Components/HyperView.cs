/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;
using BSLib;
using GKCore;
using GKCore.BBText;
using GKCore.Design.Controls;
using GKCore.Utilities;
using GKUI.Platform.Handlers;
using Terminal.Gui;

namespace GKUI.Components
{
    using TGAttribute = Terminal.Gui.Attribute;

    public delegate void LinkEventHandler(object sender, string linkName);

    /// <summary>
    /// 
    /// </summary>
    public class HyperView : ScrollView, IHyperView
    {
        private class LinkHitInfo
        {
            public BBTextChunk Chunk { get; set; }

            public bool Contains(int x, int y)
            {
                var hitRect = Chunk.LinkRect;
                if (x >= hitRect.Left && x < hitRect.Right && y == hitRect.Top) {
                    return true;
                }
                return false;
            }
        }

        private const int BorderWidth = 1;

        private Color fBackColor;
        private readonly List<BBTextChunk> fChunks;
        private BBTextChunk fCurrentLink;
        private Color fForeColor;
        private readonly StringList fLines;
        private Color fLinkColor;
        private readonly List<LinkHitInfo> fLinkHits;
        private Size fTextSize;
        private bool fWordWrap;

        private TGAttribute fDefaultAttr;
        private TGAttribute fLinkAttr;
        private TGAttribute fLinkHoverAttr;


        public event LinkEventHandler OnLink;

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
                    UpdateLinkAttributes();
                    SetNeedsDisplay();
                }
            }
        }

        public bool WordWrap
        {
            get { return fWordWrap; }
            set {
                if (fWordWrap != value) {
                    fWordWrap = value;
                    ArrangeText();
                }
            }
        }

        public HyperView()
        {
            CanFocus = true;
            WantContinuousButtonPressed = false;
            ShowVerticalScrollIndicator = true;
            ShowHorizontalScrollIndicator = true;

            fChunks = new List<BBTextChunk>();
            fCurrentLink = null;
            fLines = new StringList();
            fLines.OnChange += new NotifyEventHandler(LinesChanged);
            fBackColor = Color.Gray;
            fForeColor = Color.Black;
            fLinkColor = Color.Blue;
            fTextSize = Size.Empty;
            fWordWrap = true;
            fLinkHits = new List<LinkHitInfo>();

            UpdateLinkAttributes();
        }

        private void UpdateLinkAttributes()
        {
            fDefaultAttr = TGAttribute.Make(fForeColor, fBackColor);
            fLinkAttr = TGAttribute.Make(fLinkColor, fBackColor);
            fLinkHoverAttr = TGAttribute.Make(fLinkColor, fBackColor);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                fChunks.Clear();
                fLinkHits.Clear();
                fLines.Dispose();
            }
            base.Dispose(disposing);
        }

        public void Activate()
        {
            SetFocus();
        }

        private void LinesChanged(object sender)
        {
            ContentOffset = Point.Empty;
            ArrangeText();
        }

        private void ArrangeText()
        {
            if (fLines.Count == 0) {
                fTextSize = Size.Empty;
                ContentSize = fTextSize;
                return;
            }

            int maxWidth = Bounds.Width - (2 * BorderWidth);
            if (maxWidth <= 0) return;

            try {
                fLinkHits.Clear();
                fChunks.Clear();

                var parser = new BBTextParser(AppHost.GfxProvider, 10f, new ColorHandler(fLinkColor), new ColorHandler(fForeColor));

                string text = fLines.Text;
                text = SysUtils.StripHTML(text);
                parser.ParseText(fChunks, text);

                int xPos = 0;
                int yPos = 0;
                int xMax = 0;

                int line = -1;
                int chunksCount = fChunks.Count;
                int k = 0;

                while (k < chunksCount) {
                    BBTextChunk chunk = fChunks[k];
                    bool recalcChunk = false;

                    if (line != chunk.Line) {
                        line = chunk.Line;

                        if (line > 0) {
                            yPos += 1;
                        }

                        xPos = 0;
                    }

                    int prevX = xPos;
                    int prevY = yPos;

                    string chunkStr = chunk.Text;
                    if (!string.IsNullOrEmpty(chunkStr)) {
                        int chunkWidth = chunkStr.Length;

                        if (fWordWrap && xPos + chunkWidth > maxWidth) {
                            int lastPos = 0, prevPos = 0;
                            string tempStr, prevStr = string.Empty;
                            int sliceType = -1;

                            while (true) {
                                tempStr = GetSlice(chunkStr, ref lastPos, ref sliceType);
                                int tempWidth = tempStr.Length;

                                if (xPos + tempWidth <= maxWidth) {
                                    prevStr = tempStr;
                                    prevPos = lastPos;
                                } else {
                                    if (sliceType == 0) {
                                        if (xPos == 0) {
                                            string tail = chunkStr.Substring(lastPos);
                                            SplitChunk(chunk, k, tempStr, tail, ref chunksCount);
                                        } else {
                                            ShiftChunks(k, chunksCount);
                                            recalcChunk = true;
                                        }
                                        break;
                                    } else if (sliceType == 1 || sliceType == 2) {
                                        string tail = chunkStr.Substring(prevPos);
                                        SplitChunk(chunk, k, prevStr, tail, ref chunksCount);
                                        break;
                                    } else if (sliceType == 3) {
                                        break;
                                    }
                                }
                            }
                        }

                        chunk.Width = chunk.Text.Length;
                        xPos += chunk.Width;
                        if (xMax < xPos) xMax = xPos;

                        if (!string.IsNullOrEmpty(chunk.URL)) {
                            chunk.LinkRect = new ExtRect(prevX, prevY + 1, chunk.Width, 1);
                            fLinkHits.Add(new LinkHitInfo { Chunk = chunk });
                        }
                    }

                    if (!recalcChunk) {
                        k++;
                    }
                }

                fTextSize = new Size(xMax + 2 * BorderWidth, yPos + 2 * BorderWidth);
                ContentSize = fTextSize;
            } catch (Exception ex) {
                Logger.WriteError("HyperView.ArrangeText()", ex);
            }
        }

        private static string GetSlice(string str, ref int lastPos, ref int type)
        {
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

        public override void Redraw(Rect bounds)
        {
            try {
                Driver.SetAttribute(fDefaultAttr);

                for (int r = 0; r < bounds.Height; r++) {
                    Move(0, r);
                    for (int c = 0; c < bounds.Width; c++) {
                        Driver.AddRune(' ');
                    }
                }

                var contentOffset = ContentOffset;
                int xOffset = BorderWidth + contentOffset.X;
                int yOffset = BorderWidth + contentOffset.Y;

                int line = -1;
                int chunksCount = fChunks.Count;

                for (int k = 0; k < chunksCount; k++) {
                    BBTextChunk chunk = fChunks[k];

                    if (line != chunk.Line) {
                        line = chunk.Line;
                        xOffset = BorderWidth + contentOffset.X;
                        yOffset += 1;
                    }

                    string ct = chunk.Text;
                    if (!string.IsNullOrEmpty(ct)) {
                        var attr = fDefaultAttr;
                        if (!string.IsNullOrEmpty(chunk.URL)) {
                            attr = (chunk == fCurrentLink) ? fLinkHoverAttr : fLinkAttr;
                        } else if (chunk.Color != null) {
                            var chColor = ((ColorHandler)chunk.Color).Handle;
                            attr = TGAttribute.Make(chColor, fBackColor);
                        }

                        Driver.SetAttribute(attr);
                        Move(xOffset, yOffset);
                        Driver.AddStr(ct);

                        /*for (int i = 0; i < ct.Length; i++) {
                            int screenX = xOffset + i;
                            int screenY = yOffset;

                            if (screenY >= 0 && screenY < bounds.Height && screenX >= 0 && screenX < bounds.Width) {
                                Move(screenX, screenY);
                                Driver.AddRune((Rune)ct[i]);
                            }
                        }*/

                        xOffset += chunk.Width;
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("HyperView.Redraw()", ex);
            }
        }

        private void DoLink(string linkName)
        {
            OnLink?.Invoke(this, linkName);
        }

        /*protected override void OnResize(EventArgs e)
        {
            if (fAcceptFontChange) {
                ArrangeText();
            }
            base.OnResize(e);
        }*/

        public override bool ProcessKey(KeyEvent kb)
        {
            switch (kb.Key) {
                case Key.PageUp:
                    ContentOffset = new Point(ContentOffset.X, Math.Min(0, ContentOffset.Y + Bounds.Height - 2));
                    return true;
                case Key.PageDown:
                    ContentOffset = new Point(ContentOffset.X, Math.Max(ContentSize.Height - Bounds.Height, ContentOffset.Y - Bounds.Height + 2));
                    return true;
                case Key.Home:
                    ContentOffset = Point.Empty;
                    return true;
                case Key.End:
                    ContentOffset = new Point(0, ContentSize.Height - Bounds.Height);
                    return true;
                case Key.CursorLeft:
                    ContentOffset = new Point(Math.Min(0, ContentOffset.X + 1), ContentOffset.Y);
                    return true;
                case Key.CursorRight:
                    ContentOffset = new Point(Math.Max(ContentSize.Width - Bounds.Width, ContentOffset.X - 1), ContentOffset.Y);
                    return true;
                case Key.CursorUp:
                    ContentOffset = new Point(ContentOffset.X, Math.Min(0, ContentOffset.Y + 1));
                    return true;
                case Key.CursorDown:
                    ContentOffset = new Point(ContentOffset.X, Math.Max(ContentSize.Height - Bounds.Height, ContentOffset.Y - 1));
                    return true;
                case Key.Enter:
                    if (fCurrentLink != null) DoLink(fCurrentLink.URL);
                    return true;
            }
            return base.ProcessKey(kb);
        }

        public override bool MouseEvent(MouseEvent me)
        {
            if (!me.Flags.HasFlag(MouseFlags.Button1Clicked) && !me.Flags.HasFlag(MouseFlags.ReportMousePosition)) {
                return false;
            }

            var contentPt = new Point(me.X - BorderWidth + ContentOffset.X, me.Y - BorderWidth + ContentOffset.Y);

            fCurrentLink = null;
            foreach (var hit in fLinkHits) {
                if (hit.Contains(contentPt.X, contentPt.Y)) {
                    fCurrentLink = hit.Chunk;
                    break;
                }
            }

            if (me.Flags.HasFlag(MouseFlags.Button1Clicked) && fCurrentLink != null) {
                DoLink(fCurrentLink.URL);
                return true;
            }

            return base.MouseEvent(me);
        }
    }
}
