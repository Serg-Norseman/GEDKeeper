/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2024 by Sergey V. Zhdanovskih.
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
using Elistia.DotNetRtfWriter;
using GKCore.Design.Graphics;

namespace GKCore.Export
{
    /// <summary>
    /// 
    /// </summary>
    public class RTFWriter : CustomWriter
    {
        private sealed class FontHandler: TypeHandler<FontStruct>, IFont
        {
            public string FontFamilyName
            {
                get { return string.Empty; } // dummy
            }

            public string Name
            {
                get { return string.Empty; } // dummy
            }

            public float Size
            {
                get { return 0; } // dummy
            }

            public FontHandler(FontStruct handle) : base(handle)
            {
            }
        }

        private class FontStruct
        {
            public FontDescriptor FD;
            public float Size;
            public IColor OriginalColor;
            public ColorDescriptor Color;
            public bool Bold;
            public bool Underline;
        }

        private readonly Align[] iAlignments = new Align[] { Align.Left, Align.Center, Align.Right, Align.FullyJustify };

        private RtfDocument fDocument;
        private Stack<RtfParagraph> fStack;
        private RtfTable fTable;
        private int fTableCol;
        private int fTableRow;

        public RTFWriter()
        {
            fStack = new Stack<RtfParagraph>();
        }

        public override bool SupportedText()
        {
            return true;
        }

        public override bool SupportedTables()
        {
            return true;
        }

        public override void BeginWrite()
        {
            PaperOrientation po = (fAlbumPage) ? PaperOrientation.Landscape : PaperOrientation.Portrait;
            fDocument = new RtfDocument(PaperSize.A4, po, Lcid.English);
        }

        public override void EndWrite()
        {
            fDocument.Save(fFileName);
        }

        public override void EnablePageNumbers()
        {
        }

        public override void NewPage()
        {
        }

        public override void NewLine(float spacingBefore = 0.0f, float spacingAfter = 0.0f)
        {
        }

        public override IFont CreateFont(string name, float size, bool bold, bool underline, IColor color)
        {
            if (string.IsNullOrEmpty(name)) name = "Times New Roman";

            FontStruct fntStr = new FontStruct();
            fntStr.FD = fDocument.CreateFont(name);
            fntStr.OriginalColor = color;
            fntStr.Color = fDocument.CreateColor(new RtfColor(color.GetCode()));
            fntStr.Size = size;
            fntStr.Bold = bold;
            fntStr.Underline = underline;

            return new FontHandler(fntStr);
        }

        #region Internal support

        private static RtfCharFormat AddParagraphChunk(RtfParagraph par, string text, IFont font)
        {
            if (par == null || font == null || string.IsNullOrEmpty(text))
                return null;

            FontStruct fntStr = ((FontHandler)font).Handle;
            par.DefaultCharFormat.Font = fntStr.FD;

            int beg = par.Text.Length;
            par.Text.Append(text);
            int end = par.Text.Length - 1;

            RtfCharFormat fmt = par.AddCharFormat(beg, end);
            fmt.Font = fntStr.FD;
            fmt.FgColor = fntStr.Color;
            fmt.FontSize = fntStr.Size;
            if (fntStr.Bold) fmt.FontStyle.AddStyle(FontStyleFlag.Bold);
            if (fntStr.Underline) fmt.FontStyle.AddStyle(FontStyleFlag.Underline);

            return fmt;
        }

        private RtfParagraph GetCurrentContainer<T>() where T : RtfParagraph
        {
            var item = (fStack.Count == 0) ? null : fStack.Peek();
            while (item != null && !item.GetType().IsDerivedFromOrImplements(typeof(T))) {
                fStack.Pop();
                item = (fStack.Count == 0) ? null : fStack.Peek();
            }
            return item;
        }

        private void EndContainer()
        {
            var item = (fStack.Count == 0) ? null : fStack.Pop();
            if (item != null) {
            }
        }

        #endregion

        public override void BeginMulticolumns(int columnCount, float columnSpacing)
        {
        }

        public override void EndMulticolumns()
        {
        }

        public override void BeginList()
        {
        }

        public override void EndList()
        {
        }

        public override void BeginListItem()
        {
            BeginParagraph(TextAlignment.taLeft, 0, 0, 0);
        }

        public override void EndListItem()
        {
            EndContainer();
        }

        public override void AddListItem(string text, IFont font)
        {
            BeginListItem();

            var p = GetCurrentContainer<RtfParagraph>();
            FontStruct fntStr = ((FontHandler)font).Handle;
            var symFont = CreateFont("Symbol", fntStr.Size, fntStr.Bold, fntStr.Underline, fntStr.OriginalColor);
            AddParagraphChunk(p, "\t· ", symFont);

            AddParagraphChunk(text, font);

            EndListItem();
        }

        public override void AddListItemLink(string text, IFont font, string link, IFont linkFont)
        {
            BeginListItem();

            var p = GetCurrentContainer<RtfParagraph>();
            FontStruct fntStr = ((FontHandler)font).Handle;
            var symFont = CreateFont("Symbol", fntStr.Size, fntStr.Bold, fntStr.Underline, fntStr.OriginalColor);
            AddParagraphChunk(p, "\t· ", symFont);

            AddParagraphChunk(text, font);
            AddParagraphChunkLink(link, linkFont, link);

            EndListItem();
        }

        public override void BeginParagraph(TextAlignment alignment,
                                            float spacingBefore, float spacingAfter,
                                            float indent = 0.0f, bool keepTogether = false)
        {
            var paragraph = fDocument.AddParagraph();
            paragraph.Alignment = iAlignments[(int)alignment];
            paragraph.FirstLineIndent = indent;

            var margins = paragraph.Margins;
            margins[Direction.Top] = spacingBefore;
            margins[Direction.Bottom] = spacingAfter;

            fStack.Push(paragraph);
        }

        public override void EndParagraph()
        {
            EndContainer();
        }

        public override void AddParagraphChunk(string text, IFont font)
        {
            var p = GetCurrentContainer<RtfParagraph>();
            AddParagraphChunk(p, text, font);
        }

        public override void AddParagraphChunkAnchor(string text, IFont font, string anchor)
        {
            var p = GetCurrentContainer<RtfParagraph>();
            RtfCharFormat fmt = AddParagraphChunk(p, text, font);
            if (fmt != null) {
                fmt.Bookmark = anchor;
            }
        }

        public override void AddParagraphChunkLink(string text, IFont font, string link, bool sup = false)
        {
            var p = GetCurrentContainer<RtfParagraph>();
            RtfCharFormat fmt = AddParagraphChunk(p, text, font);
            if (fmt != null) {
                if (sup) fmt.FontStyle.AddStyle(FontStyleFlag.Super);
                fmt.LocalHyperlink = link;
            }
        }

        public override void AddImage(IImage image, TextAlignment alignment)
        {
#if !NETCORE
            try {
                if (image == null) return;

                //var p = GetCurrentContainer<RtfParagraph>();

                using (var stm = image.GetStream("png")) {
                    var rtfImage = fDocument.AddImage(stm);

                    // FIXME: dont works alignment
                    rtfImage.Alignment = iAlignments[(int)alignment];
                }
            } catch (Exception ex) {
                Logger.WriteError("RTFWriter.AddImage()", ex);
            }
#endif
        }

        public override void BeginTable(int columnsCount, int rowsCount)
        {
            fTable = fDocument.AddTable(rowsCount, columnsCount, 0);
            fTableRow = 0;
            fTableCol = 0;
        }

        public override void EndTable()
        {
        }

        public override void BeginTableRow(bool header = false)
        {
        }

        public override void EndTableRow()
        {
        }

        public override void AddTableCell(string content, IFont font = null, TextAlignment alignment = TextAlignment.taLeft)
        {
            var cell = fTable.Cell(fTableRow, fTableCol);
            if (!string.IsNullOrEmpty(content)) {
                var par = cell.AddParagraph();
                par.Alignment = iAlignments[(int)alignment];
                AddParagraphChunk(par, content, font);
            }

            fTableCol += 1;
            if (fTableCol == fTable.ColCount) {
                fTableRow += 1;
                fTableCol = 0;
            }
        }
    }
}
