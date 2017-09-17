/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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
using System.IO;

using GKCommon;
using GKCore.Interfaces;
using iTextSharp.text;
using iTextSharp.text.pdf;
using it = iTextSharp.text;

namespace GKCore.Export
{
    /*
     * Total: worked, but unicode and russian text don't work in any way. Recipe not found.
     */

    public class PDFWriter : CustomWriter
    {
        private sealed class FontHandler: TypeHandler<it.Font>, IFont
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

            public FontHandler(it.Font handle) : base(handle)
            {
            }
        }

        private readonly int[] iAlignments = new int[] { Element.ALIGN_LEFT, Element.ALIGN_CENTER, Element.ALIGN_RIGHT, Element.ALIGN_JUSTIFIED };
        
        private Document fDocument;
        private PdfWriter fWriter;
        private List fList;
        private Paragraph p;
        private BaseFont fBaseFont;

        public PDFWriter()
        {
            fBaseFont = BaseFont.CreateFont(GKUtils.GetLangsPath() + "fonts/FreeSans.ttf", BaseFont.IDENTITY_H, BaseFont.NOT_EMBEDDED);

            /*#if !__MonoCS__
            fBaseFont = BaseFont.CreateFont(Environment.ExpandEnvironmentVariables(@"%systemroot%\fonts\Times.ttf"), BaseFont.IDENTITY_H, BaseFont.NOT_EMBEDDED);
            #else
            //BaseFont.TIMES_ROMAN, "Cp1251"
            this.fBaseFont = BaseFont.CreateFont("/usr/share/fonts/truetype/abyssinica/AbyssinicaSIL-R.ttf", BaseFont.IDENTITY_H, BaseFont.NOT_EMBEDDED);
            #endif*/
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                if (fDocument != null) fDocument.Dispose();
            }
            base.Dispose(disposing);
        }

        public override void beginWrite()
        {
            iTextSharp.text.Rectangle pageSize = !fAlbumPage ? PageSize.A4 : PageSize.A4.Rotate();

            fDocument = new Document(pageSize, fMargins.Left, fMargins.Right, fMargins.Top, fMargins.Bottom);
            fWriter = PdfWriter.GetInstance(fDocument, new FileStream(fFileName, FileMode.Create));

            fDocument.AddTitle(fDocumentTitle);
            fDocument.AddSubject("");
            fDocument.AddAuthor("");
            fDocument.AddCreator(GKData.APP_TITLE);
            fDocument.Open();
        }

        public override void endWrite()
        {
            fDocument.Close();
        }

        public override IFont CreateFont(string name, float size, bool bold, bool underline, IColor color)
        {
            int style = it.Font.NORMAL;
            if (bold) style |= it.Font.BOLD;
            if (underline) style |= it.Font.UNDERLINE;

            BaseColor clr = new BaseColor(color.ToArgb());

            return new FontHandler(new it.Font(fBaseFont, size, style, clr));
        }

        public override void addParagraph(string text, IFont font)
        {
            fDocument.Add(new Paragraph(text, ((FontHandler)font).Handle) { Alignment = Element.ALIGN_LEFT });
        }

        public override void addParagraph(string text, IFont font, TextAlignment alignment)
        {
            int al = iAlignments[(int)alignment];
            
            fDocument.Add(new Paragraph(text, ((FontHandler)font).Handle) { Alignment = al });
        }

        public override void addParagraphAnchor(string text, IFont font, string anchor)
        {
            Chunk chunk = new Chunk(text, ((FontHandler)font).Handle);
            chunk.SetLocalDestination(anchor);
            fDocument.Add(new Paragraph(chunk));
        }

        public override void addParagraphLink(string text, IFont font, string link, IFont linkFont)
        {
            Paragraph pg = new Paragraph();
            pg.Add(new Chunk(text, ((FontHandler)font).Handle));
            pg.Add(new Chunk(link, ((FontHandler)linkFont).Handle).SetLocalGoto(link));
            fDocument.Add(pg);
        }

        public override void beginList()
        {
            fList = new List(List.UNORDERED);
            fList.SetListSymbol("\u2022");
            fList.IndentationLeft = 10f;
        }

        public override void endList()
        {
            fDocument.Add(fList);
        }

        public override void addListItem(string text, IFont font)
        {
            fList.Add(new ListItem(new Chunk(text, ((FontHandler)font).Handle)));
        }

        public override void addListItemLink(string text, IFont font, string link, IFont linkFont)
        {
            Paragraph p1 = new Paragraph();
            p1.Add(new Chunk(text, ((FontHandler)font).Handle));

            if (!string.IsNullOrEmpty(link)) {
                p1.Add(new Chunk(link, ((FontHandler)linkFont).Handle).SetLocalGoto(link));
            }

            fList.Add(new ListItem(p1));
        }

        public override void beginParagraph(TextAlignment alignment, float spacingBefore, float spacingAfter)
        {
            int al = iAlignments[(int)alignment];

            p = new Paragraph();
            p.Alignment = al;
        }

        public override void endParagraph()
        {
            fDocument.Add(p);
        }

        public override void addParagraphChunk(string text, IFont font)
        {
            p.Add(new Chunk(text, ((FontHandler)font).Handle));
        }

        public override void addParagraphChunkAnchor(string text, IFont font, string anchor)
        {
            p.Add(new Chunk(text, ((FontHandler)font).Handle).SetLocalDestination(anchor));
        }

        public override void addParagraphChunkLink(string text, IFont font, string link, IFont linkFont, bool sup)
        {
            Chunk chunk = new Chunk(text, ((FontHandler)font).Handle);
            if (sup) {
                chunk.SetTextRise(4);
            }

            if (!string.IsNullOrEmpty(link)) {
                chunk.SetLocalGoto(link);
                chunk.SetUnderline(0.5f, 3f);
            }
            
            p.Add(chunk);
        }

        public override void addNote(string text, IFont font)
        {
            
        }
    }
}
