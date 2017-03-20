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
using System.Drawing;
using System.IO;

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
        private readonly int[] iAlignments = new int[] { Element.ALIGN_LEFT, Element.ALIGN_CENTER, Element.ALIGN_RIGHT, Element.ALIGN_JUSTIFIED };
        
        private Document fDocument;
        private PdfWriter fWriter;
        private List fList;
        private Paragraph p;
        private BaseFont fBaseFont;

        public PDFWriter()
        {
            #if !__MonoCS__
            fBaseFont = BaseFont.CreateFont(Environment.ExpandEnvironmentVariables(@"%systemroot%\fonts\Times.ttf"), BaseFont.IDENTITY_H, BaseFont.NOT_EMBEDDED);
            #else
            //BaseFont.TIMES_ROMAN, "Cp1251"
            this.fBaseFont = BaseFont.CreateFont("/usr/share/fonts/truetype/abyssinica/AbyssinicaSIL-R.ttf", BaseFont.IDENTITY_H, BaseFont.NOT_EMBEDDED);
            #endif
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

        public override object CreateFont(string name, float size, bool bold, bool underline, Color color)
        {
            int style = it.Font.NORMAL;
            if (bold) style |= it.Font.BOLD;
            if (underline) style |= it.Font.UNDERLINE;

            BaseColor clr = new BaseColor(color.ToArgb());

            return new it.Font(fBaseFont, size, style, clr);
        }

        public override void addParagraph(string text, object font)
        {
            fDocument.Add(new Paragraph(text, (iTextSharp.text.Font)font) { Alignment = Element.ALIGN_LEFT });
        }

        public override void addParagraph(string text, object font, TextAlignment alignment)
        {
            int al = iAlignments[(int)alignment];
            
            fDocument.Add(new Paragraph(text, (iTextSharp.text.Font)font) { Alignment = al });
        }

        public override void addParagraphAnchor(string text, object font, string anchor)
        {
            Chunk chunk = new Chunk(text, (iTextSharp.text.Font)font);
            chunk.SetLocalDestination(anchor);
            fDocument.Add(new Paragraph(chunk));
        }

        public override void addParagraphLink(string text, object font, string link, object linkFont)
        {
            Paragraph pg = new Paragraph();
            pg.Add(new Chunk(text, (iTextSharp.text.Font)font));
            pg.Add(new Chunk(link, (iTextSharp.text.Font)linkFont).SetLocalGoto(link));
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

        public override void addListItem(string text, object font)
        {
            fList.Add(new ListItem(new Chunk(text, (iTextSharp.text.Font)font)));
        }

        public override void addListItemLink(string text, object font, string link, object linkFont)
        {
            Paragraph p1 = new Paragraph();
            p1.Add(new Chunk(text, (iTextSharp.text.Font)font));

            if (!string.IsNullOrEmpty(link)) {
                p1.Add(new Chunk(link, (iTextSharp.text.Font)linkFont).SetLocalGoto(link));
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

        public override void addParagraphChunk(string text, object font)
        {
            p.Add(new Chunk(text, (iTextSharp.text.Font)font));
        }

        public override void addParagraphChunkAnchor(string text, object font, string anchor)
        {
            p.Add(new Chunk(text, (iTextSharp.text.Font)font).SetLocalDestination(anchor));
        }

        public override void addParagraphChunkLink(string text, object font, string link, object linkFont, bool sup)
        {
            Chunk chunk = new Chunk(text, (iTextSharp.text.Font)font);
            if (sup) {
                chunk.SetTextRise(4);
            }

            if (!string.IsNullOrEmpty(link)) {
                chunk.SetLocalGoto(link);
                chunk.SetUnderline(0.5f, 3f);
            }
            
            p.Add(chunk);
        }

        public override void addNote(string text, object font)
        {
            
        }
    }
}
