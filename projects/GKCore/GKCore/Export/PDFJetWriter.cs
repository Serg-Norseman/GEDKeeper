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

#if __MonoCS__
//#define USE_PDFJET
#endif

#if USE_PDFJET

using System;
using System.Collections.Generic;
using System.IO;

using PDFjet.NET;
using GKCommon;
using GKCore.Interfaces;

namespace GKCore.Export
{
    using pjnFont = PDFjet.NET.Font;
    /*
     * Total: the file is generated, only one worked a strange font, the Russian language is.
     * How to make arrangement - it is unclear; how to form paragraphs - it is unclear; how to make a multi-page document - is unclear.
     * All the ugly and apparently all are necessary to do the most.
     */

    public class PDFJetWriter : CustomWriter
    {
        private sealed class FontHandler: TypeHandler<pjnFont>, IFont
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

            public FontHandler(pjnFont handle) : base(handle)
            {
            }
        }

        private int[] iAlignments = new int[] { Align.LEFT, Align.CENTER, Align.RIGHT, Align.JUSTIFY };
        
        private PDF fDocument;
        private Page fPage;

        List<Paragraph> paragraphs;
        private TextColumn fColumn;

        public PDFJetWriter()
        {
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                //if (fDocument != null) fDocument.Dispose();
            }
            base.Dispose(disposing);
        }

        private void requireDocument()
        {
            if (fDocument != null) return;
            
            float[] pageSize = !this.fAlbumPage ? A4.PORTRAIT : A4.LANDSCAPE;
            
            fDocument = new PDF(new BufferedStream(new FileStream(this.fFileName, FileMode.Create)));
            fPage = new Page(fDocument, pageSize);

            this.fDocument.SetTitle(this.fDocumentTitle);
            this.fDocument.SetSubject("");
            this.fDocument.SetAuthor(GKData.APP_TITLE);
            
            this.paragraphs = new List<Paragraph>();

            this.fColumn = new TextColumn();
        }

        public override void beginWrite()
        {
            this.requireDocument();
        }

        public override void endWrite()
        {
            Text text = new Text(paragraphs);
            text.SetLocation(20f, 20f);
            text.SetWidth(fPage.GetWidth() - 40f);
            text.SetSpaceBetweenTextLines(0f);
            text.DrawOn(fPage);

            /*foreach (Paragraph p in this.paragraphs) {
				//Text text = new Text(

			}*/

            /*this.fColumn.SetLocation(70f, 200f);
			this.fColumn.SetWidth(500f);*/
            //this.fColumn.DrawOn(fPage);

            this.fDocument.Flush();
            this.fDocument.Close();
        }

        public override IFont CreateFont(string name, float size, bool bold, bool underline, IColor color)
        {
            this.requireDocument();

            //CoreFont cf = (!bold) ? CoreFont.TIMES_ROMAN : CoreFont.TIMES_BOLD;

            pjnFont fnt = new pjnFont(this.fDocument/*, cf*/, "AdobeMingStd"/*, CodePage.UNICODE*/);
            fnt.SetSize(size);
            fnt.SetKernPairs(false);
            
            return new FontHandler(fnt);
        }

        public override void addParagraph(string text, IFont font)
        {
            pjnFont fnt = ((FontHandler)font).Handle;

            Paragraph p = new Paragraph();
            TextLine tl = new TextLine(fnt, text);
            p.Add(tl);
            this.paragraphs.Add(p);
            this.fColumn.AddParagraph(p);
        }

        public override void addParagraph(string text, IFont font, TextAlignment alignment)
        {
            pjnFont fnt = ((FontHandler)font).Handle;

            int al = iAlignments[(int)alignment];
            
            Paragraph p = new Paragraph();
            p.SetAlignment(al);
            TextLine tl = new TextLine(fnt, text);
            p.Add(tl);
            this.paragraphs.Add(p);
            this.fColumn.AddParagraph(p);
        }

        public override void addParagraphAnchor(string text, IFont font, string anchor)
        {
            pjnFont fnt = ((FontHandler)font).Handle;

            Paragraph p = new Paragraph();
            TextLine tl = new TextLine(fnt, text);
            p.Add(tl);
            this.paragraphs.Add(p);
            this.fPage.AddDestination(anchor + "#", tl.GetDestinationY());
            this.fColumn.AddParagraph(p);
        }

        public override void addParagraphLink(string text, IFont font, string link, IFont linkFont)
        {
            pjnFont fnt = ((FontHandler)font).Handle;

            Paragraph p = new Paragraph();
            TextLine tl = new TextLine(fnt, text);
            tl.SetGoToAction(link + "#");
            p.Add(tl);
            this.paragraphs.Add(p);
            this.fColumn.AddParagraph(p);
        }

        public override void beginList()
        {
        }

        public override void endList()
        {
        }

        public override void addListItem(string text, IFont font)
        {
        }

        public override void addListItemLink(string text, IFont font, string link, IFont linkFont)
        {
        }

        public override void beginParagraph(TextAlignment alignment, float spacingBefore, float spacingAfter)
        {
        }

        public override void endParagraph()
        {
        }

        public override void addParagraphChunk(string text, IFont font)
        {
        }

        public override void addParagraphChunkAnchor(string text, IFont font, string anchor)
        {
        }

        public override void addParagraphChunkLink(string text, IFont font, string link, IFont linkFont, bool sup)
        {
        }

        public override void addNote(string text, IFont font)
        {
        }
    }
}

#endif
