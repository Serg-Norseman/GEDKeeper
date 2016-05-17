/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
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

#if GK_LINUX
//#define USE_PDFJET
#endif

#if USE_PDFJET

using System;
using System.Collections.Generic;
using System.Drawing;
using System.IO;
using PDFjet.NET;

namespace GKCore.Export
{
    /*
     * Total: the file is generated, only one worked a strange font, the Russian language is.
     * How to make arrangement - it is unclear; how to form paragraphs - it is unclear; how to make a multi-page document - is unclear.
     * All the ugly and apparently all are necessary to do the most.
     */

    public class PDFJetWriter : CustomWriter
    {
        private class FontStruct
        {
            public PDFjet.NET.Font FD;
            public System.Drawing.Color OriginalColor;
            public bool Bold;
            public bool Underline;
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

        public override object createFont(string name, float size, bool bold, bool underline, System.Drawing.Color color)
        {
            this.requireDocument();

            //CoreFont cf = (!bold) ? CoreFont.TIMES_ROMAN : CoreFont.TIMES_BOLD;

            PDFjet.NET.Font fnt = new PDFjet.NET.Font(this.fDocument/*, cf*/, "AdobeMingStd"/*, CodePage.UNICODE*/);
            fnt.SetSize(size);
            fnt.SetKernPairs(false);
            
            FontStruct fntStr = new FontStruct();
            fntStr.FD = fnt;
            fntStr.OriginalColor = color;
            fntStr.Bold = bold;
            fntStr.Underline = underline;

            return fntStr;
        }

        public override void addParagraph(string text, object font)
        {
            Paragraph p = new Paragraph();
            TextLine tl = new TextLine(((FontStruct)font).FD, text);
            p.Add(tl);
            this.paragraphs.Add(p);
            this.fColumn.AddParagraph(p);
        }

        public override void addParagraph(string text, object font, TextAlignment alignment)
        {
            int al = iAlignments[(int)alignment];
            
            Paragraph p = new Paragraph();
            p.SetAlignment(al);
            TextLine tl = new TextLine(((FontStruct)font).FD, text);
            p.Add(tl);
            this.paragraphs.Add(p);
            this.fColumn.AddParagraph(p);
        }

        public override void addParagraphAnchor(string text, object font, string anchor)
        {
            Paragraph p = new Paragraph();
            TextLine tl = new TextLine(((FontStruct)font).FD, text);
            p.Add(tl);
            this.paragraphs.Add(p);
            this.fPage.AddDestination(anchor + "#", tl.GetDestinationY());
            this.fColumn.AddParagraph(p);
        }

        public override void addParagraphLink(string text, object font, string link, object linkFont)
        {
            Paragraph p = new Paragraph();
            TextLine tl = new TextLine(((FontStruct)font).FD, text);
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

        public override void addListItem(string text, object font)
        {
        }

        public override void addListItemLink(string text, object font, string link, object linkFont)
        {
        }

        public override void beginParagraph(TextAlignment alignment, float spacingBefore, float spacingAfter)
        {
        }

        public override void endParagraph()
        {
        }

        public override void addParagraphChunk(string text, object font)
        {
        }

        public override void addParagraphChunkAnchor(string text, object font, string anchor)
        {
        }

        public override void addParagraphChunkLink(string text, object font, string link, object linkFont, bool sup)
        {
        }
    }
}

#endif
