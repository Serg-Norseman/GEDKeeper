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
//#define USE_PDFCLOWN
#endif

#if USE_PDFCLOWN

using System;
using System.Collections.Generic;
using System.Drawing;
using System.IO;
using org.pdfclown;
using org.pdfclown.documents;
using org.pdfclown.documents.interaction.forms;
using org.pdfclown.documents.contents;
using org.pdfclown.documents.files;
using org.pdfclown.documents.interaction;
using org.pdfclown.documents.contents.composition;
using org.pdfclown.documents.contents.fonts;

namespace GKCore.Export
{
    /*
     * Total: not worked even the simplest example,
     * the class system is such that it is impossible to find necessary neither in source code nor in the examples.
     */

    public class PDFClownWriter : CustomWriter
    {
        private Margins fMargins;
        protected Document fDocument;
        protected bool fAlbumPage;

        public PDFClownWriter()
        {
            this.fMargins = new Margins(20);
            this.fAlbumPage = false;
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
            }
            base.Dispose(disposing);
        }

        private void requireDocument()
        {
            if (fDocument != null) return;
            
            org.pdfclown.files.File file = new org.pdfclown.files.File(this.fFileName);
            fDocument = file.Document;
        }

        public override void beginWrite()
        {
            this.requireDocument();
        }

        public override void endWrite()
        {
            Page page = new Page(fDocument, PageFormat.GetSize(PageFormat.SizeEnum.A4, PageFormat.OrientationEnum.Portrait));
            fDocument.Pages.Add(page);
            PrimitiveComposer composer = new PrimitiveComposer(page);
            composer.SetFont(
                new StandardType1Font(
                    fDocument,
                    StandardType1Font.FamilyEnum.Courier,
                    true,
                    false
                   ),
                32
               );

            composer.ShowText("Hello World!"/*,new PointF(32,48)*/);

            composer.Flush();
        }

        public override object CreateFont(string name, float size, bool bold, bool underline, System.Drawing.Color color)
        {
            this.requireDocument();

            return new object();
        }

        public override void addParagraph(string text, object font)
        {
        }

        public override void addParagraph(string text, object font, TextAlignment alignment)
        {
        }

        public override void addParagraphAnchor(string text, object font, string anchor)
        {
        }

        public override void addParagraphLink(string text, object font, string link, object linkFont)
        {
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

        public override void addNote(string text, object font)
        {
        }
    }
}

#endif
