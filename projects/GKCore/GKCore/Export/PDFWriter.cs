/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2018 by Sergey V. Zhdanovskih.
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

#if !NETSTANDARD

using System.IO;
using BSLib;
using BSLib.Design.Graphics;
using GKCore.Charts;
using iTextSharp.text;
using iTextSharp.text.pdf;
using it = iTextSharp.text;
using itFont = iTextSharp.text.Font;
using itImage = iTextSharp.text.Image;
using itTable = iTextSharp.text.pdf.PdfPTable;
using itCell = iTextSharp.text.pdf.PdfPCell;
using itRectangle = iTextSharp.text.Rectangle;

namespace GKCore.Export
{
    public class PDFWriter : CustomWriter
    {
        internal sealed class FontHandler: TypeHandler<itFont>, IFont
        {
            public BaseFont BaseFont
            {
                get { return Handle.BaseFont; }
            }

            public string FontFamilyName
            {
                get { return Handle.Familyname; }
            }

            public string Name
            {
                get { return string.Empty; } // dummy
            }

            public float Size
            {
                get { return Handle.Size; }
            }

            public FontHandler(itFont handle) : base(handle)
            {
            }

            public static int GetTextHeight(BaseFont baseFont, float fontSize)
            {
                float ascent = baseFont.GetAscentPoint(ChartRenderer.STR_HEIGHT_SAMPLE, fontSize);
                float descent = baseFont.GetDescentPoint(ChartRenderer.STR_HEIGHT_SAMPLE, fontSize);
                float height = (ascent - descent) * 1.33f; // Line spacing
                return (int)(height);
            }

            public static int GetTextWidth(string text, BaseFont baseFont, float fontSize)
            {
                float width = baseFont.GetWidthPoint(text, fontSize);
                return (int)(width);
            }

            public int GetTextHeight()
            {
                return GetTextHeight(Handle.BaseFont, Handle.Size);
            }

            public int GetTextWidth(string text)
            {
                return GetTextWidth(text, Handle.BaseFont, Handle.Size);
            }

            public ExtSizeF GetTextSize(string text)
            {
                return new ExtSizeF(GetTextWidth(text), GetTextHeight());
            }
        }

        private readonly int[] iAlignments = new int[] { Element.ALIGN_LEFT, Element.ALIGN_CENTER, Element.ALIGN_RIGHT, Element.ALIGN_JUSTIFIED };
        
        private readonly BaseFont fBaseFont;
        private float fColumnWidth;
        private SimpleColumnText fColumns;
        private Document fDocument;
        private List fList;
        private bool fMulticolumns;
        private PdfWriter fPdfWriter;
        private Paragraph p;
        private itTable fTable;

        public PDFWriter()
        {
            //fBaseFont = BaseFont.CreateFont(GKUtils.GetLangsPath() + "fonts/FreeSans.ttf", BaseFont.IDENTITY_H, BaseFont.NOT_EMBEDDED);

            Stream fontStream = GetType().Assembly.GetManifestResourceStream("Resources.fonts.FreeSans.ttf");
            var fontBytes = FileHelper.ReadByteArray(fontStream);
            fBaseFont = BaseFont.CreateFont("FreeSans.ttf", BaseFont.IDENTITY_H, BaseFont.EMBEDDED, BaseFont.CACHED, fontBytes, null);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                if (fDocument != null) fDocument.Dispose();
            }
            base.Dispose(disposing);
        }

        public override ChartRenderer GetPageRenderer()
        {
            var itPS = fDocument.PageSize;
            float pageHeight = itPS.Height;
            float pageWidth = itPS.Width;

            var renderer = new PDFRenderer(pageWidth, pageHeight);
            renderer.SetTarget(fPdfWriter.DirectContent);
            return renderer;
        }

        public override ExtRectF GetPageSize()
        {
            return ExtRectF.Create(fDocument.Left, fDocument.Bottom, fDocument.Right, fDocument.Top);
        }

        public override void BeginWrite()
        {
            itRectangle pageSize = !fAlbumPage ? PageSize.A4 : PageSize.A4.Rotate();

            fDocument = new Document(pageSize, fMargins.Left, fMargins.Right, fMargins.Top, fMargins.Bottom);
            fPdfWriter = PdfWriter.GetInstance(fDocument, new FileStream(fFileName, FileMode.Create, FileAccess.Write));

            fDocument.AddTitle(fDocumentTitle);
            fDocument.AddSubject("");
            fDocument.AddAuthor("");
            fDocument.AddCreator(GKData.APP_TITLE);
            fDocument.Open();
        }

        public override void EndWrite()
        {
            fDocument.Close();
        }

        public override void EnablePageNumbers()
        {
            fPdfWriter.PageEvent = new PDFWriterEvents(fBaseFont, LangMan.LS(LSID.LSID_Page)+": ");
        }

        public override void NewPage()
        {
            fDocument.NewPage();
        }

        public override void NewLine(float spacingBefore = 0.0f, float spacingAfter = 0.0f)
        {
            var pg = new Paragraph(Chunk.NEWLINE) { SpacingAfter = spacingAfter };

            if (fMulticolumns) {
                fColumns.AddElement(pg);
            } else {
                fDocument.Add(pg);
            }
        }

        public override IFont CreateFont(string name, float size, bool bold, bool underline, IColor color)
        {
            int style = itFont.NORMAL;
            if (bold) style |= itFont.BOLD;
            if (underline) style |= itFont.UNDERLINE;

            BaseColor clr = new BaseColor(color.ToArgb());

            return new FontHandler(new itFont(fBaseFont, size, style, clr));
        }

        public override void AddParagraph(string text, IFont font)
        {
            var pg = new Paragraph(text, ((FontHandler)font).Handle) { Alignment = Element.ALIGN_LEFT };

            if (fMulticolumns) {
                fColumns.AddElement(pg);
            } else {
                fDocument.Add(pg);
            }
        }

        public override void AddParagraph(string text, IFont font, TextAlignment alignment)
        {
            int al = iAlignments[(int)alignment];
            
            var pg = new Paragraph(text, ((FontHandler)font).Handle) { Alignment = al };

            if (fMulticolumns) {
                fColumns.AddElement(pg);
            } else {
                fDocument.Add(pg);
            }
        }

        public override void AddParagraphAnchor(string text, IFont font, string anchor)
        {
            Chunk chunk = new Chunk(text, ((FontHandler)font).Handle);
            chunk.SetLocalDestination(anchor);
            var pg = new Paragraph(chunk);

            if (fMulticolumns) {
                fColumns.AddElement(pg);
            } else {
                fDocument.Add(pg);
            }
        }

        public override void AddParagraphLink(string text, IFont font, string link)
        {
            Paragraph pg = new Paragraph();
            pg.Add(new Chunk(text, ((FontHandler)font).Handle).SetLocalGoto(link));

            if (fMulticolumns) {
                fColumns.AddElement(pg);
            } else {
                fDocument.Add(pg);
            }
        }

        public override void AddParagraphLink(string text, IFont font, string link, IFont linkFont)
        {
            Paragraph pg = new Paragraph();
            pg.Add(new Chunk(text, ((FontHandler)font).Handle));
            pg.Add(new Chunk(link, ((FontHandler)linkFont).Handle).SetLocalGoto(link));

            if (fMulticolumns) {
                fColumns.AddElement(pg);
            } else {
                fDocument.Add(pg);
            }
        }

        public override void BeginMulticolumns(int columnCount, float columnSpacing)
        {
            fMulticolumns = true;
            fColumns = new SimpleColumnText(fDocument, fPdfWriter.DirectContent, columnCount, columnSpacing);
            float pageWidth = fDocument.PageSize.Width - fDocument.LeftMargin - fDocument.RightMargin;
            fColumnWidth = (pageWidth - (columnSpacing * (columnCount - 1))) / columnCount;
        }

        public override void EndMulticolumns()
        {
            fMulticolumns = false;
        }

        public override void BeginList()
        {
            fList = new List(List.UNORDERED);
            fList.SetListSymbol("\u2022");
            fList.IndentationLeft = 10f;
        }

        public override void EndList()
        {
            if (fMulticolumns) {
                fColumns.AddElement(fList);
            } else {
                fDocument.Add(fList);
            }
        }

        public override void AddListItem(string text, IFont font)
        {
            fList.Add(new ListItem(new Chunk(text, ((FontHandler)font).Handle)));
        }

        public override void AddListItemLink(string text, IFont font, string link, IFont linkFont)
        {
            Paragraph p1 = new Paragraph();
            p1.Add(new Chunk(text, ((FontHandler)font).Handle));

            if (!string.IsNullOrEmpty(link)) {
                p1.Add(new Chunk(link, ((FontHandler)linkFont).Handle).SetLocalGoto(link));
            }

            fList.Add(new ListItem(p1));
        }

        public override void BeginParagraph(TextAlignment alignment,
                                            float spacingBefore, float spacingAfter,
                                            float indent = 0.0f, bool keepTogether = false)
        {
            p = new Paragraph();
            p.Alignment = iAlignments[(int)alignment];
            p.SpacingBefore = spacingBefore;
            p.SpacingAfter = spacingAfter;
            p.IndentationLeft = indent;
            p.KeepTogether = keepTogether;
        }

        public override void EndParagraph()
        {
            if (fMulticolumns) {
                fColumns.AddElement(p);
            } else {
                fDocument.Add(p);
            }
        }

        public override void AddParagraphChunk(string text, IFont font)
        {
            p.Add(new Chunk(text, ((FontHandler)font).Handle));
        }

        public override void AddParagraphChunkAnchor(string text, IFont font, string anchor)
        {
            p.Add(new Chunk(text, ((FontHandler)font).Handle).SetLocalDestination(anchor));
        }

        public override void AddParagraphChunkLink(string text, IFont font, string link, bool sup = false)
        {
            Chunk chunk = new Chunk(text, ((FontHandler)font).Handle);
            if (sup) {
                chunk.SetTextRise(4);
            }

            if (!string.IsNullOrEmpty(link)) {
                chunk.SetLocalGoto(link);
                //chunk.SetUnderline(0.5f, 3f);
            }
            
            p.Add(chunk);
        }

        public override void AddNote(string text, IFont font)
        {
            
        }

        // FIXME: add to other writers?
        public void AddLineSeparator()
        {
            var line1 = new it.pdf.draw.LineSeparator(0.0f, 100.0f, BaseColor.BLACK, Element.ALIGN_LEFT, 1);
            fDocument.Add(new Chunk(line1));
        }

        public override void AddImage(IImage image)
        {
            if (image != null) {
                itImage img = PDFRenderer.ConvertImage(image);

                float fitWidth = fColumnWidth * 0.5f;
                img.ScaleToFit(fitWidth, fitWidth);

                // FIXME: the moving, if the page height is insufficient for the image height

                //img.Alignment = Image.TEXTWRAP;
                img.IndentationLeft = 5f;
                img.SpacingBefore = 5f;
                img.SpacingAfter = 5f;

                //Paragraph imgpar = new Paragraph(new Chunk(img, 0, 0, true));
                //imgpar.KeepTogether = true;

                if (fMulticolumns) {
                    fColumns.AddElement(img);
                } else {
                    fDocument.Add(img);
                }
            }
        }

        public override void BeginTable(int columnsCount, int rowsCount)
        {
            fTable = new itTable(columnsCount);

            //table.WidthPercentage = 100f;
            //table.TableFitsPage = true;
            //table.Padding = 2f;
            //table.Spacing = 0f;
            //table.Cellpadding = 2f;
            //table.Cellspacing = 0f;
            //table.SpaceInsideCell = 2f;
            //table.BorderColor = Color.BLACK;
            //table.BorderWidth = 1f;
            //table.DefaultCellBackgroundColor = new Color(System.Drawing.Color.CornflowerBlue);
            //table.DefaultVerticalAlignment = Element.ALIGN_TOP;
            //int[] widths = new int[] { 5, 20, 10, 15, 10, 15, 20 };
            //table.SetWidths(widths);
        }

        public override void EndTable()
        {
            fDocument.Add(fTable);
        }

        public override void BeginTableRow(bool header = false)
        {
        }

        public override void EndTableRow()
        {
        }

        public override void AddTableCell(string content, IFont font, TextAlignment alignment)
        {
            itCell cell = new itCell(new Phrase(content, ((FontHandler)font).Handle));
            cell.HorizontalAlignment = iAlignments[(int)alignment];
            //cell.GrayFill
            fTable.AddCell(cell);
        }
    }

    public sealed class PDFWriterEvents : IPdfPageEvent
    {
        private readonly BaseFont fFont;
        private readonly string fFooter;

        public PDFWriterEvents(BaseFont font, string footer)
        {
            fFont = font;
            fFooter = footer;
        }

        void IPdfPageEvent.OnOpenDocument(PdfWriter writer, Document document) { }
        void IPdfPageEvent.OnCloseDocument(PdfWriter writer, Document document) { }
        void IPdfPageEvent.OnStartPage(PdfWriter writer, Document document) { }

        void IPdfPageEvent.OnEndPage(PdfWriter writer, Document document)
        {
            if (writer.PageNumber == 1) return;

            PdfContentByte cb = writer.DirectContent;
            Rectangle pageSize = document.PageSize;
            string text = fFooter + writer.PageNumber;

            cb.SaveState();
            cb.BeginText();

            cb.SetFontAndSize(fFont, 9);
            cb.ShowTextAligned(PdfContentByte.ALIGN_RIGHT, text,
                               pageSize.GetRight(document.RightMargin), pageSize.GetBottom(document.BottomMargin), 0);

            cb.EndText();
            cb.RestoreState();
        }

        void IPdfPageEvent.OnParagraph(PdfWriter writer, Document document, float paragraphPosition) { }
        void IPdfPageEvent.OnParagraphEnd(PdfWriter writer, Document document, float paragraphPosition) { }
        void IPdfPageEvent.OnChapter(PdfWriter writer, Document document, float paragraphPosition, Paragraph title) { }
        void IPdfPageEvent.OnChapterEnd(PdfWriter writer, Document document, float paragraphPosition) { }
        void IPdfPageEvent.OnSection(PdfWriter writer, Document document, float paragraphPosition, int depth, Paragraph title) { }
        void IPdfPageEvent.OnSectionEnd(PdfWriter writer, Document document, float paragraphPosition) { }
        void IPdfPageEvent.OnGenericTag(PdfWriter writer, Document document, Rectangle rect, string text) { }
    }

    internal class SimpleColumnText : ColumnText
    {
        private readonly Document fDocument;
        private readonly System.Collections.Generic.List<Rectangle> fColumns;
        private int fCurrentColumn;

        public SimpleColumnText(Document document, PdfContentByte content, int columnCount, float columnSpacing) : base(content)
        {
            fDocument = document;
            fColumns = new System.Collections.Generic.List<Rectangle>();
            fCurrentColumn = 0;
            CalculateColumnBoundries(columnCount, columnSpacing);
        }

        private void CalculateColumnBoundries(int columnCount, float columnSpacing)
        {
            float columnHeight = (fDocument.PageSize.Height - fDocument.TopMargin - fDocument.BottomMargin);
            float columnWidth = ((fDocument.PageSize.Width - fDocument.LeftMargin - fDocument.RightMargin) - (columnSpacing * (columnCount - 1))) / columnCount;

            for (int x = 0; x < columnCount; x++)
            {
                float llx = ((columnWidth + columnSpacing) * x) + fDocument.LeftMargin;
                float lly = fDocument.BottomMargin;
                float urx = llx + columnWidth;
                float ury = columnHeight;

                Rectangle newRectangle = new Rectangle(llx, lly, urx, ury);
                fColumns.Add(newRectangle);
            }
        }

        public override void AddElement(IElement element)
        {
            base.AddElement(element);

            int status = 0;
            if (fCurrentColumn == 0) {
                status = NO_MORE_COLUMN;
            }

            do {
                if (status == NO_MORE_COLUMN) {
                    if (fCurrentColumn == fColumns.Count) {
                        fDocument.NewPage();
                        fCurrentColumn = 0;
                    }
                    SetSimpleColumn(fColumns[fCurrentColumn]);
                    fCurrentColumn += 1;
                }

                status = Go();
            } while (HasMoreText(status));
        }
    }
}

#endif
