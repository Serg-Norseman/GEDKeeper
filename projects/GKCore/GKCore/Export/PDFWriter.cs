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

#if NETCORE
#define UNOFF_ITS
#endif

using System;
using System.Collections.Generic;
using System.IO;
using BSLib;
using GKCore.Charts;
using GKCore.Design.Graphics;
using GKCore.Types;
using iTextSharp.text;
using iTextSharp.text.pdf;
using it = iTextSharp.text;
using itCell = iTextSharp.text.pdf.PdfPCell;
using itFont = iTextSharp.text.Font;
using itImage = iTextSharp.text.Image;
using itRectangle = iTextSharp.text.Rectangle;
using itTable = iTextSharp.text.pdf.PdfPTable;

namespace GKCore.Export
{
    public class PDFWriter : CustomWriter
    {
        internal sealed class FontHandler : TypeHandler<itFont>, IFont
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
        private bool fMulticolumns;
        private PdfWriter fPdfWriter;
        private GKPageSize fPredefPage;
        private itTable fTable;
        private Stack<ITextElementArray> fStack;

        public PDFWriter()
        {
            fStack = new Stack<ITextElementArray>();

            //fBaseFont = BaseFont.CreateFont(GKUtils.GetLangsPath() + "fonts/FreeSans.ttf", BaseFont.IDENTITY_H, BaseFont.NOT_EMBEDDED);

            Stream fontStream = GetType().Assembly.GetManifestResourceStream("Resources.fonts.FreeSans.ttf");
            var fontBytes = FileHelper.ReadByteArray(fontStream);
            fBaseFont = BaseFont.CreateFont("FreeSans.ttf", BaseFont.IDENTITY_H, BaseFont.EMBEDDED, BaseFont.CACHED, fontBytes, null);
        }

        public PDFWriter(GKPageSize predefPage, bool albumPage) : this()
        {
            fPredefPage = predefPage;
            fAlbumPage = albumPage;
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                if (fDocument != null) fDocument.Dispose();
            }
            base.Dispose(disposing);
        }

        public override bool SupportedText()
        {
            return true;
        }

        public override bool SupportedTables()
        {
            return true;
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

        public void SetPageSize(ExtSize size)
        {
            itRectangle pageRect = new itRectangle(0, 0, size.Height, size.Width);
            pageRect = pageRect.Rotate();
            fDocument.SetPageSize(pageRect);
            fPdfWriter.SetPageSize(pageRect);
        }

        public override void BeginWrite()
        {
            itRectangle pageSize;
            if (fPredefPage == GKPageSize.None) {
                pageSize = PageSize.A4;
            } else {
                switch (fPredefPage) {
                    case GKPageSize.A0:
                        pageSize = PageSize.A0;
                        break;
                    case GKPageSize.A1:
                        pageSize = PageSize.A1;
                        break;
                    case GKPageSize.A2:
                        pageSize = PageSize.A2;
                        break;
                    case GKPageSize.A3:
                        pageSize = PageSize.A3;
                        break;
                    case GKPageSize.A4:
                    default:
                        pageSize = PageSize.A4;
                        break;
                    case GKPageSize.A5:
                        pageSize = PageSize.A5;
                        break;
                }
            }

            itRectangle pageRect = !fAlbumPage ? pageSize : pageSize.Rotate();

            fDocument = new Document(pageRect, fMargins.Left, fMargins.Right, fMargins.Top, fMargins.Bottom);
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
            fPdfWriter.PageEvent = new PDFWriterEvents(fBaseFont, LangMan.LS(LSID.Page) + ": ");
        }

        public override void NewPage()
        {
            fDocument.NewPage();
        }

        public override void NewLine(float spacingBefore = 0.0f, float spacingAfter = 0.0f)
        {
            Chunk newline;
#if !UNOFF_ITS
            newline = Chunk.NEWLINE;
#else
            newline = Chunk.Newline;
#endif
            var pg = new Paragraph(newline) { SpacingAfter = spacingAfter };

            AddElement(pg);
        }

        public override IFont CreateFont(string name, float size, bool bold, bool underline, IColor color)
        {
            int style = itFont.NORMAL;
            if (bold) style |= itFont.BOLD;
            if (underline) style |= itFont.UNDERLINE;

            BaseColor clr = new BaseColor(color.ToArgb());

            return new FontHandler(new itFont(fBaseFont, size, style, clr));
        }

        #region Internal support

        private void AddElement(IElement o)
        {
            if (fMulticolumns) {
                fColumns.AddElement(o);
            } else {
                fDocument.Add(o);
            }
        }

        private ITextElementArray GetCurrentContainer<T>() where T : ITextElementArray
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
                var cont = (fStack.Count == 0) ? null : fStack.Peek();
                if (cont != null) {
                    cont.Add(item);
                } else {
                    AddElement(item);
                }
            }
        }

        #endregion

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
            var list = new List(List.UNORDERED);
            list.SetListSymbol("\u2022");
            list.IndentationLeft = 10f;

            fStack.Push(list);
        }

        public override void EndList()
        {
            EndContainer();
        }

        public override void BeginListItem()
        {
            var listItem = new ListItem();
            fStack.Push(listItem);
        }

        public override void EndListItem()
        {
            EndContainer();
        }

        public override void BeginParagraph(TextAlignment alignment,
                                            float spacingBefore, float spacingAfter,
                                            float indent = 0.0f, bool keepTogether = false)
        {
            var p = new Paragraph();
            p.Alignment = iAlignments[(int)alignment];
            p.SpacingBefore = spacingBefore;
            p.SpacingAfter = spacingAfter;
            p.IndentationLeft = indent;
            p.KeepTogether = keepTogether;

            fStack.Push(p);
        }

        public override void EndParagraph()
        {
            EndContainer();
        }

        public override void AddParagraphChunk(string text, IFont font)
        {
            var p = GetCurrentContainer<ITextElementArray>();
            p.Add(new Chunk(text, ((FontHandler)font).Handle));
        }

        public override void AddParagraphChunkAnchor(string text, IFont font, string anchor)
        {
            var p = GetCurrentContainer<ITextElementArray>();
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

            var p = GetCurrentContainer<ITextElementArray>();
            p.Add(chunk);
        }

        // FIXME: unused; add to other writers?
        public void AddLineSeparator()
        {
            BaseColor color;
#if !UNOFF_ITS
            color = BaseColor.BLACK;
#else
            color = BaseColor.Black;
#endif
            var line1 = new it.pdf.draw.LineSeparator(0.0f, 100.0f, color, Element.ALIGN_LEFT, 1);
            fDocument.Add(new Chunk(line1));
        }

        public override void AddImage(IImage image, TextAlignment alignment)
        {
            try {
                if (image == null) return;

                itImage img = PDFRenderer.ConvertImage(image, string.Empty);
                if (img == null) return;

                ///float fitWidth = fColumnWidth * 0.5f;
                ///img.ScaleToFit(fitWidth, fitWidth);

                // FIXME: the moving, if the page height is insufficient for the image height

                img.Alignment = itImage.TEXTWRAP | iAlignments[(int)alignment];
                img.IndentationLeft = 4f;
                img.SpacingBefore = 4f;
                img.SpacingAfter = 4f;

                //Paragraph imgpar = new Paragraph(new Chunk(img, 0, 0, true));
                //imgpar.KeepTogether = true;

                AddElement(img);
            } catch (Exception ex) {
                Logger.WriteError("PDFWriter.AddImage()", ex);
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

        public override void AddTableCell(string content, IFont font = null, TextAlignment alignment = TextAlignment.taLeft)
        {
            itFont itf = (font == null) ? null : ((FontHandler)font).Handle;
            itCell cell = new itCell(new Phrase(content, itf));
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
        private readonly List<itRectangle> fColumns;
        private int fCurrentColumn;

        public SimpleColumnText(Document document, PdfContentByte content, int columnCount, float columnSpacing) : base(content)
        {
            fDocument = document;
            fColumns = new List<itRectangle>();
            fCurrentColumn = 0;
            CalculateColumnBoundries(columnCount, columnSpacing);
        }

        private void CalculateColumnBoundries(int columnCount, float columnSpacing)
        {
            float columnHeight = (fDocument.PageSize.Height - fDocument.TopMargin - fDocument.BottomMargin);
            float columnWidth = ((fDocument.PageSize.Width - fDocument.LeftMargin - fDocument.RightMargin) - (columnSpacing * (columnCount - 1))) / columnCount;

            for (int x = 0; x < columnCount; x++) {
                float llx = ((columnWidth + columnSpacing) * x) + fDocument.LeftMargin;
                float lly = fDocument.BottomMargin;
                float urx = llx + columnWidth;
                float ury = columnHeight;

                Rectangle newRectangle = new Rectangle(llx, lly, urx, ury);
                fColumns.Add(newRectangle);
            }
        }

        public new void AddElement(IElement element)
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
                    var rect = fColumns[fCurrentColumn];
#if !UNOFF_ITS
                    SetSimpleColumn(rect);
#else
                    SetSimpleColumn(rect.Left, rect.Bottom, rect.Right, rect.Top);
#endif
                    fCurrentColumn += 1;
                }

                status = Go();
            } while (HasMoreText(status));
        }
    }
}
