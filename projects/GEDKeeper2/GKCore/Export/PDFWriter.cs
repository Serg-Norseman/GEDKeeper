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

using System;
using System.Drawing;
using System.IO;
using System.Windows.Forms;

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
        private int[] iAlignments = new int[] { Element.ALIGN_LEFT, Element.ALIGN_CENTER, Element.ALIGN_RIGHT, Element.ALIGN_JUSTIFIED };
        
        private Document fDocument;
        private PdfWriter fWriter;
        private it.List list;
        private Paragraph p;
        private BaseFont baseFont;

        public PDFWriter()
        {
            #if !GK_LINUX
            this.baseFont = BaseFont.CreateFont(Environment.ExpandEnvironmentVariables(@"%systemroot%\fonts\Times.ttf"), BaseFont.IDENTITY_H, BaseFont.NOT_EMBEDDED);
            #else
            //BaseFont.TIMES_ROMAN, "Cp1251"
            this.baseFont = BaseFont.CreateFont("/usr/share/fonts/truetype/abyssinica/AbyssinicaSIL-R.ttf", BaseFont.IDENTITY_H, BaseFont.NOT_EMBEDDED);
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
            iTextSharp.text.Rectangle pageSize = !this.fAlbumPage ? PageSize.A4 : PageSize.A4.Rotate();

            this.fDocument = new Document(pageSize, this.fMargins.Left, this.fMargins.Right, this.fMargins.Top, this.fMargins.Bottom);
            this.fWriter = PdfWriter.GetInstance(fDocument, new FileStream(this.fFileName, FileMode.Create));

            this.fDocument.AddTitle(this.fDocumentTitle);
            this.fDocument.AddSubject("");
            this.fDocument.AddAuthor("");
            this.fDocument.AddCreator(GKData.APP_TITLE);
            this.fDocument.Open();
        }

        public override void endWrite()
        {
            this.fDocument.Close();
        }

        public override object createFont(string name, float size, bool bold, bool underline, Color color)
        {
            int style = it.Font.NORMAL;
            if (bold) style |= it.Font.BOLD;
            if (underline) style |= it.Font.UNDERLINE;

            BaseColor clr = new BaseColor(color.ToArgb());

            return new it.Font(baseFont, size, style, clr);
        }

        public override void addParagraph(string text, object font)
        {
            this.fDocument.Add(new Paragraph(text, (iTextSharp.text.Font)font) { Alignment = Element.ALIGN_LEFT });
        }

        public override void addParagraph(string text, object font, TextAlignment alignment)
        {
            int al = iAlignments[(int)alignment];
            
            this.fDocument.Add(new Paragraph(text, (iTextSharp.text.Font)font) { Alignment = al });
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
            list = new it.List(it.List.UNORDERED);
            list.SetListSymbol("\u2022");
            list.IndentationLeft = 10f;
        }

        public override void endList()
        {
            fDocument.Add(list);
        }

        public override void addListItem(string text, object font)
        {
            list.Add(new it.ListItem(new Chunk(text, (iTextSharp.text.Font)font)));
        }

        public override void addListItemLink(string text, object font, string link, object linkFont)
        {
            Paragraph p1 = new Paragraph();
            p1.Add(new Chunk(text, (iTextSharp.text.Font)font));
            p1.Add(new Chunk(link, (iTextSharp.text.Font)linkFont).SetLocalGoto(link));
            list.Add(new it.ListItem(p1));
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

        /*private void AddTableHeaderCell(string content, Font font, Table table, int alignment, int width)
		{
			Cell cell = new Cell();
			cell.Header = true;
			cell.Add(new Chunk(content, font));
			table.DefaultHorizontalAlignment = alignment;
			table.AddCell(cell);
		}

		private bool CreateReport_PersonsTable(string sFile)
		{
				Table table = new Table(7);
				table.WidthPercentage = 100f;
				table.TableFitsPage = true;
				table.Padding = 2f;
				table.Spacing = 0f;
				table.Cellpadding = 2f;
				table.Cellspacing = 0f;
				table.SpaceInsideCell = 2f;
				table.BorderColor = Color.BLACK;
				table.BorderWidth = 1f;
				table.DefaultCellBackgroundColor = new Color(System.Drawing.Color.CornflowerBlue);
				table.DefaultVerticalAlignment = Element.ALIGN_TOP;

				int[] widths = new int[] { 5, 20, 10, 15, 10, 15, 20 };
				table.SetWidths(widths);

				AddTableHeaderCell("№", headers_font, table, 0, 5);
				AddTableHeaderCell("Имя", headers_font, table, 0, 15);
				AddTableHeaderCell("Родился", headers_font, table, 0, 10);
				AddTableHeaderCell("Место рождения", headers_font, table, 0, 15);
				AddTableHeaderCell("Умер", headers_font, table, 0, 10);
				AddTableHeaderCell("Место смерти", headers_font, table, 0, 15);
				AddTableHeaderCell("Занятие", headers_font, table, 0, 15);

				table.EndHeaders();

				int num = 0;

				var iEnum = this.FTree.GetEnumerator(GEDCOMRecordType.rtIndividual);
				GEDCOMRecord rec;
				while (iEnum.MoveNext(out rec))
				{
					GEDCOMIndividualRecord iRec = rec as GEDCOMIndividualRecord;
					num++;

					if (num % 2 == 1)
					{
						table.DefaultCellGrayFill = 1f;
					}

					table.DefaultHorizontalAlignment = 0;

					table.AddCell(new Phrase(iRec.XRef, cells_font));
					table.AddCell(new Phrase(iRec.GetNameString(true, false), cells_font));

					table.AddCell(new Phrase(GKUtils.GetBirthDate(iRec, TDateFormat.dfDD_MM_YYYY, false), cells_font));
					table.AddCell(new Phrase(GKUtils.GetBirthPlace(iRec), cells_font));

					table.AddCell(new Phrase(GKUtils.GetDeathDate(iRec, TDateFormat.dfDD_MM_YYYY, false), cells_font));
					table.AddCell(new Phrase(GKUtils.GetDeathPlace(iRec), cells_font));

					GEDCOMCustomEvent evt2 = iRec.GetIndividualEvent("OCCU");
					string st = ((evt2 == null) ? "" : evt2.StringValue);
					table.AddCell(new Phrase(st, cells_font));

					if (num % 2 == 1)
					{
						table.DefaultCellGrayFill = 0.9f;
					}

					Application.DoEvents();
				}

				document.Add(table);
				document.Add(new Paragraph("\nПерсональных записей: " + num.ToString(), text_font));
		}*/
    }
}
