using System;
using System.Drawing;
using System.IO;
using System.Windows.Forms;
using iTextSharp.text;
using iTextSharp.text.pdf;
using it = iTextSharp.text;

namespace GKCore.Export
{
	public class PDFWriter : CustomWriter
	{
		private int[] iAlignments = new int[] { Element.ALIGN_LEFT, Element.ALIGN_CENTER, Element.ALIGN_RIGHT, Element.ALIGN_JUSTIFIED };
		
		private Padding fMargins;
		protected Document fDocument;
		protected PdfWriter fWriter;
		protected bool fAlbumPage;
		private it.List list;
		private Paragraph p;

		public PDFWriter()
		{
			this.fMargins.Left = 20;
			this.fMargins.Top = 20;
			this.fMargins.Right = 20;
			this.fMargins.Bottom = 20;
			this.fAlbumPage = false;
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

		public override void setAlbumPage(bool value)
		{
			this.fAlbumPage = value;
		}

		public override object createFont(string name, int size, bool bold, bool underline, Color color)
		{
			return null;
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
			Paragraph p = new Paragraph();
			p.Add(new Chunk(text, (iTextSharp.text.Font)font));
			p.Add(new Chunk(link, (iTextSharp.text.Font)linkFont).SetLocalGoto(link));
			fDocument.Add(p);
		}

		private void addParagraph(Chunk chunk, int alignment = Element.ALIGN_LEFT)
		{
			this.fDocument.Add(new Paragraph(chunk) { Alignment = alignment });
		}

		public override void beginList()
		{
			list = new it.List(it.List.UNORDERED);
			list.SetListSymbol("\u2022");
			list.IndentationLeft = 10f;
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

		public override void endList()
		{
			fDocument.Add(list);
		}

		public override void beginParagraph(TextAlignment alignment)
		{
			int al = iAlignments[(int)alignment];

			p = new Paragraph();
			p.Alignment = al;
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

		public override void endParagraph()
		{
			fDocument.Add(p);
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
