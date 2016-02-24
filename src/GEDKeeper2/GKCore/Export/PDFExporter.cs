using System;
using System.IO;
using System.Windows.Forms;

using GKCore.Interfaces;
using iTextSharp.text;
using iTextSharp.text.pdf;

namespace GKCore.Export
{
    /// <summary>
    /// Localization: dirty
    /// CodeTransformation: need
    /// </summary>
    public abstract class PDFExporter : Exporter
	{
		private Padding fMargins;
		protected Document fDocument;
		protected PdfWriter fWriter;
		protected bool fAlbumPage;

	    protected PDFExporter(IBaseWindow aBase) : base(aBase)
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

        protected abstract void InternalGenerate();

		public override void Generate(bool show)
		{
			bool success = false;
			if (!this.IsRequireFilename("PDF files (*.pdf)|*.pdf")) return;

			Rectangle pageSize = !this.fAlbumPage ? PageSize.A4 : PageSize.A4.Rotate();
			
			fDocument = new Document(pageSize, this.fMargins.Left, this.fMargins.Right, this.fMargins.Top, this.fMargins.Bottom);
			try
			{
				try
				{
					this.fWriter = PdfWriter.GetInstance(fDocument, new FileStream(this.fPath, FileMode.Create));
					this.InternalGenerate();
					success = true;
				}
				finally
				{
					fDocument.Close();
				}
			}
			catch (Exception ex)
			{
				this.fBase.Host.LogWrite("PDFExporter.Generate(): " + ex.Message);
				this.fBase.Host.LogWrite("PDFExporter.Generate(): " + ex.StackTrace);
			}

			if (!success) {
				MessageBox.Show(LangMan.LS(LSID.LSID_GenerationFailed));
			} else {
				if (show) this.ShowResult();
			}
		}

		protected void AddParagraph(Chunk chunk, int alignment = Element.ALIGN_LEFT)
		{
			fDocument.Add(new Paragraph(chunk) { Alignment = alignment });
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
