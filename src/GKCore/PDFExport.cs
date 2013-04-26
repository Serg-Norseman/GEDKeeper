using System;
using System.Diagnostics;
using System.IO;
using System.Windows.Forms;

using Ext.Utils;
using GedCom551;
using iTextSharp.text;
using iTextSharp.text.pdf;

/// <summary>
/// Localization: dirty
/// CodeTransformation: need
/// </summary>

namespace GKCore
{
	public enum PDFReportType
	{
		rtPersonsTable,
		rtFamilyBook
	}

	public class PDFExport : Exporter
	{
		public Padding Margins;
		public string Message;

		public PDFExport(TGenEngine aEngine) : base(aEngine)
		{
			this.Margins.Left = 20;
			this.Margins.Top = 20;
			this.Margins.Right = 20;
			this.Margins.Bottom = 20;

			/*int num;
			if (int.TryParse(this.tbMgnLeft.Text, out num)) this.reportFactory.Margins.Left = num;
			if (int.TryParse(this.tbMgnTop.Text, out num)) this.reportFactory.Margins.Top = num;
			if (int.TryParse(this.tbMgnRight.Text, out num)) this.reportFactory.Margins.Right = num;
			if (int.TryParse(this.tbMgnBottom.Text, out num)) this.reportFactory.Margins.Bottom = num;*/
		}

		public override void Generate()
		{
			
		}

		public void AddTableHeaderCell(string content, Font font, Table table, int alignment, int width)
		{
			Cell cell = new Cell();
			cell.Header = true;
			cell.Add(new Chunk(content, font));
			table.DefaultHorizontalAlignment = alignment;
			table.AddCell(cell);
		}

		public bool CreateReport_PersonsTable(string sFile)
		{
			bool success = false;

			Rectangle pageSize = PageSize.A4.Rotate();
			Document document = new Document(pageSize, (float)this.Margins.Left, (float)this.Margins.Right, (float)this.Margins.Top, (float)this.Margins.Bottom);
			try
			{
				PdfWriter.GetInstance(document, new FileStream(sFile, FileMode.Create));

				BaseFont baseFont = BaseFont.CreateFont(Environment.ExpandEnvironmentVariables(@"%systemroot%\fonts\Calibri.ttf"), "CP1251", BaseFont.EMBEDDED);
    			Font text_font = new Font(baseFont, 8f, Font.NORMAL);
				Font headers_font = new Font(baseFont, 8f, Font.BOLD, Color.WHITE);
				Font cells_font = new Font(baseFont, 8f, Font.NORMAL);
				Font link_font = new Font(baseFont, 12f, Font.NORMAL, Color.BLUE);
				Font title_font = new Font(baseFont, 14f, Font.BOLD, Color.BLACK);

				document.Footer = new HeaderFooter(new Phrase("Страница: ", text_font), true) { Border = 0, Alignment = 2 };
				//document.AddTitle("");
				//document.AddSubject("");
				//document.AddAuthor("");
				document.AddCreator(TGenEngine.AppTitle);
				document.Open();

				document.Add(new Paragraph(new Chunk("Список персон", title_font)) { SpacingAfter = 15f, Alignment = 1 });

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
				table.DefaultCellBackgroundColor = new Color(System.Drawing.Color.CornflowerBlue /*Color.DodgerBlue*/);
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

				var iEnum = this.FTree.GetEnumerator(TGEDCOMRecordType.rtIndividual);
				TGEDCOMRecord rec;
				while (iEnum.MoveNext(out rec))
				{
					TGEDCOMIndividualRecord iRec = rec as TGEDCOMIndividualRecord;
					num++;

					if (num % 2 == 1)
					{
						table.DefaultCellGrayFill = 1f;
					}

					table.DefaultHorizontalAlignment = 0;

					table.AddCell(new Phrase(iRec.XRef, cells_font));
					table.AddCell(new Phrase(iRec.aux_GetNameStr(true, false), cells_font));

					table.AddCell(new Phrase(TGenEngine.GetBirthDate(iRec, TGenEngine.TDateFormat.dfDD_MM_YYYY, false), cells_font));
					table.AddCell(new Phrase(TGenEngine.GetBirthPlace(iRec), cells_font));

					table.AddCell(new Phrase(TGenEngine.GetDeathDate(iRec, TGenEngine.TDateFormat.dfDD_MM_YYYY, false), cells_font));
					table.AddCell(new Phrase(TGenEngine.GetDeathPlace(iRec), cells_font));

					TGEDCOMCustomEvent evt2 = iRec.GetIndividualEvent("OCCU");
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

				success = true;
			}
			catch (DocumentException ex)
			{
				this.Message = ex.Message;
			}
			catch (IOException ex2)
			{
				this.Message = ex2.Message;
			}
			document.Close();

			return success;
		}

		private void PreparePersonData(MultiColumnText mct, TGEDCOMIndividualRecord iRec, string iName)
		{
			//string st;

			/*TGEDCOMCustomEvent evt = iRec.GetIndividualEvent("BIRT");
			if (evt != null) {
				mct.AddElement(new Paragraph("Родился: " + st));
			}

			mct.AddElement(new Paragraph("Date of Birth: " + p.BirthDate));
			mct.AddElement(new Paragraph("Place of Birth: " + p.BirthPlace));
			mct.AddElement(new Paragraph("Date of Death: " + p.DeathDate));
			mct.AddElement(new Paragraph("Place of Death: " + p.DeathPlace));
			mct.AddElement(new Paragraph("Address: " + p.Address));
			mct.AddElement(new Paragraph("Occupation: " + p.Occupation));
			mct.AddElement(new Paragraph("Photo: " + p.Photo));*/
		}

		public bool CreateReport_FamilyBook(string filename)
		{
			bool success = false;

			Rectangle pageSize = PageSize.A4.Rotate();
			Document document = new Document(pageSize, (float)this.Margins.Left, (float)this.Margins.Right, (float)this.Margins.Top, (float)this.Margins.Bottom);
			try
			{
				PdfWriter.GetInstance(document, new FileStream(filename, FileMode.Create));

				BaseFont base_font = BaseFont.CreateFont(Environment.ExpandEnvironmentVariables(@"%systemroot%\fonts\Times.ttf"), "CP1251", BaseFont.EMBEDDED);
				Font title_font = new Font(base_font, 30f, Font.BOLD);
				Font chap_font = new Font(base_font, 16f, Font.BOLD, Color.BLACK);
				Font subchap_font = new Font(base_font, 14f, Font.BOLD, Color.BLACK);
				Font link_font = new Font(base_font, 8f, Font.UNDERLINE, Color.BLUE);
				Font text_font = new Font(base_font, 8f, Font.NORMAL, Color.BLACK);
				Font sym_font = new Font(base_font, 16f, Font.BOLD, Color.BLACK);

				BaseFont baseFont = BaseFont.CreateFont(Environment.ExpandEnvironmentVariables(@"%systemroot%\fonts\Calibri.ttf"), "CP1251", BaseFont.EMBEDDED);
    			Font page_font = new Font(baseFont, 9f, Font.NORMAL);

				HeaderFooter footer = new HeaderFooter(new Phrase("Страница: ", page_font), true) { Border = 0, Alignment = 2 };
				document.Footer = footer;

				document.AddTitle("FamilyBook");
				document.AddSubject("FamilyBook");
				document.AddAuthor("");
				document.AddCreator(TGenEngine.AppTitle);
				document.Open();

				float halfpage = (document.Top - document.Bottom - (title_font.Size + footer.Height) * 4) / 2f;
				document.Add(new Paragraph(Chunk.NEWLINE) { SpacingAfter = halfpage });
				document.Add(new Paragraph("Фамильная книга", title_font) { Alignment = Element.ALIGN_CENTER });

				//document.Add(new Chunk("Chapter 1"));
				//document.Add(new Paragraph(new Chunk("Press here to go chapter 2", font2).SetLocalGoto("2")));// Code 2
				//document.NewPage();
				//document.Add(new Paragraph(new Chunk("http://www.geek-tutorials.com", font2).SetAnchor("http://www.geek-tutorials.com")));//Code 3
				//document.Add(new Paragraph(new Chunk("Open outline.pdf chapter 3", font2).SetRemoteGoto("outline.pdf", "3")));//Code 4
				//document.NewPage();

				document.NewPage();

				Chunk chap_chunk = new Chunk("1. Персональные записи", link_font);
				chap_chunk.SetLocalGoto("IndividualRecords");
				document.Add(new Paragraph(chap_chunk));

				chap_chunk = new Chunk("2. Каталоги", link_font);
				chap_chunk.SetLocalGoto("Catalogs");
				document.Add(new Paragraph(chap_chunk));

				chap_chunk = new Chunk("2.1. Места", link_font);
				chap_chunk.SetLocalGoto("CatalogPlaces");
				document.Add(new Paragraph(chap_chunk) { IndentationLeft = 1f });

				document.NewPage();

				chap_chunk = new Chunk("Персональные записи", chap_font);
				chap_chunk.SetLocalDestination("IndividualRecords");
				document.Add(new Paragraph(chap_chunk) { Alignment = 1 });
				document.Add(new Paragraph(Chunk.NEWLINE));

				MultiColumnText mct = new MultiColumnText();
				mct.AddRegularColumns(document.Left, document.Right, 10f, 3);
				
				StringList persons = new StringList();
				TGEDCOMRecord rec;

				var iEnum = this.FTree.GetEnumerator(TGEDCOMRecordType.rtIndividual);
				while (iEnum.MoveNext(out rec))
				{
					TGEDCOMIndividualRecord iRec = rec as TGEDCOMIndividualRecord;
					string text = iRec.aux_GetNameStr(true, false);
					persons.AddObject(text, iRec);
				}
				persons.Sort();

				char sym = '!';
				int num = persons.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					string text = persons[i];
					TGEDCOMIndividualRecord iRec = persons.GetObject(i) as TGEDCOMIndividualRecord;

					char isym = text[0];
					if ((isym >= 'A' && isym <= 'Z') || (isym >= 'А' && isym <= 'Я')) {
						if (sym != isym) {
							Paragraph ps = new Paragraph(new Chunk(isym, sym_font));
							ps.SpacingBefore = 0f;
							ps.SpacingAfter = 20f;
							ps.Alignment = 1;
							ps.Add(Chunk.NEWLINE);
							mct.AddElement(ps);
							sym = isym;
						}
					}

					Chunk chunk = new Chunk(text, text_font);
					chunk.SetLocalDestination(iRec.XRef);
					Paragraph p = new Paragraph(chunk);
					mct.AddElement(p);

					Stream portrait_stm = this.FEngine.GetPrimaryBitmapStream(iRec);
					if (portrait_stm != null) {
						iTextSharp.text.Image img = iTextSharp.text.Image.GetInstance(portrait_stm);
						//img.SetDpi(50, 50);
						//img.ScaleAbsolute(100f, 100f);
						//img.XYRatio = 0.5f;
						//img.WidthPercentage = 40f;
						//img.ScalePercent(0.02f); -not work
						//img.ScalePercent(50, 50);
						img.Alignment = Image.ALIGN_LEFT | Image.TEXTWRAP;
						img.IndentationLeft = 5f;
						img.SpacingBefore = 5f;
						img.SpacingAfter = 5f;
						//img.BorderWidthTop = 36f;
						//img.BorderColorTop = Color.ORANGE;
						mct.AddElement(img);
					}

					TGEDCOMIndividualRecord father, mother;
					iRec.aux_GetParents(out father, out mother);

					if (father != null) {
						p = new Paragraph();
						text = father.aux_GetNameStr(true, false);
						chunk = new Chunk(text, link_font);
						chunk.SetLocalGoto(father.XRef);						
						p.Add(new Chunk("Отец: ", text_font)); p.Add(chunk);
						mct.AddElement(p);
					}

					if (mother != null) {
						p = new Paragraph();
						text = mother.aux_GetNameStr(true, false);
						chunk = new Chunk(text, link_font);
						chunk.SetLocalGoto(mother.XRef);
						p.Add(new Chunk("Мать: ", text_font)); p.Add(chunk);
						mct.AddElement(p);
					}

					PreparePersonData(mct, iRec, text);

					p = new Paragraph(Chunk.NEWLINE);
					p.SpacingAfter = 10f;
					mct.AddElement(p);
				}

				document.Add(mct);
				document.NewPage();

				chap_chunk = new Chunk("Каталоги", chap_font);
				chap_chunk.SetLocalDestination("Catalogs");
				document.Add(new Paragraph(chap_chunk) { Alignment = 1 });
				document.Add(new Paragraph(Chunk.NEWLINE));

				chap_chunk = new Chunk("2.1. Места", subchap_font);
				chap_chunk.SetLocalDestination("CatalogPlaces");
				document.Add(new Paragraph(chap_chunk));
				document.Add(new Paragraph(Chunk.NEWLINE));

				document.NewPage();

				success = true;
			}
			catch (DocumentException ex)
			{
				SysUtils.LogWrite("PDFExport.CreateReport_FamilyBook(): " + ex.Message);
			}
			catch (IOException ex2)
			{
				this.Message = ex2.Message;
			}
			document.Close();

			return success;
		}

		public static void GenReport(TGenEngine engine, PDFReportType reportType)
		{
			PDFExport reportFactory = new PDFExport(engine);

			SaveFileDialog saveDialog = new SaveFileDialog();
			saveDialog.Filter = "PDF files (*.pdf)|*.pdf";

			if (saveDialog.ShowDialog() == DialogResult.OK)
			{
				string fileName = saveDialog.FileName;

				bool res = true;
				if (reportType == PDFReportType.rtPersonsTable) { 
					res	= reportFactory.CreateReport_PersonsTable(fileName);
				} else if (reportType == PDFReportType.rtFamilyBook) { 
					res	= reportFactory.CreateReport_FamilyBook(fileName);
				}

				if (!res) {
					MessageBox.Show(reportFactory.Message);
				} else {
					if (System.IO.File.Exists(fileName)) Process.Start(fileName);
				}
			}
		}

	}
}
