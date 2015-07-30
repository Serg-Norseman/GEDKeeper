using System;
using System.Collections.Generic;
using GKCommon;
using GKCommon.GEDCOM;
using GKCommon.GEDCOM.Enums;
using GKCore.Interfaces;
using GKCore.Types;
using iTextSharp.text;
using iTextSharp.text.pdf;

namespace GKCore.Export
{
    /// <summary>
    /// Localization: dirty
    /// CodeTransformation: need
    /// </summary>
    public sealed class FamilyBookExporter : PDFExporter
	{
		private enum BookCatalog {
			Catalog_First = 0,
			
			Catalog_Places = Catalog_First,
			Catalog_BirthYears,
			Catalog_DeathYears,
			Catalog_BirthPlaces,
			Catalog_DeathPlaces,
			Catalog_DeathCauses,
			Catalog_Occupations,
			Catalog_Sources,
			
			Catalog_Last = Catalog_Sources
		}

		private struct CatalogProps {
			public readonly string Sign;
			public readonly string Title;
			
			public CatalogProps(string sign, string title)
			{
				this.Sign = sign;
				this.Title = title;
			}
		}
		
		private readonly CatalogProps[] BookCatalogs = { 
			new CatalogProps("Catalog_Places", "2.1. Места"),
			new CatalogProps("Catalog_BirthYears", "2.2. Годы рождения"),
			new CatalogProps("Catalog_DeathYears", "2.3. Годы смерти"),
			new CatalogProps("Catalog_BirthPlaces", "2.4. Места рождения"),
			new CatalogProps("Catalog_DeathPlaces", "2.5. Места смерти"),
			new CatalogProps("Catalog_DeathCauses", "2.6. Причины смерти"),
			new CatalogProps("Catalog_Occupations", "2.7. Профессии"),
			new CatalogProps("Catalog_Sources", "2.8. Источники")
		};
		
		private Font fTitleFont;
		private Font fChapFont;
		private Font fSubchapFont;
		private Font fLinkFont;
		private Font fTextFont;
		private Font fBoldFont;
		private Font fSymFont;

		private StringList mainIndex;
		private StringList byIndex, dyIndex, bpIndex, dpIndex;
		private StringList deathCauses, occuIndex, sourcesIndex;

		public FamilyBookExporter(IBase aBase) : base(aBase)
		{
			this.fAlbumPage = true;
		}

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                if (mainIndex != null) mainIndex.Dispose();
                if (byIndex != null) byIndex.Dispose();
                if (dyIndex != null) dyIndex.Dispose();
                if (bpIndex != null) bpIndex.Dispose();
                if (dpIndex != null) dpIndex.Dispose();
                if (deathCauses != null) deathCauses.Dispose();
                if (occuIndex != null) occuIndex.Dispose();
                if (sourcesIndex != null) sourcesIndex.Dispose();
            }
            base.Dispose(disposing);
        }

		protected override void InternalGenerate()
		{
			try
			{
				fDocument.AddTitle("FamilyBook");
				fDocument.AddSubject("FamilyBook");
				fDocument.AddAuthor("");
				fDocument.AddCreator(GKData.AppTitle);
				fDocument.Open();

				BaseFont base_font = BaseFont.CreateFont(Environment.ExpandEnvironmentVariables(@"%systemroot%\fonts\Times.ttf"), "CP1251", BaseFont.EMBEDDED);
				fTitleFont = new Font(base_font, 30f, Font.BOLD);
				fChapFont = new Font(base_font, 16f, Font.BOLD, BaseColor.BLACK);
				fSubchapFont = new Font(base_font, 14f, Font.BOLD, BaseColor.BLACK);
				fLinkFont = new Font(base_font, 8f, Font.UNDERLINE, BaseColor.BLUE);
				fTextFont = new Font(base_font, 8f, Font.NORMAL, BaseColor.BLACK);
				fBoldFont = new Font(base_font, 8f, Font.BOLD, BaseColor.BLACK);
				fSymFont = new Font(base_font, 14f, Font.BOLD, BaseColor.BLACK);

				base_font = BaseFont.CreateFont(Environment.ExpandEnvironmentVariables(@"%systemroot%\fonts\Calibri.ttf"), "CP1251", BaseFont.EMBEDDED);
				Font page_font = new Font(base_font, 9f, Font.NORMAL);
				
				fWriter.PageEvent = new PDFWriterEvents(base_font, "Страница: ");

				float halfpage = (fDocument.Top - fDocument.Bottom - (fTitleFont.Size) * 4) / 2f;
				fDocument.Add(new Paragraph(Chunk.NEWLINE) { SpacingAfter = halfpage });
				fDocument.Add(new Paragraph("Фамильная книга", fTitleFont) { Alignment = Element.ALIGN_CENTER });
				fDocument.NewPage();

				Chunk chap_chunk = new Chunk("Оглавление", fChapFont);
				fDocument.Add(new Paragraph(chap_chunk));
				fDocument.Add(new Paragraph(Chunk.NEWLINE));

				chap_chunk = new Chunk("1. Персональные записи", fLinkFont);
				chap_chunk.SetLocalGoto("IndividualRecords");
				fDocument.Add(new Paragraph(chap_chunk));

				chap_chunk = new Chunk("2. Каталоги", fLinkFont);
				chap_chunk.SetLocalGoto("Catalogs");
				fDocument.Add(new Paragraph(chap_chunk));

				// debug
				/*Rectangle pgSize = fDocument.PageSize;
				fDocument.Add(new Paragraph(Chunk.NEWLINE));
				chap_chunk = new Chunk(pgSize.Height.ToString() + " / " + pgSize.Width.ToString(), fChapFont);
				fDocument.Add(new Paragraph(chap_chunk) { Alignment = 1 });*/
				
				for (BookCatalog cat = BookCatalog.Catalog_First; cat <= BookCatalog.Catalog_Last; cat++)
				{
					chap_chunk = new Chunk(BookCatalogs[(int)cat].Title, fLinkFont);
					chap_chunk.SetLocalGoto(BookCatalogs[(int)cat].Sign);
					fDocument.Add(new Paragraph(chap_chunk) { IndentationLeft = 1f });
				}

				fDocument.NewPage();

				chap_chunk = new Chunk("Персональные записи", fChapFont);
				chap_chunk.SetLocalDestination("IndividualRecords");
				fDocument.Add(new Paragraph(chap_chunk) { Alignment = 1, SpacingAfter = 20f });
				fDocument.Add(new Paragraph(Chunk.NEWLINE));

				SimpleColumnText columnText = new SimpleColumnText(fDocument, fWriter.DirectContent, 3, 10f);
				float pageWidth = fDocument.PageSize.Width - fDocument.LeftMargin - fDocument.RightMargin;
				float colWidth = (pageWidth - (10f * 2)) / 3;

				this.PrepareData();

				char sym = '!';
				int num = mainIndex.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					string text = mainIndex[i];
					GEDCOMIndividualRecord iRec = mainIndex.GetObject(i) as GEDCOMIndividualRecord;

					char isym = text[0];
					if ((isym >= 'A' && isym <= 'Z') || (isym >= 'А' && isym <= 'Я')) {
						if (sym != isym) {
							Paragraph ps = new Paragraph(new Chunk(isym, fSymFont));
							ps.Alignment = 1;
							columnText.AddElement(ps);
							columnText.AddElement(new Paragraph(Chunk.NEWLINE));
							sym = isym;
						}
					}

					this.ExposePerson(columnText, iRec, text, colWidth);

					columnText.AddElement(new Paragraph(Chunk.NEWLINE));
				}

				fDocument.NewPage();

				chap_chunk = new Chunk("Каталоги", fChapFont);
				chap_chunk.SetLocalDestination("Catalogs");
				fDocument.Add(new Paragraph(chap_chunk) { Alignment = 1 });
				fDocument.Add(new Paragraph(Chunk.NEWLINE));

				this.ExposeCatalog(fDocument, null, BookCatalog.Catalog_Places);

				fDocument.NewPage();
				this.ExposeCatalog(fDocument, byIndex, BookCatalog.Catalog_BirthYears);

				fDocument.NewPage();
				this.ExposeCatalog(fDocument, dyIndex, BookCatalog.Catalog_DeathYears);

				fDocument.NewPage();
				this.ExposeCatalog(fDocument, bpIndex, BookCatalog.Catalog_BirthPlaces);

				fDocument.NewPage();
				this.ExposeCatalog(fDocument, dpIndex, BookCatalog.Catalog_DeathPlaces);

				fDocument.NewPage();
				this.ExposeCatalog(fDocument, deathCauses, BookCatalog.Catalog_DeathCauses);

				fDocument.NewPage();
				this.ExposeCatalog(fDocument, occuIndex, BookCatalog.Catalog_Occupations);

				fDocument.NewPage();
				this.ExposeCatalog(fDocument, sourcesIndex, BookCatalog.Catalog_Sources);
			}
			catch (Exception ex)
			{
				this.fBase.Host.LogWrite("FamilyBookExporter.InternalGenerate(): " + ex.Message);
				throw;
			}
		}
		
		private void PrepareData()
		{
			mainIndex = new StringList();
			byIndex = new StringList();
			dyIndex = new StringList();

			bpIndex = new StringList();
			dpIndex = new StringList();
			
			deathCauses = new StringList();
			occuIndex = new StringList();
			sourcesIndex = new StringList();
			
			GEDCOMRecord rec;

			var iEnum = this.fTree.GetEnumerator(GEDCOMRecordType.rtIndividual);
			while (iEnum.MoveNext(out rec))
			{
				GEDCOMIndividualRecord iRec = rec as GEDCOMIndividualRecord;
				string text = iRec.aux_GetNameStr(true, false);
                string st;

				mainIndex.AddObject(text, iRec);

				int ev_num = iRec.IndividualEvents.Count - 1;
				for (int k = 0; k <= ev_num; k++) 
				{
                    GEDCOMCustomEvent evt = iRec.IndividualEvents[k];
					if (evt != null)
					{
						int src_num2 = evt.Detail.SourceCitations.Count - 1;
						for (int m = 0; m <= src_num2; m++)
						{
							GEDCOMSourceRecord src = evt.Detail.SourceCitations[m].Value as GEDCOMSourceRecord;
							if (src != null)
							{
								st = src.FiledByEntry;
								if (string.IsNullOrEmpty(st)) st = src.Title.Text;
								PrepareSpecIndex(sourcesIndex, st, iRec);
							}
						}

						// Анализ по местам
//						st = ev.Detail.Place.StringValue;
//						if (!string.IsNullOrEmpty(st)) PrepareSpecIndex(places, st, iRec);

						if (evt.Name == "BIRT") {
							// Анализ по рождениям
							Exporter.PrepareEventYear(byIndex, evt, iRec);
							st = GKUtils.GetPlaceStr(evt, false);
							if (!string.IsNullOrEmpty(st)) PrepareSpecIndex(bpIndex, st, iRec);
						} 
						else if (evt.Name == "DEAT")
						{
							// Анализ по причинам смерти
							Exporter.PrepareEventYear(dyIndex, evt, iRec);
							st = GKUtils.GetPlaceStr(evt, false);
							if (!string.IsNullOrEmpty(st)) PrepareSpecIndex(dpIndex, st, iRec);

							st = evt.Detail.Cause;
							if (!string.IsNullOrEmpty(st)) PrepareSpecIndex(deathCauses, st, iRec);
						}
						else if (evt.Name == "OCCU")
						{
							// Анализ по занятиям
							st = evt.StringValue;
							if (!string.IsNullOrEmpty(st)) PrepareSpecIndex(occuIndex, st, iRec);
						}
					}
				}

				int src_num = iRec.SourceCitations.Count - 1;
				for (int k = 0; k <= src_num; k++)
				{
					GEDCOMSourceRecord src = iRec.SourceCitations[k].Value as GEDCOMSourceRecord;
					if (src != null)
					{
						st = src.FiledByEntry;
						if (string.IsNullOrEmpty(st)) st = src.Title.Text;
						PrepareSpecIndex(sourcesIndex, st, iRec);
					}
				}
			}

			mainIndex.Sort();
			byIndex.Sort();
			dyIndex.Sort();
			bpIndex.Sort();
			dpIndex.Sort();
			deathCauses.Sort();
			occuIndex.Sort();
			sourcesIndex.Sort();
		}

		private void ExposePerson(ColumnText mct, GEDCOMIndividualRecord iRec, string iName, float colWidth)
		{
			Paragraph pg = new Paragraph();
			Chunk chunk = new Chunk(iName, fBoldFont);
			chunk.SetLocalDestination(iRec.XRef);
			pg.Add(chunk);
			chunk = new Chunk(GKUtils.GetPedigreeLifeStr(iRec, PedigreeFormat.pfCompact), fTextFont);
			pg.Add(chunk);
			pg.KeepTogether = true;
			mct.AddElement(pg);

			var bmp = this.fBase.GetPrimaryBitmap(iRec, 0, 0, false);
			if (bmp != null)
			{
				iTextSharp.text.Image img = iTextSharp.text.Image.GetInstance(bmp, System.Drawing.Imaging.ImageFormat.Bmp);

				float fitWidth = colWidth * 0.5f;
				img.ScaleToFit(fitWidth, fitWidth);

				// FIXME: перенос, если высоты страницы недостаточно для высоты изображения

				//img.Alignment = Image.TEXTWRAP;
				img.IndentationLeft = 5f;
				img.SpacingBefore = 5f;
				img.SpacingAfter = 5f;

				//Paragraph imgpar = new Paragraph(new Chunk(img, 0, 0, true));
				//imgpar.KeepTogether = true;
				
				mct.AddElement(img);
			}

			GEDCOMIndividualRecord father, mother;
			iRec.aux_GetParents(out father, out mother);
			string text;

			if (father != null) {
				pg = new Paragraph();
				chunk = new Chunk(father.aux_GetNameStr(true, false), fLinkFont);
				chunk.SetLocalGoto(father.XRef);
				pg.Add(new Chunk("Отец: ", fTextFont)); pg.Add(chunk);
				mct.AddElement(pg);
			}

			if (mother != null) {
				pg = new Paragraph();
				chunk = new Chunk(mother.aux_GetNameStr(true, false), fLinkFont);
				chunk.SetLocalGoto(mother.XRef);
				pg.Add(new Chunk("Мать: ", fTextFont)); pg.Add(chunk);
				mct.AddElement(pg);
			}

			//string st;

			/*GEDCOMCustomEvent evt = iRec.GetIndividualEvent("BIRT");
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

		private void ExposeCatalog(Document document, StringList index, BookCatalog catalog)
		{
			Chunk chunk = new Chunk(BookCatalogs[(int)catalog].Title, fSubchapFont);
			chunk.SetLocalDestination(BookCatalogs[(int)catalog].Sign);
			document.Add(new Paragraph(chunk));
			document.Add(new Paragraph(Chunk.NEWLINE));

			if (index == null) return;

			SimpleColumnText columnText = new SimpleColumnText(fDocument, fWriter.DirectContent, 3, 10f);
			for (int i = 0; i < index.Count; i++)
			{
				Paragraph ps = new Paragraph(new Chunk(index[i], fSymFont));
				ps.SpacingBefore = 0f;
				ps.SpacingAfter = 20f;
				ps.Alignment = 1;
				ps.Add(Chunk.NEWLINE);
				columnText.AddElement(ps);

				StringList persons = index.GetObject(i) as StringList;
				persons.Sort();

				for (int k = 0; k < persons.Count; k++)
				{
					GEDCOMIndividualRecord iRec = persons.GetObject(k) as GEDCOMIndividualRecord;

					chunk = new Chunk(persons[k], fTextFont);
					chunk.SetLocalGoto(iRec.XRef);
					Paragraph p = new Paragraph(chunk);
					columnText.AddElement(p);
				}

				columnText.AddElement(new Paragraph(Chunk.NEWLINE) { SpacingAfter = 10f });
			}
		}
	}
	
	public class PDFWriterEvents : IPdfPageEvent
	{
		private readonly BaseFont fFont;
		private readonly string fFooter;

		public PDFWriterEvents(BaseFont font, string footer)
		{
			this.fFont = font;
			this.fFooter = footer;
		}

		public void OnOpenDocument(PdfWriter writer, Document document) { }
		public void OnCloseDocument(PdfWriter writer, Document document) { }
		public void OnStartPage(PdfWriter writer, Document document) { }

		public void OnEndPage(PdfWriter writer, Document document)
		{
			if (writer.PageNumber == 1) return;
			
			try
			{
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
			catch (DocumentException)
			{
				throw;
			}
		}

		public void OnParagraph(PdfWriter writer, Document document, float paragraphPosition) { }
		public void OnParagraphEnd(PdfWriter writer, Document document, float paragraphPosition) { }
		public void OnChapter(PdfWriter writer, Document document, float paragraphPosition, Paragraph title) { }
		public void OnChapterEnd(PdfWriter writer, Document document, float paragraphPosition) { }
		public void OnSection(PdfWriter writer, Document document, float paragraphPosition, int depth, Paragraph title) { }
		public void OnSectionEnd(PdfWriter writer, Document document, float paragraphPosition) { }
		public void OnGenericTag(PdfWriter writer, Document document, Rectangle rect, String text) { }
	}
	
	public class SimpleColumnText : ColumnText
	{
		private readonly Document fDocument;
		private readonly List<Rectangle> fColumns;
		private int fCurrentColumn;

		public SimpleColumnText(Document document, PdfContentByte content, int columnCount, float columnSpacing) : base(content)
		{
			this.fDocument = document;
            this.fColumns = new List<Rectangle>();
            this.fCurrentColumn = 0;
			this.CalculateColumnBoundries(columnCount, columnSpacing);
		}

		private void CalculateColumnBoundries(int columnCount, float columnSpacing)
		{
			float columnHeight = (fDocument.PageSize.Height - fDocument.TopMargin - fDocument.BottomMargin);
			float columnWidth = ((fDocument.PageSize.Width - fDocument.LeftMargin - fDocument.RightMargin) - (columnSpacing * (columnCount - 1))) / columnCount;

			for (int x = 0; x <= columnCount - 1; x++)
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
				status = ColumnText.NO_MORE_COLUMN;
			}

			do {
				if (status == ColumnText.NO_MORE_COLUMN) {
					if (fCurrentColumn == fColumns.Count) {
						fDocument.NewPage();
						fCurrentColumn = 0;
					}
					base.SetSimpleColumn(this.fColumns[fCurrentColumn]);
					fCurrentColumn += 1;
				}

				status = base.Go();
			} while (ColumnText.HasMoreText(status));
		}
	}
}
