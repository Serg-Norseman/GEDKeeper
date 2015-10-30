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
			
			Catalog_BirthYears = Catalog_First,
			Catalog_DeathYears,
			Catalog_BirthPlaces,
			Catalog_DeathPlaces,
			Catalog_DeathCauses,
			Catalog_Occupations,
			Catalog_Religion,
			Catalog_Sources,
			
			Catalog_Last = Catalog_Sources
		}

		private struct CatalogProps {
			public readonly string Sign;
			public readonly string Title;
			public StringList Index;
			
			public CatalogProps(string sign, string title)
			{
				this.Sign = sign;
				this.Title = title;
				this.Index = null;
			}
		}
		
		private readonly CatalogProps[] BookCatalogs = { 
			new CatalogProps("Catalog_BirthYears", "Годы рождения"),
			new CatalogProps("Catalog_DeathYears", "Годы смерти"),
			new CatalogProps("Catalog_BirthPlaces", "Места рождения"),
			new CatalogProps("Catalog_DeathPlaces", "Места смерти"),
			new CatalogProps("Catalog_DeathCauses", "Причины смерти"),
			new CatalogProps("Catalog_Occupations", "Профессии"),
			new CatalogProps("Catalog_Religion", "Вероисповедание"),
			new CatalogProps("Catalog_Sources", "Источники")
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
		private StringList deathCauses, occuIndex, reliIndex, sourcesIndex;

		// temp options
		public bool SkipEmptyCatalogs = true;
		public bool CatalogNewPages = false;
		public bool IncludeEvents = true;
		public bool IncludeNotes = true;
		
		
		public FamilyBookExporter(IBaseWindow aBase) : base(aBase)
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
                if (reliIndex != null) reliIndex.Dispose();
                if (sourcesIndex != null) sourcesIndex.Dispose();
            }
            base.Dispose(disposing);
        }

		protected override void InternalGenerate()
		{
			try
			{
				this.PrepareData();

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
				fSymFont = new Font(base_font, 12f, Font.BOLD, BaseColor.BLACK);

				base_font = BaseFont.CreateFont(Environment.ExpandEnvironmentVariables(@"%systemroot%\fonts\Calibri.ttf"), "CP1251", BaseFont.EMBEDDED);
				//Font page_font = new Font(base_font, 9f, Font.NORMAL);
				
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
				
				int catNum = 0;
				for (BookCatalog cat = BookCatalog.Catalog_First; cat <= BookCatalog.Catalog_Last; cat++)
				{
					CatalogProps catProps = BookCatalogs[(int)cat];
					
					if (!this.SkipEmptyCatalogs || catProps.Index.Count > 0) {
						catNum++;
						string title = "2." + catNum.ToString() + ". " + catProps.Title;
						
						chap_chunk = new Chunk(title, fLinkFont);
						chap_chunk.SetLocalGoto(catProps.Sign);
						fDocument.Add(new Paragraph(chap_chunk) { IndentationLeft = 1f });
					}
				}

				fDocument.NewPage();

				chap_chunk = new Chunk("Персональные записи", fChapFont);
				chap_chunk.SetLocalDestination("IndividualRecords");
				fDocument.Add(new Paragraph(chap_chunk) { Alignment = 1, SpacingAfter = 20f });
				fDocument.Add(new Paragraph(Chunk.NEWLINE));

				SimpleColumnText columnText = new SimpleColumnText(fDocument, fWriter.DirectContent, 3, 10f);
				float pageWidth = fDocument.PageSize.Width - fDocument.LeftMargin - fDocument.RightMargin;
				float colWidth = (pageWidth - (10f * 2)) / 3;

				char sym = '!';
				int num = mainIndex.Count;
				for (int i = 0; i < num; i++)
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

				//SimpleColumnText columnText;
				if (!this.CatalogNewPages) {
					columnText = new SimpleColumnText(fDocument, fWriter.DirectContent, 3, 10f);
				}

				for (BookCatalog cat = BookCatalog.Catalog_First; cat <= BookCatalog.Catalog_Last; cat++)
				{
					CatalogProps catProps = BookCatalogs[(int)cat];
					
					if (!this.SkipEmptyCatalogs || catProps.Index.Count > 0) {
						if (this.CatalogNewPages) {
							fDocument.NewPage();
							columnText = new SimpleColumnText(fDocument, fWriter.DirectContent, 3, 10f);
						}

						this.ExposeCatalog(fDocument, columnText, catProps);
					}
				}
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
			reliIndex = new StringList();
			sourcesIndex = new StringList();
			
			GEDCOMRecord rec;

			var iEnum = this.fTree.GetEnumerator(GEDCOMRecordType.rtIndividual);
			while (iEnum.MoveNext(out rec))
			{
				GEDCOMIndividualRecord iRec = rec as GEDCOMIndividualRecord;
				string text = iRec.GetNameString(true, false);
                string st;

				mainIndex.AddObject(text, iRec);

				int ev_num = iRec.Events.Count;
				for (int k = 0; k < ev_num; k++)
				{
                    GEDCOMCustomEvent evt = iRec.Events[k];

                    if (evt != null)
					{
						int src_num2 = evt.Detail.SourceCitations.Count;
						for (int m = 0; m < src_num2; m++)
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
						else if (evt.Name == "RELI")
						{
							// Анализ по вероисповеданию
							st = evt.StringValue;
							if (!string.IsNullOrEmpty(st)) PrepareSpecIndex(reliIndex, st, iRec);
						}
					}
				}

				int src_num = iRec.SourceCitations.Count;
				for (int k = 0; k < src_num; k++)
				{
					GEDCOMSourceRecord src = iRec.SourceCitations[k].Value as GEDCOMSourceRecord;

					if (src != null) {
						st = src.FiledByEntry;
						if (string.IsNullOrEmpty(st)) st = src.Title.Text;
						PrepareSpecIndex(sourcesIndex, st, iRec);
					}
				}
			}

			mainIndex.Sort();
			
			BookCatalogs[(int)BookCatalog.Catalog_BirthYears].Index = byIndex;
			BookCatalogs[(int)BookCatalog.Catalog_DeathYears].Index = dyIndex;
			BookCatalogs[(int)BookCatalog.Catalog_BirthPlaces].Index = bpIndex;
			BookCatalogs[(int)BookCatalog.Catalog_DeathPlaces].Index = dpIndex;
			BookCatalogs[(int)BookCatalog.Catalog_DeathCauses].Index = deathCauses;
			BookCatalogs[(int)BookCatalog.Catalog_Occupations].Index = occuIndex;
			BookCatalogs[(int)BookCatalog.Catalog_Religion].Index = reliIndex;
			BookCatalogs[(int)BookCatalog.Catalog_Sources].Index = sourcesIndex;
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

			var bmp = this.fBase.Context.GetPrimaryBitmap(iRec, 0, 0, false);
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
			iRec.GetParents(out father, out mother);

			if (father != null) {
				pg = new Paragraph();
				chunk = new Chunk(father.GetNameString(true, false), fLinkFont);
				chunk.SetLocalGoto(father.XRef);
				pg.Add(new Chunk("Отец: ", fTextFont)); pg.Add(chunk);
				mct.AddElement(pg);
			}

			if (mother != null) {
				pg = new Paragraph();
				chunk = new Chunk(mother.GetNameString(true, false), fLinkFont);
				chunk.SetLocalGoto(mother.XRef);
				pg.Add(new Chunk("Мать: ", fTextFont)); pg.Add(chunk);
				mct.AddElement(pg);
			}

			if (this.IncludeEvents && iRec.Events.Count != 0)
			{
				int num = iRec.Events.Count;
				for (int i = 0; i < num; i++)
				{
					GEDCOMCustomEvent evt = iRec.Events[i];
					if (evt.Name == "BIRT" || evt.Name == "DEAT") continue;
					
					string evtName = GKUtils.GetIndividualEventName(evt);
					string evtVal = evt.StringValue;
					string evtDesc = GKUtils.GetEventDesc(evt, false);

					string tmp = evtName + ": " + evtVal;
					if (evtVal != "") tmp += ", ";
					tmp += evtDesc;

					mct.AddElement(new Paragraph(new Chunk(tmp, fTextFont)));
				}
			}

			if (this.IncludeNotes && iRec.Notes.Count != 0)
			{
				int num = iRec.Notes.Count;
				for (int i = 0; i < num; i++)
				{
					GEDCOMNotes note = iRec.Notes[i];
					mct.AddElement(new Paragraph(GKUtils.MergeStrings(note.Notes), fTextFont));
				}
			}
		}

		private void ExposeCatalog(Document document, SimpleColumnText columnText, CatalogProps catProps)
		{
			StringList index = catProps.Index;
			if (index == null) return;

			Chunk chunk = new Chunk(catProps.Title, fSubchapFont);
			chunk.SetLocalDestination(catProps.Sign);
			
			if (this.CatalogNewPages) {
				document.Add(new Paragraph(chunk));
				document.Add(new Paragraph(Chunk.NEWLINE));
			} else {
				columnText.AddElement(new Paragraph(chunk) { Alignment = 1 });
				columnText.AddElement(new Paragraph(Chunk.NEWLINE));
			}

			index.Sort();
			int num = index.Count;
			for (int i = 0; i < num; i++)
			{
				Paragraph ps = new Paragraph(new Chunk(index[i], fSymFont));
				ps.SpacingBefore = 0f;
				ps.SpacingAfter = 20f;
				//ps.Alignment = 1;
				ps.Add(Chunk.NEWLINE);
				columnText.AddElement(ps);

				StringList persons = index.GetObject(i) as StringList;

				persons.Sort();
				int num2 = persons.Count;
				for (int k = 0; k < num2; k++)
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
