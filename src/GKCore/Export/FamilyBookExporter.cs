using System;
using System.IO;

using Ext.Utils;
using GedCom551;
using iTextSharp.text;
using iTextSharp.text.pdf;

/// <summary>
/// Localization: dirty
/// CodeTransformation: need
/// </summary>

namespace GKCore.Export
{
	public sealed class FamilyBookExporter : PDFExporter
	{
		private Font title_font;
		private Font chap_font;
		private Font subchap_font;
		private Font link_font;
		private Font text_font;
		private Font sym_font;

		private StringList mainIndex;
		private StringList byIndex, dyIndex, bpIndex, dpIndex;
		private StringList deathCauses, occuIndex, sourcesIndex;

		public FamilyBookExporter(TGenEngine engine) : base(engine)
		{
			this.albumPage = true;
		}
		
		protected override void InternalGenerate()
		{
			try
			{
				fDocument.AddTitle("FamilyBook");
				fDocument.AddSubject("FamilyBook");
				fDocument.AddAuthor("");
				fDocument.AddCreator(TGenEngine.AppTitle);
				fDocument.Open();

				BaseFont base_font = BaseFont.CreateFont(Environment.ExpandEnvironmentVariables(@"%systemroot%\fonts\Times.ttf"), "CP1251", BaseFont.EMBEDDED);
				title_font = new Font(base_font, 30f, Font.BOLD);
				chap_font = new Font(base_font, 16f, Font.BOLD, Color.BLACK);
				subchap_font = new Font(base_font, 14f, Font.BOLD, Color.BLACK);
				link_font = new Font(base_font, 8f, Font.UNDERLINE, Color.BLUE);
				text_font = new Font(base_font, 8f, Font.NORMAL, Color.BLACK);
				sym_font = new Font(base_font, 16f, Font.BOLD, Color.BLACK);

				base_font = BaseFont.CreateFont(Environment.ExpandEnvironmentVariables(@"%systemroot%\fonts\Calibri.ttf"), "CP1251", BaseFont.EMBEDDED);
				Font page_font = new Font(base_font, 9f, Font.NORMAL);
				HeaderFooter footer = new HeaderFooter(new Phrase("Страница: ", page_font), true) { Border = 0, Alignment = 2 };
				fDocument.Footer = footer;

				float halfpage = (fDocument.Top - fDocument.Bottom - (title_font.Size + footer.Height) * 4) / 2f;
				fDocument.Add(new Paragraph(Chunk.NEWLINE) { SpacingAfter = halfpage });
				fDocument.Add(new Paragraph("Фамильная книга", title_font) { Alignment = Element.ALIGN_CENTER });

				//document.Add(new Chunk("Chapter 1"));
				//document.Add(new Paragraph(new Chunk("Press here to go chapter 2", font2).SetLocalGoto("2")));// Code 2
				//document.NewPage();
				//document.Add(new Paragraph(new Chunk("http://www.geek-tutorials.com", font2).SetAnchor("http://www.geek-tutorials.com")));//Code 3
				//document.Add(new Paragraph(new Chunk("Open outline.pdf chapter 3", font2).SetRemoteGoto("outline.pdf", "3")));//Code 4
				//document.NewPage();

				fDocument.NewPage();

				Chunk chap_chunk = new Chunk("1. Персональные записи", link_font);
				chap_chunk.SetLocalGoto("IndividualRecords");
				fDocument.Add(new Paragraph(chap_chunk));

				chap_chunk = new Chunk("2. Каталоги", link_font);
				chap_chunk.SetLocalGoto("Catalogs");
				fDocument.Add(new Paragraph(chap_chunk));

				chap_chunk = new Chunk("2.1. Места", link_font);
				chap_chunk.SetLocalGoto("Catalog_Places");
				fDocument.Add(new Paragraph(chap_chunk) { IndentationLeft = 1f });

				chap_chunk = new Chunk("2.2. Годы рождения", link_font);
				chap_chunk.SetLocalGoto("Catalog_BirthYears");
				fDocument.Add(new Paragraph(chap_chunk) { IndentationLeft = 1f });

				chap_chunk = new Chunk("2.3. Годы смерти", link_font);
				chap_chunk.SetLocalGoto("Catalog_DeathYears");
				fDocument.Add(new Paragraph(chap_chunk) { IndentationLeft = 1f });

				fDocument.NewPage();

				chap_chunk = new Chunk("Персональные записи", chap_font);
				chap_chunk.SetLocalDestination("IndividualRecords");
				fDocument.Add(new Paragraph(chap_chunk) { Alignment = 1 });
				fDocument.Add(new Paragraph(Chunk.NEWLINE));

				MultiColumnText mct = new MultiColumnText();
				mct.AddRegularColumns(fDocument.Left, fDocument.Right, 10f, 3);
				
				this.PrepareData();

				char sym = '!';
				int num = mainIndex.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					string text = mainIndex[i];
					TGEDCOMIndividualRecord iRec = mainIndex.GetObject(i) as TGEDCOMIndividualRecord;

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

					this.ExposePerson(mct, iRec, text, text_font, link_font);

					mct.AddElement(new Paragraph(Chunk.NEWLINE) { SpacingAfter = 10f });
				}

				fDocument.Add(mct);
				fDocument.NewPage();

				chap_chunk = new Chunk("Каталоги", chap_font);
				chap_chunk.SetLocalDestination("Catalogs");
				fDocument.Add(new Paragraph(chap_chunk) { Alignment = 1 });
				fDocument.Add(new Paragraph(Chunk.NEWLINE));

				chap_chunk = new Chunk("2.1. Места", subchap_font);
				chap_chunk.SetLocalDestination("Catalog_Places");
				fDocument.Add(new Paragraph(chap_chunk));
				fDocument.Add(new Paragraph(Chunk.NEWLINE));

				fDocument.NewPage();
				this.ExposeCatalog(fDocument, byIndex, "2.2. Годы рождения", "Catalog_BirthYears");

				fDocument.NewPage();
				this.ExposeCatalog(fDocument, dyIndex, "2.3. Годы смерти", "Catalog_DeathYears");

				fDocument.NewPage();
				this.ExposeCatalog(fDocument, bpIndex, "2.4. Места рождения", "Catalog_BirthPlaces");

				fDocument.NewPage();
				this.ExposeCatalog(fDocument, dpIndex, "2.5. Места смерти", "Catalog_DeathPlaces");

				fDocument.NewPage();
				this.ExposeCatalog(fDocument, deathCauses, "2.6. Причины смерти", "Catalog_DeathCauses");

				fDocument.NewPage();
				this.ExposeCatalog(fDocument, occuIndex, "2.7. Профессии", "Catalog_Occupations");

				fDocument.NewPage();
				this.ExposeCatalog(fDocument, sourcesIndex, "2.8. Источники", "Catalog_Sources");
			}
			catch (Exception ex)
			{
				SysUtils.LogWrite("FamilyBookExporter.InternalGenerate(): " + ex.Message);
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
			
			TGEDCOMRecord rec;
			TGEDCOMCustomEvent evt;
			string st;

			var iEnum = this.FTree.GetEnumerator(TGEDCOMRecordType.rtIndividual);
			while (iEnum.MoveNext(out rec))
			{
				TGEDCOMIndividualRecord iRec = rec as TGEDCOMIndividualRecord;
				string text = iRec.aux_GetNameStr(true, false);

				mainIndex.AddObject(text, iRec);

				int ev_num = iRec.IndividualEvents.Count - 1;
				for (int k = 0; k <= ev_num; k++) 
				{
					evt = iRec.IndividualEvents[k];
					if (evt != null)
					{
						int src_num2 = evt.Detail.SourceCitations.Count - 1;
						for (int m = 0; m <= src_num2; m++)
						{
							TGEDCOMSourceRecord src = evt.Detail.SourceCitations[m].Value as TGEDCOMSourceRecord;
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
							st = TGenEngine.GetPlaceStr(evt, false);
							if (!string.IsNullOrEmpty(st)) PrepareSpecIndex(bpIndex, st, iRec);
						} 
						else if (evt.Name == "DEAT")
						{
							// Анализ по причинам смерти
							Exporter.PrepareEventYear(dyIndex, evt, iRec);
							st = TGenEngine.GetPlaceStr(evt, false);
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
					TGEDCOMSourceRecord src = iRec.SourceCitations[k].Value as TGEDCOMSourceRecord;
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

		private void ExposePerson(MultiColumnText mct, TGEDCOMIndividualRecord iRec, string iName, Font text_font, Font link_font)
		{
			Chunk chunk = new Chunk(iName, text_font);
			chunk.SetLocalDestination(iRec.XRef);
			Paragraph p = new Paragraph(chunk);
			mct.AddElement(p);

			Stream portrait_stm = this.FEngine.GetPrimaryBitmapStream(iRec);
			if (portrait_stm != null) {
				iTextSharp.text.Image img = iTextSharp.text.Image.GetInstance(portrait_stm);
				img.WidthPercentage = 40f;
				//img.Alignment = Image.ALIGN_RIGHT | Image.TEXTWRAP;
				img.IndentationLeft = 5f;
				img.SpacingBefore = 5f;
				img.SpacingAfter = 5f;

				mct.AddElement(img);
			}

			TGEDCOMIndividualRecord father, mother;
			iRec.aux_GetParents(out father, out mother);
			string text;

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

		private void ExposeCatalog(Document document, StringList index, string title, string anchor)
		{
			Chunk chunk = new Chunk(title, subchap_font);
			chunk.SetLocalDestination(anchor);
			document.Add(new Paragraph(chunk));
			document.Add(new Paragraph(Chunk.NEWLINE));

			MultiColumnText mct = new MultiColumnText();
			mct.AddRegularColumns(document.Left, document.Right, 10f, 3);
			
			for (int i = 0; i < index.Count; i++)
			{
				Paragraph ps = new Paragraph(new Chunk(index[i], sym_font));
				ps.SpacingBefore = 0f;
				ps.SpacingAfter = 20f;
				ps.Alignment = 1;
				ps.Add(Chunk.NEWLINE);
				mct.AddElement(ps);

				StringList persons = index.GetObject(i) as StringList;
				persons.Sort();

				for (int k = 0; k < persons.Count; k++)
				{
					TGEDCOMIndividualRecord iRec = persons.GetObject(k) as TGEDCOMIndividualRecord;
					
					chunk = new Chunk(persons[k], text_font);
					chunk.SetLocalGoto(iRec.XRef);
					Paragraph p = new Paragraph(chunk);
					mct.AddElement(p);
				}

				mct.AddElement(new Paragraph(Chunk.NEWLINE) { SpacingAfter = 10f });
			}

			document.Add(mct);
		}
	}
}
