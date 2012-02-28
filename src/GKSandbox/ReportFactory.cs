using System;
using System.Collections.Generic;
using System.IO;
using System.Windows.Forms;

using GedCom551;
using GKCore;
using iTextSharp.text;
using iTextSharp.text.html;
using iTextSharp.text.pdf;
using iTextSharp.text.rtf;
using GKSys;

/// <summary>
/// Localization: unknown
/// CodeTransformation: need
/// </summary>

namespace GKSandbox
{
	public class ReportFactory
	{
		private TGEDCOMTree tree;
		private List<string> halfsiblings = new List<string>();
		private ReportProperties ReportProperties;

		public string Caption;
		public int PagesCreated;
		public string Message;
		public bool Success;

		private Font TableHeaderFont
		{
			get
			{
				return new Font(1, 12f, 1);
			}
		}

		private Font LinkFont
		{
			get
			{
				return new Font(1, 12f, 4, Color.BLUE);
			}
		}

		public ReportFactory(TGEDCOMTree dc, ReportProperties repProperties)
		{
			this.tree = dc;
			this.ReportProperties = repProperties;
			this.Caption = "";
		}

		private bool DoInsertImageFile(Document document, string sFilename, bool bInsertMsg)
		{
			bool result = false;
			/*try
			{
				if (!File.Exists(sFilename))
				{
					string text = "Unable to find '" + sFilename + "' in the current folder.\n\nWould you like to locate it?";
					if (SysUtils.Confirm(text))
					{
						sFilename = ExtUtils.GetFilenameToOpen(ExtUtils.FileType.Image);
					}
				}
				Image image = null;
				if (File.Exists(sFilename))
				{
					this.DoGetImageFile(sFilename, out image);
				}
				if (image != null)
				{
					document.Add(image);
					result = true;
				}
				else
				{
					if (bInsertMsg)
					{
						document.Add(new Paragraph(sFilename + " not found"));
					}
				}
			}
			catch (Exception)
			{
			}*/
			return result;
		}

		private bool DoGetImageFile(string sFilename, out Image img)
		{
			bool result = false;
			/*img = null;
			try
			{
				if (!File.Exists(sFilename))
				{
					string text = "Unable to find '" + sFilename + "' in the current folder.\n\nWould you like to locate it?";
					if (SysUtils.Confirm(text))
					{
						sFilename = ExtUtils.GetFilenameToOpen(ExtUtils.FileType.Image);
					}
				}
				if (File.Exists(sFilename))
				{
					img = Image.GetInstance(sFilename);
				}
				result = (img != null);
			}
			catch (Exception)
			{
			}*/
			img = null;
			return result;
		}

		public void AddTableHeaderCell(string content, Font font, Table table, int alignment)
		{
			Cell cell = new Cell();
			cell.Header = true;
			cell.Add(new Chunk(content, font));
			table.DefaultHorizontalAlignment = alignment;
			table.AddCell(cell);
		}

		public bool CreateReport_PersonsTable(string sFile)
		{
			Rectangle pageSize = ((this.ReportProperties.Landscape) ? PageSize.A4.Rotate() : PageSize.A4);

			Document document = new Document(pageSize, (float)this.ReportProperties.Margins.Left, (float)this.ReportProperties.Margins.Right, (float)this.ReportProperties.Margins.Top, (float)this.ReportProperties.Margins.Bottom);
			try
			{
				if (sFile.EndsWith(".pdf", StringComparison.OrdinalIgnoreCase))
				{
					PdfWriter.GetInstance(document, new FileStream(sFile, FileMode.Create));
				}
				else
				{
					if (sFile.EndsWith(".htm", StringComparison.OrdinalIgnoreCase))
					{
						HtmlWriter.GetInstance(document, new FileStream(sFile, FileMode.Create));
					}
					else
					{
						if (sFile.EndsWith(".rtf", StringComparison.OrdinalIgnoreCase))
						{
							RtfWriter.GetInstance(document, new FileStream(sFile, FileMode.Create));
						}
					}
				}

				BaseFont baseFont = BaseFont.CreateFont(Environment.ExpandEnvironmentVariables(@"%systemroot%\fonts\Calibri.ttf"), "CP1251", BaseFont.EMBEDDED);
				
    			Font font = new Font(baseFont, 8f, Font.NORMAL);
				Font font2 = new Font(baseFont, 8f, Font.BOLD, new Color(this.ReportProperties.Table.HeaderForeColor));
				Font font3 = new Font(baseFont, 8f, Font.NORMAL);

				document.Footer = new HeaderFooter(new Phrase("Page: ", font), true)
				{
					Border = 0, 
					Alignment = 1
				};

				document.AddTitle("Person report");
				document.AddSubject("All persons");
				document.AddAuthor("Ove Kjærstadbakk");
				document.AddCreator(TGenEngine.AppTitle);
				document.Open();
				/*document.Add(new Anchor("http://www.agetoage4.com", this.LinkFont)
				{
					Reference = "http://www.agetoage4.com"
				});*/

				if (!string.IsNullOrEmpty(this.Caption))
				{
					document.Add(new Paragraph(this.Caption, new Font(baseFont, 12f, Font.BOLD, Color.BLACK)));
				}

				Table table = new Table(/*this.ReportProperties.Table.fields2.Count*/7);
				table.WidthPercentage = 100f;
				table.TableFitsPage = true;
				//table.Widths = this.ReportProperties.Table.GetFields2Widths();
				table.DefaultVerticalAlignment = 5;
				table.Padding = this.ReportProperties.Table.Padding;
				table.Spacing = this.ReportProperties.Table.Spacing;
				table.Cellpadding = this.ReportProperties.Table.CellPadding;
				table.Cellspacing = this.ReportProperties.Table.CellSpacing;
				table.SpaceInsideCell = this.ReportProperties.Table.SpaceInsideCell;
				table.BorderColor = new Color(this.ReportProperties.Table.BorderColor);
				table.BorderWidth = this.ReportProperties.Table.BorderWidth;
				table.DefaultCellBackgroundColor = new Color(this.ReportProperties.Table.HeaderBackColor);

				AddTableHeaderCell("XRef", font2, table, 0);
				AddTableHeaderCell("Name", font2, table, 0);
				AddTableHeaderCell("Birt", font2, table, 0);
				AddTableHeaderCell("BirtPlace", font2, table, 0);
				AddTableHeaderCell("Deat", font2, table, 0);
				AddTableHeaderCell("DeatPlace", font2, table, 0);
				AddTableHeaderCell("Occu", font2, table, 0);

				if (this.ReportProperties.Table.ShowHeadersOnEveryPage)
				{
					table.EndHeaders();
				}
				table.DefaultVerticalAlignment = 1;

				int num = 1;

				var iEnum = this.tree.GetEnumerator(TGEDCOMRecordType.rtIndividual);
				TGEDCOMRecord rec;
				while (iEnum.MoveNext(out rec))
				{
					TGEDCOMIndividualRecord iRec = (TGEDCOMIndividualRecord)rec;

					if (num % 2 == 1)
					{
						table.DefaultCellGrayFill = 1f;
					}

					table.DefaultHorizontalAlignment = 0;

					table.AddCell(new Phrase(iRec.XRef, font3));
					table.AddCell(new Phrase(iRec.aux_GetNameStr(true, false), font3));

					table.AddCell(new Phrase(TGenEngine.GetBirthDate(iRec, TGenEngine.TDateFormat.dfDD_MM_YYYY, false), font3));
					table.AddCell(new Phrase(TGenEngine.GetBirthPlace(iRec), font3));

					table.AddCell(new Phrase(TGenEngine.GetDeathDate(iRec, TGenEngine.TDateFormat.dfDD_MM_YYYY, false), font3));
					table.AddCell(new Phrase(TGenEngine.GetDeathPlace(iRec), font3));

					TGEDCOMCustomEvent evt2 = iRec.GetIndividualEvent("OCCU");
					string st = ((evt2 == null) ? "" : evt2.StringValue);
					table.AddCell(new Phrase(st, font3));

					if (num % 2 == 1)
					{
						table.DefaultCellGrayFill = 0.9f;
					}
					this.PagesCreated = num;

					Application.DoEvents();
					num++;
				}

				document.Add(table);
				num--;
				document.Add(new Phrase("\nNumber of persons: " + num.ToString()));
				this.Success = true;
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

			if (this.Success)
			{
				this.Message = sFile + " has been created";
			}

			return this.Success;
		}

		private void GetPageData(Document document, TGEDCOMIndividualRecord p, TGEDCOMIndividualRecord pp)
		{
			TGEDCOMIndividualRecord person;
			TGEDCOMIndividualRecord person2;

			if (p.ChildToFamilyLinks.Count > 0)
			{
				TGEDCOMFamilyRecord fam = p.ChildToFamilyLinks[0].Value as TGEDCOMFamilyRecord;
				person = (TGEDCOMIndividualRecord)fam.Husband.Value;
				person2 = (TGEDCOMIndividualRecord)fam.Wife.Value;
			} else {
				person = null;
				person2 = null;
			}

			document.Add(new Paragraph(""));
			document.Add(new Paragraph("ID: " + p.XRef));
			document.Add(new Paragraph("Name: " + p.aux_GetNameStr(true, false)));
			if (p.Sex == TGEDCOMSex.svMale)
			{
				document.Add(new Paragraph("Sex: Male"));
			}
			else
			{
				document.Add(new Paragraph("Sex: Female"));
			}

			//document.Add(new Paragraph("Date of Birth: " + p.BirthDate));
			//document.Add(new Paragraph("Place of Birth: " + p.BirthPlace));
			//document.Add(new Paragraph("Date of Baptismal: " + p.BaptismDate));
			//document.Add(new Paragraph("Place of Baptismal: " + p.BaptismPlace));
			//document.Add(new Paragraph("Date of Death: " + p.DeathDate));
			//document.Add(new Paragraph("Place of Death: " + p.DeathPlace));
			//document.Add(new Paragraph("Address: " + p.Address));
			//document.Add(new Paragraph("Occupation: " + p.Occupation));
			//document.Add(new Paragraph("Photo: " + p.Photo));

			/*if (p.Photo != null && p.Photo.Count > 0)
			{
				string filename = this.doc.PathToPhotos + Path.DirectorySeparatorChar + p.Photo[0];
				if (File.Exists(filename))
				{
					Image instance = Image.GetInstance(filename);
					instance.ScaleAbsolute(100f, 100f);
					document.Add(instance);
				}
			}*/

			if (person != null) document.Add(new Paragraph("Father: " + person.aux_GetNameStr(true, false)));
			if (person2 != null) document.Add(new Paragraph("Mother: " + person2.aux_GetNameStr(true, false)));

			/*Family family = this.doc.GetFamily(p.FatherID, p.MotherID);
			if (family != null)
			{
				if (family.ChildCount > 1)
				{
					document.Add(new Paragraph("Siblings:"));
				}
				for (clsPersonRef clsPersonRef = family.GetFirstChild(); clsPersonRef != null; clsPersonRef = family.GetNextChild(clsPersonRef))
				{
					if (clsPersonRef.mp.ID != p.ID)
					{
						document.Add(new Paragraph("\t\t" + clsPersonRef.mp.GetFirstAndLastName()));
					}
				}
			}*/

			/*this.halfsiblings.Clear();
			if (person != null)
			{
				for (Marriage marriage = person.GetFirstMarriage(); marriage != null; marriage = person.GetNextMarriage(marriage))
				{
					if (marriage.PID != person2.ID)
					{
						Family family2 = this.doc.GetFamily(person.ID, marriage.PID);
						for (clsPersonRef clsPersonRef2 = family2.GetFirstChild(); clsPersonRef2 != null; clsPersonRef2 = family2.GetNextChild(clsPersonRef2))
						{
							this.halfsiblings.Add(clsPersonRef2.mp.GetFirstAndLastName());
						}
					}
				}
			}

			if (person2 != null)
			{
				for (Marriage marriage2 = person2.GetFirstMarriage(); marriage2 != null; marriage2 = person2.GetNextMarriage(marriage2))
				{
					if (marriage2.PID != person.ID)
					{
						Family family3 = this.doc.GetFamily(person2.ID, marriage2.PID);
						for (clsPersonRef clsPersonRef3 = family3.GetFirstChild(); clsPersonRef3 != null; clsPersonRef3 = family3.GetNextChild(clsPersonRef3))
						{
							this.halfsiblings.Add(clsPersonRef3.mp.GetFirstAndLastName());
						}
					}
				}
			}

			if (this.halfsiblings.Count > 0)
			{
				document.Add(new Paragraph("Half siblings:"));
				foreach (string current in this.halfsiblings)
				{
					document.Add(new Paragraph("\t\t" + current));
				}
			}

			for (Marriage marriage3 = p.GetFirstMarriage(); marriage3 != null; marriage3 = p.GetNextMarriage(marriage3))
			{
				family = this.doc.GetFamily(p.ID, marriage3.PID);
				clsPerson clsPerson = null;
				if (family.Male.ID == p.ID)
				{
					clsPerson = family.Female;
				}
				else
				{
					clsPerson = family.Male;
				}
				document.Add(new Paragraph(string.Concat(new string[]
				{
					"Married: ", 
					clsPerson.GetFirstAndLastName(), 
					" [", 
					clsPerson.ID.ToString(), 
					"]"
				})));
				clsPersonRef clsPersonRef4 = family.GetFirstChild();
				if (clsPersonRef4 != null)
				{
					document.Add(new Paragraph("Children:"));
				}
				while (clsPersonRef4 != null)
				{
					document.Add(new Paragraph("\t\t" + clsPersonRef4.mp.GetFirstAndLastName()));
					clsPersonRef4 = family.GetNextChild(clsPersonRef4);
				}
			}*/

			/*if (pp != null && p.ID != pp.ID)
			{
				document.Add(new Paragraph("Relation:"));
				RelationInfo relationInfo = new RelationInfo(this.doc, p, pp);
				relationInfo.CalculateRelation();
				document.Add(new Paragraph(relationInfo.RelationText));
				document.Add(new Paragraph("---------------"));
				if (relationInfo.CommonAncestor.Length > 0)
				{
					string text = p.GetFirstAndLastName();
					for (clsPersonRef clsPersonRef5 = (clsPersonRef)relationInfo.ancestorcoll1.Last; clsPersonRef5 != null; clsPersonRef5 = (clsPersonRef)relationInfo.ancestorcoll1.Prev(clsPersonRef5))
					{
						text = text + " - " + clsPersonRef5.mp.GetFirstAndLastName();
					}
					document.Add(new Paragraph(text));
					document.Add(new Paragraph("---------------"));
					text = pp.GetFirstAndLastName();
					for (clsPersonRef clsPersonRef5 = (clsPersonRef)relationInfo.ancestorcoll2.Last; clsPersonRef5 != null; clsPersonRef5 = (clsPersonRef)relationInfo.ancestorcoll2.Prev(clsPersonRef5))
					{
						text = text + " - " + clsPersonRef5.mp.GetFirstAndLastName();
					}
					document.Add(new Paragraph(text));
					document.Add(new Paragraph("---------------"));
				}
				document.Add(new Paragraph(""));
			}*/

			//document.Add(new Paragraph("Comments: " + p.GetCommentString()));

			document.NewPage();
		}

		/*private bool GetCommentsPageData(Document document, TGEDCOMIndividualRecord p, string sep)
		{
			Font font = new Font(2, 12f, 1);
			string commentString = p.GetCommentString();
			if (commentString.Length > 0)
			{
				document.Add(new Paragraph("ID: " + p.ID.ToString() + " - " + p.GetFirstAndLastName(), font));
				document.Add(new Paragraph(commentString));
				document.Add(new Paragraph(sep));
				return true;
			}
			return false;
		}*/

		/*private bool GetCommentsPageData(Document document, TGEDCOMFamilyRecord f, string sep)
		{
			Font font = new Font(2, 12f, 1);
			string commentString = f.GetCommentString();
			if (commentString.Length > 0)
			{
				Paragraph element = new Paragraph(f.GetLongFamilyName(true), font);
				document.Add(element);
				element = new Paragraph(commentString);
				document.Add(element);
				element = new Paragraph(sep);
				document.Add(element);
				return true;
			}
			return false;
		}*/

		private void AddChapter()
		{
			
		}

		private StringList persons;

		private void Prepare()
		{
			persons = new StringList();

			TGEDCOMRecord rec;
			var iEnum = this.tree.GetEnumerator(TGEDCOMRecordType.rtIndividual);
			while (iEnum.MoveNext(out rec))
			{
				TGEDCOMIndividualRecord iRec = (TGEDCOMIndividualRecord)rec;
				string text = iRec.aux_GetNameStr(true, false);
				persons.AddObject(text, iRec);
			}

			persons.Sort();
		}

		public bool CreateReport_FamilyBook(string filename)
		{
			Rectangle pageSize = ((this.ReportProperties.Landscape) ? PageSize.A4.Rotate() : PageSize.A4);
			Document document = new Document(pageSize, (float)this.ReportProperties.Margins.Left, (float)this.ReportProperties.Margins.Right, (float)this.ReportProperties.Margins.Top, (float)this.ReportProperties.Margins.Bottom);
			try
			{
				DocWriter writer = null;
				if (filename.EndsWith(".pdf", StringComparison.OrdinalIgnoreCase))
				{
					writer = PdfWriter.GetInstance(document, new FileStream(filename, FileMode.Create));
				}
				else
				{
					if (filename.EndsWith(".htm", StringComparison.OrdinalIgnoreCase))
					{
						writer = HtmlWriter.GetInstance(document, new FileStream(filename, FileMode.Create));
					}
					else
					{
						if (filename.EndsWith(".rtf", StringComparison.OrdinalIgnoreCase))
						{
							writer = RtfWriter.GetInstance(document, new FileStream(filename, FileMode.Create));
						}
					}
				}

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

				Prepare();

				char sym = '!';
				int num = persons.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					string text = persons[i];
					TGEDCOMIndividualRecord iRec = (TGEDCOMIndividualRecord)persons.GetObject(i);

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
					p.SpacingBefore = 0f;
					p.SpacingAfter = 15f;

					TGEDCOMIndividualRecord father, mother;
					iRec.aux_GetParents(out father, out mother);

					if (father != null) {
						p.Add(Chunk.NEWLINE);
						text = father.aux_GetNameStr(true, false);
						chunk = new Chunk(text, link_font);
						chunk.SetLocalGoto(father.XRef);
						p.Add("Отец: "); p.Add(chunk);
					}

					if (mother != null) {
						p.Add(Chunk.NEWLINE);
						text = mother.aux_GetNameStr(true, false);
						chunk = new Chunk(text, link_font);
						chunk.SetLocalGoto(mother.XRef);
						p.Add("Мать: "); p.Add(chunk);
					}

					p.Add(Chunk.NEWLINE);
					mct.AddElement(p);
					
					//chunk = new Chunk(text, subFont);
					//chunk.
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

				this.Success = true;
			}
			catch (DocumentException ex)
			{
				SysUtils.LogWrite("ReportFactory.CreateReport_FamilyBook(): " + ex.Message);
			}
			catch (IOException ex2)
			{
				this.Message = ex2.Message;
			}
			document.Close();

			if (this.Success) this.Message = filename + " has been created";

			return this.Success;
		}

		public bool CreateReport_PersonsPages(string sFile, string subject)
		{
			this.Success = false;
			TGEDCOMIndividualRecord person = (TGEDCOMIndividualRecord)this.tree.XRefIndex_Find(subject);

			Document document = new Document(PageSize.A4, (float)this.ReportProperties.Margins.Left, (float)this.ReportProperties.Margins.Right, (float)this.ReportProperties.Margins.Top, (float)this.ReportProperties.Margins.Bottom);
			try
			{
				if (sFile.EndsWith(".pdf", StringComparison.OrdinalIgnoreCase))
				{
					PdfWriter.GetInstance(document, new FileStream(sFile, FileMode.Create));
				}
				else
				{
					if (sFile.EndsWith(".htm", StringComparison.OrdinalIgnoreCase))
					{
						HtmlWriter.GetInstance(document, new FileStream(sFile, FileMode.Create));
					}
					else
					{
						if (sFile.EndsWith(".rtf", StringComparison.OrdinalIgnoreCase))
						{
							RtfWriter.GetInstance(document, new FileStream(sFile, FileMode.Create));
						}
					}
				}

				//Font font = FontFactory.GetFont("Helvetica", 13f, 1);

				HeaderFooter header = new HeaderFooter(new Phrase("AgetoAge4 - Person Report"), false);
				document.Header = header;
				document.Footer = new HeaderFooter(new Phrase("Page: "), true)
				{
					Border = 0
				};
				document.Open();

				for (int i = 0; i <= this.tree.RecordsCount - 1; i++)
				{
					TGEDCOMRecord rec = this.tree[i];
					if (rec.RecordType != TGEDCOMRecordType.rtIndividual) continue;

					TGEDCOMIndividualRecord p = (TGEDCOMIndividualRecord)rec;

					this.GetPageData(document, p, person);

					this.PagesCreated = document.PageNumber;
					Application.DoEvents();
				}

				this.Success = true;
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

			if (this.Success)
			{
				this.Message = sFile + " has been created";
			}

			return this.Success;
		}

		private Paragraph GetLinkParagraph(string reference)
		{
			// "http://www.agetoage4.com"
			return new Paragraph(new Anchor(reference, this.LinkFont) { Reference = reference }) { Alignment = 5 };
		}

	}
}
