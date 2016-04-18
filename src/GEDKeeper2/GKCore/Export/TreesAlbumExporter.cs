using System;
using GKCommon;
using GKCommon.GEDCOM;
using GKCore.Interfaces;
using GKCore.Types;
using iTextSharp.text;
using iTextSharp.text.pdf;

namespace GKCore.Export
{
	public sealed class TreesAlbumExporter : PDFExporter
	{
		private GEDCOMIndividualRecord fAncestor;
		private ShieldState fShieldState;
		private StringList fPatList;

		private Font fTitleFont;
		//private Font fChapFont;
		//private Font fPersonFont;
		//private Font fLinkFont;
		private Font fTextFont;
	    //private Font fSupText;

	    public GEDCOMIndividualRecord Ancestor
		{
			get { return this.fAncestor; }
			set { this.fAncestor = value; }
		}

		public ShieldState ShieldState
		{
			get { return this.fShieldState; }
			set { this.fShieldState = value; }
		}

		public TreesAlbumExporter(IBaseWindow aBase) : base(aBase)
		{
			this.fAlbumPage = true;
			this.fPatList = new StringList();
		}

		protected override void InternalGenerate()
		{
			try
			{
				//fFormat = this.fOptions.PedigreeOptions.Format;

				if (this.fAncestor == null)
				{
					GKUtils.ShowError(LangMan.LS(LSID.LSID_NotSelectedPerson));
				}
				else
				{
					string title = LangMan.LS(LSID.LSID_ExpPedigree) + ": " + this.fAncestor.GetNameString(true, false);

					fDocument.AddTitle("Pedigree");
					fDocument.AddSubject("Pedigree");
					fDocument.AddAuthor("");
					fDocument.AddCreator(GKData.APP_TITLE);
					fDocument.Open();

					BaseFont baseFont = BaseFont.CreateFont(Environment.ExpandEnvironmentVariables(@"%systemroot%\fonts\Times.ttf"), BaseFont.IDENTITY_H, BaseFont.EMBEDDED);
					fTitleFont = new Font(baseFont, 20f, Font.BOLD);
					//fChapFont = new Font(baseFont, 16f, Font.BOLD, BaseColor.BLACK);
					//fPersonFont = new Font(baseFont, 10f, Font.BOLD, BaseColor.BLACK);
					//fLinkFont = new Font(baseFont, 8f, Font.UNDERLINE, BaseColor.BLUE);
					fTextFont = new Font(baseFont, 8f, Font.NORMAL, BaseColor.BLACK);
					//fSupText = new Font(baseFont, 5f, Font.NORMAL, BaseColor.BLUE);

					fDocument.Add(new Paragraph(title, fTitleFont) { Alignment = Element.ALIGN_CENTER, SpacingAfter = 6f });

					this.PreparePatriarchs();
					
					int num = this.fPatList.Count;
					for (int i = 0; i < num; i++) {
						String iName = this.fPatList[i];
						//GEDCOMIndividualRecord iRec = this.fPatList.GetObject(i) as GEDCOMIndividualRecord;
						fDocument.Add(new Paragraph(iName, fTextFont) { Alignment = Element.ALIGN_LEFT, SpacingBefore = 2f, SpacingAfter = 2f });
					}
					
					fDocument.NewPage();
					
					//GEDCOMIndividualRecord iRec = this.fPatList.GetObject(0) as GEDCOMIndividualRecord;
					
					/*this.fPersonList = new ExtList<PedigreePerson>(true);
					this.fSourceList = new StringList();
					try
					{
						this.GenStep(null, this.fAncestor, 1, 1);
						this.ReIndex();

						int curLevel = 0;
						int num = this.fPersonList.Count - 1;
						for (int i = 0; i <= num; i++)
						{
							PedigreePerson person = this.fPersonList[i];
							if (curLevel != person.Level)
							{
								curLevel = person.Level;
								string genTitle = LangMan.LS(LSID.LSID_Generation)+" "+RomeNumbers.GetRome(curLevel);
								fDocument.Add(new Paragraph(genTitle, fChapFont) { Alignment = Element.ALIGN_LEFT, SpacingBefore = 2f, SpacingAfter = 2f });
							}

							this.WritePerson(person);
						}

						if (this.fSourceList.Count > 0)
						{
							fDocument.Add(new Paragraph(LangMan.LS(LSID.LSID_RPSources), fChapFont) { Alignment = Element.ALIGN_CENTER });

							int num2 = this.fSourceList.Count - 1;
							for (int j = 0; j <= num2; j++)
							{
								string sn = (j + 1).ToString();
								Chunk chunk = new Chunk(sn + ". " + this.fSourceList[j], fTextFont);
								chunk.SetLocalDestination("src_" + sn);
								fDocument.Add(new Paragraph(chunk));
							}
						}
					}
					finally
					{
                        this.fSourceList.Dispose();
						this.fPersonList.Dispose();
					}*/
				}
			}
			catch (Exception)
			{
				throw;
			}
		}
		
		private void PreparePatriarchs()
		{
			using (ExtList<PatriarchObj> lst = this.fBase.Context.GetPatriarchsList(3, false))
			{
				int num = lst.Count;
				for (int i = 0; i < num; i++) {
					PatriarchObj pObj = lst[i];

					this.fPatList.AddObject(pObj.IRec.GetNameString(true, false), pObj.IRec);
				}

				this.fPatList.Sort();
			}
		}
	}
}
