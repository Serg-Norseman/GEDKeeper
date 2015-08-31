using System.Collections.Generic;
using ExcelLibrary.SpreadSheet;
using GKCommon.GEDCOM;
using GKCore.Interfaces;
using GKCore.Types;

namespace GKCore.Export
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class ExcelExporter : Exporter
	{
		private bool fAppMode;
		private List<GEDCOMRecord> fSelectedRecords;

		public bool AppMode
		{
			get { return this.fAppMode; }
			set { this.fAppMode = value; }
		}

		public List<GEDCOMRecord> SelectedRecords
		{
			get { return this.fSelectedRecords; }
			set { this.fSelectedRecords = value; }
		}

        public ExcelExporter(IBase aBase) : base(aBase)
        {
        }

		public override void Generate(bool show)
		{
			if (!this.IsRequireFilename("Excel files (*.xls)|*.xls")) return;
			
			Workbook workbook = new Workbook();
			Worksheet worksheet = new Worksheet("First Sheet");

			this.fBase.ProgressInit(LangMan.LS(LSID.LSID_MIExport) + "...", this.fTree.RecordsCount);

			//TCellAttributeSet cas = (TCellAttributeSet.acBottomBorder | TCellAttributeSet.acTopBorder | TCellAttributeSet.acRightBorder | TCellAttributeSet.acLeftBorder);

			try
			{
				worksheet.Cells[1,  1] = new Cell("¹");
				worksheet.Cells[1,  2] = new Cell(LangMan.LS(LSID.LSID_Surname));
				worksheet.Cells[1,  3] = new Cell(LangMan.LS(LSID.LSID_Name));
				worksheet.Cells[1,  4] = new Cell(LangMan.LS(LSID.LSID_Patronymic));
				worksheet.Cells[1,  5] = new Cell(LangMan.LS(LSID.LSID_Sex));
				worksheet.Cells[1,  6] = new Cell(LangMan.LS(LSID.LSID_BirthDate));
				worksheet.Cells[1,  7] = new Cell(LangMan.LS(LSID.LSID_DeathDate));
				worksheet.Cells[1,  8] = new Cell(LangMan.LS(LSID.LSID_BirthPlace));
				worksheet.Cells[1,  9] = new Cell(LangMan.LS(LSID.LSID_DeathPlace));
				worksheet.Cells[1, 10] = new Cell(LangMan.LS(LSID.LSID_Residence));
				worksheet.Cells[1, 11] = new Cell(LangMan.LS(LSID.LSID_Age));
				worksheet.Cells[1, 12] = new Cell(LangMan.LS(LSID.LSID_LifeExpectancy));

				ushort row = 1;
				int num = this.fTree.RecordsCount - 1;
				for (int i = 0; i <= num; i++)
				{
					GEDCOMRecord rec = this.fTree[i];
					if (rec is GEDCOMIndividualRecord) {
						GEDCOMIndividualRecord ind = rec as GEDCOMIndividualRecord;
						if (this.fSelectedRecords == null || this.fSelectedRecords.IndexOf(rec) >= 0) {
							string fam, nam, pat;
							ind.GetNameParts(out fam, out nam, out pat);

							string sx = "" + GKUtils.SexStr(ind.Sex)[0];
							row++;

							worksheet.Cells[row, 1] = new Cell(ind.GetXRefNum());
							worksheet.Cells[row, 2] = new Cell(fam);
							worksheet.Cells[row, 3] = new Cell(nam);
							worksheet.Cells[row, 4] = new Cell(pat);
							worksheet.Cells[row, 5] = new Cell(sx);
							worksheet.Cells[row, 6] = new Cell(GKUtils.GetBirthDate(ind, DateFormat.dfDD_MM_YYYY, false));
							worksheet.Cells[row, 7] = new Cell(GKUtils.GetDeathDate(ind, DateFormat.dfDD_MM_YYYY, false));
							worksheet.Cells[row, 8] = new Cell(GKUtils.GetBirthPlace(ind));
							worksheet.Cells[row, 9] = new Cell(GKUtils.GetDeathPlace(ind));
							worksheet.Cells[row,10] = new Cell(GKUtils.GetResidencePlace(ind, this.fOptions.PlacesWithAddress));
							worksheet.Cells[row,11] = new Cell(GKUtils.GetAge(ind, -1));
							worksheet.Cells[row,12] = new Cell(GKUtils.GetLifeExpectancy(ind));
						}
					}

					this.fBase.ProgressStep();
				}

				workbook.Worksheets.Add(worksheet);
				workbook.Save(this.fPath);

				if (show) this.ShowResult();
			}
			finally
			{
				this.fBase.ProgressDone();
			}
		}
	}
}
