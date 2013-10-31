using System;

using ExcelLibrary.SpreadSheet;
using Ext.Utils;
using GedCom551;
using GKUI;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKCore.Export
{
	public sealed class ExcelExporter : Exporter
	{
		private bool FAppMode;
		private TList FSelectedRecords;

		public bool AppMode
		{
			get { return this.FAppMode; }
			set { this.FAppMode = value; }
		}

		public TList SelectedRecords
		{
			get { return this.FSelectedRecords; }
			set { this.FSelectedRecords = value; }
		}

//		public override void Generate(bool show)
//		{
//			if (!this.IsRequireFilename("Excel files (*.xls)|*.xls")) return;
//
//			TXLSFile xls = new TXLSFile();
//			TfmProgress.ProgressInit(this.FTree.RecordsCount, LangMan.LSList[12] + "...");
//
//			TCellAttributeSet cas = (TCellAttributeSet.acBottomBorder | TCellAttributeSet.acTopBorder | TCellAttributeSet.acRightBorder | TCellAttributeSet.acLeftBorder);
//
//			try
//			{
//				xls.AddStrCell(1, 1, cas, "¹");
//				xls.AddStrCell(2, 1, cas, LangMan.LSList[84]);
//				xls.AddStrCell(3, 1, cas, LangMan.LSList[85]);
//				xls.AddStrCell(4, 1, cas, LangMan.LSList[86]);
//				xls.AddStrCell(5, 1, cas, LangMan.LSList[87]);
//				xls.AddStrCell(6, 1, cas, LangMan.LSList[122]);
//				xls.AddStrCell(7, 1, cas, LangMan.LSList[123]);
//				xls.AddStrCell(8, 1, cas, LangMan.LSList[302]);
//				xls.AddStrCell(9, 1, cas, LangMan.LSList[303]);
//				xls.AddStrCell(10, 1, cas, LangMan.LSList[304]);
//				xls.AddStrCell(11, 1, cas, LangMan.LSList[305]);
//				xls.AddStrCell(12, 1, cas, LangMan.LSList[306]);
//
//				ushort row = 1;
//				int num = this.FTree.RecordsCount - 1;
//				for (int i = 0; i <= num; i++)
//				{
//					TGEDCOMRecord rec = this.FTree[i];
//					if (rec is TGEDCOMIndividualRecord) {
//						TGEDCOMIndividualRecord ind = rec as TGEDCOMIndividualRecord;
//						if (this.FSelectedRecords == null || this.FSelectedRecords.IndexOf(rec) >= 0) {
//							string fam, nam, pat;
//							ind.aux_GetNameParts(out fam, out nam, out pat);
//
//							string sx = "" + GKUtils.SexStr(ind.Sex)[0];
//							row++;
//
//							xls.AddStrCell( 1, row, cas, ind.aux_GetXRefNum());
//							xls.AddStrCell( 2, row, cas, fam);
//							xls.AddStrCell( 3, row, cas, nam);
//							xls.AddStrCell( 4, row, cas, pat);
//							xls.AddStrCell( 5, row, cas, sx);
//							xls.AddStrCell( 6, row, cas, GKUtils.GetBirthDate(ind, TDateFormat.dfDD_MM_YYYY, false));
//							xls.AddStrCell( 7, row, cas, GKUtils.GetDeathDate(ind, TDateFormat.dfDD_MM_YYYY, false));
//							xls.AddStrCell( 8, row, cas, GKUtils.GetBirthPlace(ind));
//							xls.AddStrCell( 9, row, cas, GKUtils.GetDeathPlace(ind));
//							xls.AddStrCell(10, row, cas, GKUtils.GetResidencePlace(ind, this.FOptions.PlacesWithAddress));
//							xls.AddStrCell(11, row, cas, GKUtils.GetAge(ind, -1));
//							xls.AddStrCell(12, row, cas, GKUtils.GetLifeExpectancy(ind));
//						}
//					}
//					TfmProgress.ProgressStep();
//				}
//
//				xls.SaveToFile(this.FPath);
//
//				if (show) this.ShowResult();
//			}
//			finally
//			{
//				TfmProgress.ProgressDone();
//				xls.Dispose();
//			}
//		}

		public override void Generate(bool show)
		{
			if (!this.IsRequireFilename("Excel files (*.xls)|*.xls")) return;
			
			Workbook workbook = new Workbook();
			Worksheet worksheet = new Worksheet("First Sheet");

			TfmProgress.ProgressInit(this.FTree.RecordsCount, LangMan.LSList[12] + "...");

			//TCellAttributeSet cas = (TCellAttributeSet.acBottomBorder | TCellAttributeSet.acTopBorder | TCellAttributeSet.acRightBorder | TCellAttributeSet.acLeftBorder);

			try
			{
				worksheet.Cells[1,  1] = new Cell("¹");
				worksheet.Cells[1,  2] = new Cell(LangMan.LSList[84]);
				worksheet.Cells[1,  3] = new Cell(LangMan.LSList[85]);
				worksheet.Cells[1,  4] = new Cell(LangMan.LSList[86]);
				worksheet.Cells[1,  5] = new Cell(LangMan.LSList[87]);
				worksheet.Cells[1,  6] = new Cell(LangMan.LSList[122]);
				worksheet.Cells[1,  7] = new Cell(LangMan.LSList[123]);
				worksheet.Cells[1,  8] = new Cell(LangMan.LSList[302]);
				worksheet.Cells[1,  9] = new Cell(LangMan.LSList[303]);
				worksheet.Cells[1, 10] = new Cell(LangMan.LSList[304]);
				worksheet.Cells[1, 11] = new Cell(LangMan.LSList[305]);
				worksheet.Cells[1, 12] = new Cell(LangMan.LSList[306]);

				ushort row = 1;
				int num = this.FTree.RecordsCount - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMRecord rec = this.FTree[i];
					if (rec is TGEDCOMIndividualRecord) {
						TGEDCOMIndividualRecord ind = rec as TGEDCOMIndividualRecord;
						if (this.FSelectedRecords == null || this.FSelectedRecords.IndexOf(rec) >= 0) {
							string fam, nam, pat;
							ind.aux_GetNameParts(out fam, out nam, out pat);

							string sx = "" + GKUtils.SexStr(ind.Sex)[0];
							row++;

							worksheet.Cells[row, 1] = new Cell(ind.aux_GetXRefNum());
							worksheet.Cells[row, 2] = new Cell(fam);
							worksheet.Cells[row, 3] = new Cell(nam);
							worksheet.Cells[row, 4] = new Cell(pat);
							worksheet.Cells[row, 5] = new Cell(sx);
							worksheet.Cells[row, 6] = new Cell(GKUtils.GetBirthDate(ind, TDateFormat.dfDD_MM_YYYY, false));
							worksheet.Cells[row, 7] = new Cell(GKUtils.GetDeathDate(ind, TDateFormat.dfDD_MM_YYYY, false));
							worksheet.Cells[row, 8] = new Cell(GKUtils.GetBirthPlace(ind));
							worksheet.Cells[row, 9] = new Cell(GKUtils.GetDeathPlace(ind));
							worksheet.Cells[row,10] = new Cell(GKUtils.GetResidencePlace(ind, this.FOptions.PlacesWithAddress));
							worksheet.Cells[row,11] = new Cell(GKUtils.GetAge(ind, -1));
							worksheet.Cells[row,12] = new Cell(GKUtils.GetLifeExpectancy(ind));
						}
					}
					TfmProgress.ProgressStep();
				}

				workbook.Worksheets.Add(worksheet);
				workbook.Save(this.FPath);

				if (show) this.ShowResult();
			}
			finally
			{
				TfmProgress.ProgressDone();
			}
		}

		public ExcelExporter(TGEDCOMTree tree) : base(tree)
		{
		}
	}
}
