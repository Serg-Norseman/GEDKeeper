using System;

using GedCom551;
using GKCore.Sys;
using GKUI;
using XLSFile;

namespace GKCore
{
	public class TExcelExporter : TExporter
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

		public override void Generate()
		{
			TXLSFile xls = new TXLSFile();
			TfmProgress.ProgressInit(this.FTree.RecordsCount, GKL.LSList[12] + "...");

			TCellAttributeSet cas = (TCellAttributeSet.acBottomBorder | TCellAttributeSet.acTopBorder | TCellAttributeSet.acRightBorder | TCellAttributeSet.acLeftBorder);

			try
			{
				xls.AddStrCell(1, 1, cas, "¹");
				xls.AddStrCell(2, 1, cas, GKL.LSList[84]);
				xls.AddStrCell(3, 1, cas, GKL.LSList[85]);
				xls.AddStrCell(4, 1, cas, GKL.LSList[86]);
				xls.AddStrCell(5, 1, cas, GKL.LSList[87]);
				xls.AddStrCell(6, 1, cas, GKL.LSList[122]);
				xls.AddStrCell(7, 1, cas, GKL.LSList[123]);
				xls.AddStrCell(8, 1, cas, GKL.LSList[302]);
				xls.AddStrCell(9, 1, cas, GKL.LSList[303]);
				xls.AddStrCell(10, 1, cas, GKL.LSList[304]);
				xls.AddStrCell(11, 1, cas, GKL.LSList[305]);
				xls.AddStrCell(12, 1, cas, GKL.LSList[306]);

				int row = 1;
				int num = this.FTree.RecordsCount - 1;
				for (int i = 0; i <= num; i++)
				{
					TGEDCOMRecord rec = this.FTree.GetRecord(i);
					if (rec is TGEDCOMIndividualRecord) {
						TGEDCOMIndividualRecord ind = (TGEDCOMIndividualRecord)rec;
						if (this.FSelectedRecords == null || this.FSelectedRecords.IndexOf(rec) >= 0) {
							string fam, nam, pat;
							TGenEngine.GetNameParts(ind, out fam, out nam, out pat);

							string sx = "" + TGenEngine.SexStr(ind.Sex)[0];
							row++;

							xls.AddStrCell(1, (ushort)row, cas, TGenEngine.GetId(ind).ToString());
							xls.AddStrCell(2, (ushort)row, cas, fam);
							xls.AddStrCell(3, (ushort)row, cas, nam);
							xls.AddStrCell(4, (ushort)row, cas, pat);
							xls.AddStrCell(5, (ushort)row, cas, sx);
							xls.AddStrCell(6, (ushort)row, cas, TGenEngine.GetBirthDate(ind, TGenEngine.TDateFormat.dfDD_MM_YYYY, false));
							xls.AddStrCell(7, (ushort)row, cas, TGenEngine.GetDeathDate(ind, TGenEngine.TDateFormat.dfDD_MM_YYYY, false));
							xls.AddStrCell(8, (ushort)row, cas, TGenEngine.GetBirthPlace(ind));
							xls.AddStrCell(9, (ushort)row, cas, TGenEngine.GetDeathPlace(ind));
							xls.AddStrCell(10, (ushort)row, cas, TGenEngine.GetResidencePlace(ind, this.FOptions.PlacesWithAddress));
							xls.AddStrCell(11, (ushort)row, cas, TGenEngine.GetAge(ind, -1));
							xls.AddStrCell(12, (ushort)row, cas, TGenEngine.GetLifeExpectancy(ind));
						}
					}
					TfmProgress.ProgressStep();
				}

				string fname = this.FPath + "export.xls";
				xls.SaveToFile(fname);
				SysUtils.LoadExtFile(fname);
			}
			finally
			{
				TfmProgress.ProgressDone();
				xls.Dispose();
			}
		}

		public TExcelExporter(TGenEngine aEngine, string aPath) : base(aEngine, aPath)
		{
		}
	}
}
