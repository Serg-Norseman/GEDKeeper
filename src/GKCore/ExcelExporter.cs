using System;

using Ext.Utils;
using Ext.XLSFile;
using GedCom551;
using GKUI;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKCore
{
	public class ExcelExporter : Exporter
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
			TfmProgress.ProgressInit(this.FTree.RecordsCount, LangMan.LSList[12] + "...");

			TCellAttributeSet cas = (TCellAttributeSet.acBottomBorder | TCellAttributeSet.acTopBorder | TCellAttributeSet.acRightBorder | TCellAttributeSet.acLeftBorder);

			try
			{
				xls.AddStrCell(1, 1, cas, "¹");
				xls.AddStrCell(2, 1, cas, LangMan.LSList[84]);
				xls.AddStrCell(3, 1, cas, LangMan.LSList[85]);
				xls.AddStrCell(4, 1, cas, LangMan.LSList[86]);
				xls.AddStrCell(5, 1, cas, LangMan.LSList[87]);
				xls.AddStrCell(6, 1, cas, LangMan.LSList[122]);
				xls.AddStrCell(7, 1, cas, LangMan.LSList[123]);
				xls.AddStrCell(8, 1, cas, LangMan.LSList[302]);
				xls.AddStrCell(9, 1, cas, LangMan.LSList[303]);
				xls.AddStrCell(10, 1, cas, LangMan.LSList[304]);
				xls.AddStrCell(11, 1, cas, LangMan.LSList[305]);
				xls.AddStrCell(12, 1, cas, LangMan.LSList[306]);

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

							string sx = "" + TGenEngine.SexStr(ind.Sex)[0];
							row++;

							xls.AddStrCell( 1, row, cas, TGenEngine.GetId(ind).ToString());
							xls.AddStrCell( 2, row, cas, fam);
							xls.AddStrCell( 3, row, cas, nam);
							xls.AddStrCell( 4, row, cas, pat);
							xls.AddStrCell( 5, row, cas, sx);
							xls.AddStrCell( 6, row, cas, TGenEngine.GetBirthDate(ind, TGenEngine.TDateFormat.dfDD_MM_YYYY, false));
							xls.AddStrCell( 7, row, cas, TGenEngine.GetDeathDate(ind, TGenEngine.TDateFormat.dfDD_MM_YYYY, false));
							xls.AddStrCell( 8, row, cas, TGenEngine.GetBirthPlace(ind));
							xls.AddStrCell( 9, row, cas, TGenEngine.GetDeathPlace(ind));
							xls.AddStrCell(10, row, cas, TGenEngine.GetResidencePlace(ind, this.FOptions.PlacesWithAddress));
							xls.AddStrCell(11, row, cas, TGenEngine.GetAge(ind, -1));
							xls.AddStrCell(12, row, cas, TGenEngine.GetLifeExpectancy(ind));
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

		public ExcelExporter(TGenEngine aEngine, string aPath) : base(aEngine, aPath)
		{
		}
	}
}
