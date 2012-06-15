using System;

using GedCom551;
using GKCore;
using GKUI.Controls;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKUI.Lists
{
	public enum TResearchColumnType : byte
	{
		rctName,
		rctPriority,
		rctStatus,
		rctStartDate,
		rctStopDate,
		rctPercent,
		rctChangeDate
	}

	public sealed class TResearchListMan : TListManager
	{
		private TGEDCOMResearchRecord FRec;

		public override bool CheckFilter(TPersonsFilter aFilter, TGenEngine.TShieldState aShieldState)
		{
			bool res = (this.QuickFilter == "*" || IsMatchesMask(this.FRec.ResearchName, this.QuickFilter));

			res = res && base.CheckNewFilter();

			return res;
		}

		public override void Fetch(TGEDCOMRecord aRec)
		{
			this.FRec = (aRec as TGEDCOMResearchRecord);
		}

		public override object GetColumnValueEx(int col_index)
		{
			switch (col_index) {
				case 1:
					return this.FRec.ResearchName;
				case 2:
					return LangMan.LSList[(int)TGenEngine.PriorityNames[(int)this.FRec.Priority] - 1];
				case 3:
					return LangMan.LSList[(int)TGenEngine.StatusNames[(int)this.FRec.Status] - 1];
				case 4:
					return TGenEngine.GEDCOMDateToStr(this.FRec.StartDate, GKUI.TfmGEDKeeper.Instance.Options.DefDateFormat);
				case 5:
					return TGenEngine.GEDCOMDateToStr(this.FRec.StopDate, GKUI.TfmGEDKeeper.Instance.Options.DefDateFormat);
				case 6:
					return this.FRec.Percent;
				case 7:
					return this.FRec.ChangeDate.ChangeDateTime;
				default:
					return null;
			}
		}

		protected override void InitColumnStatics()
		{
			this.ColumnStatics.Clear();
			this.ColumnStatics.Add(new TColumnStatic(LangMan.LSList[125], TDataType.dtString, 300));
			this.ColumnStatics.Add(new TColumnStatic(LangMan.LSList[178], TDataType.dtString, 90));
			this.ColumnStatics.Add(new TColumnStatic(LangMan.LSList[117], TDataType.dtString, 90));
			this.ColumnStatics.Add(new TColumnStatic(LangMan.LSList[180], TDataType.dtString, 90));
			this.ColumnStatics.Add(new TColumnStatic(LangMan.LSList[181], TDataType.dtString, 90));
			this.ColumnStatics.Add(new TColumnStatic(LangMan.LSList[179], TDataType.dtInteger, 90));
			this.ColumnStatics.Add(new TColumnStatic(LangMan.LSList[317], TDataType.dtDateTime, 150));
		}

		public override Type GetColumnsEnum()
		{
			return typeof(TResearchColumnType);
		}

		public override TListColumns GetDefaultListColumns()
		{
			return null;
		}

		public TResearchListMan(TGEDCOMTree aTree) : base(aTree)
		{
		}
	}
}
