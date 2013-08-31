using System;

using GedCom551;
using GKCore;

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

	public sealed class TResearchListColumns : TListColumns
	{
		protected override void InitColumnStatics()
		{
			this.AddStatic(LangMan.LSList[125], TDataType.dtString, 300, true);
			this.AddStatic(LangMan.LSList[178], TDataType.dtString, 90, true);
			this.AddStatic(LangMan.LSList[117], TDataType.dtString, 90, true);
			this.AddStatic(LangMan.LSList[180], TDataType.dtString, 90, true);
			this.AddStatic(LangMan.LSList[181], TDataType.dtString, 90, true);
			this.AddStatic(LangMan.LSList[179], TDataType.dtInteger, 90, true);
			this.AddStatic(LangMan.LSList[317], TDataType.dtDateTime, 150, true);
		}

		public TResearchListColumns() : base()
		{
			InitData(typeof(TResearchColumnType));
		}
	}

	public sealed class TResearchListMan : TListManager
	{
		private TGEDCOMResearchRecord FRec;

		public override bool CheckFilter(TShieldState aShieldState)
		{
			bool res = (this.QuickFilter == "*" || IsMatchesMask(this.FRec.ResearchName, this.QuickFilter));

			res = res && base.CheckNewFilter();

			return res;
		}

		public override void Fetch(TGEDCOMRecord aRec)
		{
			this.FRec = (aRec as TGEDCOMResearchRecord);
		}

		protected override object GetColumnValueEx(int col_type, int col_subtype)
		{
			switch (col_type) {
				case 0:
					return this.FRec.ResearchName;
				case 1:
					return LangMan.LSList[(int)GKData.PriorityNames[(int)this.FRec.Priority] - 1];
				case 2:
					return LangMan.LSList[(int)GKData.StatusNames[(int)this.FRec.Status] - 1];
				case 3:
					return TGenEngine.GEDCOMDateToStr(this.FRec.StartDate, GKUI.TfmGEDKeeper.Instance.Options.DefDateFormat);
				case 4:
					return TGenEngine.GEDCOMDateToStr(this.FRec.StopDate, GKUI.TfmGEDKeeper.Instance.Options.DefDateFormat);
				case 5:
					return this.FRec.Percent;
				case 6:
					return this.FRec.ChangeDate.ChangeDateTime;
				default:
					return null;
			}
		}

		public override Type GetColumnsEnum()
		{
			return typeof(TResearchColumnType);
		}

		public override TListColumns GetDefaultListColumns()
		{
			return new TResearchListColumns();
		}

		public TResearchListMan(TGEDCOMTree aTree) : base(aTree)
		{
		}
	}
}
