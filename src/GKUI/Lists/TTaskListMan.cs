using System;

using GedCom551;
using GKCore;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKUI.Lists
{
	public enum TTaskColumnType : byte
	{
		tctGoal,
		tctPriority,
		tctStartDate,
		tctStopDate,
		tctChangeDate
	}

	public sealed class TTaskListColumns : TListColumns
	{
		protected override void InitColumnStatics()
		{
			this.AddStatic(LangMan.LSList[182], TDataType.dtString, 300, true);
			this.AddStatic(LangMan.LSList[178], TDataType.dtString, 90, true);
			this.AddStatic(LangMan.LSList[180], TDataType.dtString, 90, true);
			this.AddStatic(LangMan.LSList[181], TDataType.dtString, 90, true);
			this.AddStatic(LangMan.LSList[317], TDataType.dtDateTime, 150, true);
		}

		public TTaskListColumns() : base()
		{
			InitData(typeof(TTaskColumnType));
		}
	}

	public sealed class TTaskListMan : TListManager
	{
		private TGEDCOMTaskRecord FRec;

		public override bool CheckFilter(TGenEngine.TShieldState aShieldState)
		{
			bool res = (this.QuickFilter == "*" || IsMatchesMask(TGenEngine.GetTaskGoalStr(this.FRec), this.QuickFilter));

			res = res && base.CheckNewFilter();

			return res;
		}

		public override void Fetch(TGEDCOMRecord aRec)
		{
			this.FRec = (aRec as TGEDCOMTaskRecord);
		}

		protected override object GetColumnValueEx(int col_type, int col_subtype)
		{
			switch (col_type) {
				case 0:
					return TGenEngine.GetTaskGoalStr(this.FRec);
				case 1:
					return LangMan.LSList[(int)TGenEngine.PriorityNames[(int)this.FRec.Priority] - 1];
				case 2:
					return TGenEngine.GEDCOMDateToStr(this.FRec.StartDate, GKUI.TfmGEDKeeper.Instance.Options.DefDateFormat);
				case 3:
					return TGenEngine.GEDCOMDateToStr(this.FRec.StopDate, GKUI.TfmGEDKeeper.Instance.Options.DefDateFormat);
				case 4:
					return this.FRec.ChangeDate.ChangeDateTime;
				default:
					return null;
			}
		}

		public override Type GetColumnsEnum()
		{
			return typeof(TTaskColumnType);
		}

		public override TListColumns GetDefaultListColumns()
		{
			return new TTaskListColumns();
		}

		public TTaskListMan(TGEDCOMTree aTree) : base(aTree)
		{
		}
	}
}
