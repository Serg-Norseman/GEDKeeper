using System;

using GedCom551;
using GKCore;
using GKCore.Interfaces;

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
			this.AddStatic(LangMan.LS(LSID.LSID_Goal), TDataType.dtString, 300, true);
			this.AddStatic(LangMan.LS(LSID.LSID_Priority), TDataType.dtString, 90, true);
			this.AddStatic(LangMan.LS(LSID.LSID_StartDate), TDataType.dtString, 90, true);
			this.AddStatic(LangMan.LS(LSID.LSID_StopDate), TDataType.dtString, 90, true);
			this.AddStatic(LangMan.LS(LSID.LSID_Changed), TDataType.dtDateTime, 150, true);
		}

		public TTaskListColumns() : base()
		{
			InitData(typeof(TTaskColumnType));
		}
	}

	public sealed class TTaskListMan : TListManager
	{
		private TGEDCOMTaskRecord FRec;

		public override bool CheckFilter(ShieldState aShieldState)
		{
			bool res = (this.QuickFilter == "*" || IsMatchesMask(GKUtils.GetTaskGoalStr(this.FRec), this.QuickFilter));

			res = res && base.CheckNewFilter();

			return res;
		}

		public override void Fetch(TGEDCOMRecord aRec)
		{
			this.FRec = (aRec as TGEDCOMTaskRecord);
		}

		protected override object GetColumnValueEx(int colType, int colSubtype)
		{
			switch (colType) {
				case 0:
					return GKUtils.GetTaskGoalStr(this.FRec);
				case 1:
					return LangMan.LS(GKData.PriorityNames[(int)this.FRec.Priority]);
				case 2:
					return GKUtils.GEDCOMDateToStr(this.FRec.StartDate, TfmGEDKeeper.Instance.Options.DefDateFormat);
				case 3:
					return GKUtils.GEDCOMDateToStr(this.FRec.StopDate, TfmGEDKeeper.Instance.Options.DefDateFormat);
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
