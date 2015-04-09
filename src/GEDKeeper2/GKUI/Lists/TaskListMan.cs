using System;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKCore.Types;

/// <summary>
/// 
/// </summary>

namespace GKUI.Lists
{
	public enum TaskColumnType : byte
	{
		tctGoal,
		tctPriority,
		tctStartDate,
		tctStopDate,
		tctChangeDate
	}

	public sealed class TaskListColumns : ListColumns
	{
		protected override void InitColumnStatics()
		{
			this.AddStatic(LangMan.LS(LSID.LSID_Goal), TDataType.dtString, 300, true);
			this.AddStatic(LangMan.LS(LSID.LSID_Priority), TDataType.dtString, 90, true);
			this.AddStatic(LangMan.LS(LSID.LSID_StartDate), TDataType.dtString, 90, true);
			this.AddStatic(LangMan.LS(LSID.LSID_StopDate), TDataType.dtString, 90, true);
			this.AddStatic(LangMan.LS(LSID.LSID_Changed), TDataType.dtDateTime, 150, true);
		}

		public TaskListColumns() : base()
		{
			InitData(typeof(TaskColumnType));
		}
	}

	public sealed class TaskListMan : ListManager
	{
		private GEDCOMTaskRecord fRec;

		public override bool CheckFilter(ShieldState aShieldState)
		{
			bool res = (this.QuickFilter == "*" || IsMatchesMask(GKUtils.GetTaskGoalStr(this.fRec), this.QuickFilter));

			res = res && base.CheckNewFilter();

			return res;
		}

		public override void Fetch(GEDCOMRecord aRec)
		{
			this.fRec = (aRec as GEDCOMTaskRecord);
		}

		protected override object GetColumnValueEx(int colType, int colSubtype)
		{
			switch (colType) {
				case 0:
					return GKUtils.GetTaskGoalStr(this.fRec);
				case 1:
					return LangMan.LS(GKData.PriorityNames[(int)this.fRec.Priority]);
				case 2:
					return GKUtils.GEDCOMDateToStr(this.fRec.StartDate, TfmGEDKeeper.Instance.Options.DefDateFormat);
				case 3:
					return GKUtils.GEDCOMDateToStr(this.fRec.StopDate, TfmGEDKeeper.Instance.Options.DefDateFormat);
				case 4:
					return this.fRec.ChangeDate.ChangeDateTime;
				default:
					return null;
			}
		}

		public override Type GetColumnsEnum()
		{
			return typeof(TaskColumnType);
		}

		public override ListColumns GetDefaultListColumns()
		{
			return new TaskListColumns();
		}

		public TaskListMan(GEDCOMTree aTree) : base(aTree)
		{
		}
	}
}
