using System;
using GKCommon.GEDCOM;
using GKCore.Interfaces;
using GKCore.Options;
using GKCore.Types;

namespace GKCore.Lists
{
	/// <summary>
	/// 
	/// </summary>
	public enum TaskColumnType
	{
		tctGoal,
		tctPriority,
		tctStartDate,
		tctStopDate,
		tctChangeDate
	}

	/// <summary>
	/// 
	/// </summary>
	public sealed class TaskListColumns : ListColumns
	{
		protected override void InitColumnStatics()
		{
			this.AddStatic(LSID.LSID_Goal, DataType.dtString, 300, true);
			this.AddStatic(LSID.LSID_Priority, DataType.dtString, 90, true);
			this.AddStatic(LSID.LSID_StartDate, DataType.dtString, 90, true);
			this.AddStatic(LSID.LSID_StopDate, DataType.dtString, 90, true);
			this.AddStatic(LSID.LSID_Changed, DataType.dtDateTime, 150, true);
		}

		public TaskListColumns() : base()
		{
			InitData(typeof(TaskColumnType));
		}
	}

	/// <summary>
	/// 
	/// </summary>
	public sealed class TaskListMan : ListManager
	{
		private GEDCOMTaskRecord fRec;

		public override bool CheckFilter(ShieldState shieldState)
		{
			bool res = (this.QuickFilter == "*" || IsMatchesMask(GKUtils.GetTaskGoalStr(this.fRec), this.QuickFilter));

			res = res && base.CheckCommonFilter();

			return res;
		}

		public override void Fetch(GEDCOMRecord aRec)
		{
			this.fRec = (aRec as GEDCOMTaskRecord);
		}

		protected override object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
		{
			switch (colType) {
				case 0:
					return GKUtils.GetTaskGoalStr(this.fRec);
				case 1:
					return LangMan.LS(GKData.PriorityNames[(int)this.fRec.Priority]);
				case 2:
					return GetDateValue(this.fRec.StartDate, isVisible);
				case 3:
					return GetDateValue(this.fRec.StopDate, isVisible);
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

		protected override ListColumns GetDefaultListColumns()
		{
			return new TaskListColumns();
		}

		public TaskListMan(GEDCOMTree tree) : base(tree)
		{
		}
	}
}
