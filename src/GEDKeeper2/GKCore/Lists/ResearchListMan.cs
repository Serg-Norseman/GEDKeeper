using System;
using GKCommon.GEDCOM;
using GKCore.Interfaces;
using GKCore.Types;

namespace GKCore.Lists
{
	/// <summary>
	/// 
	/// </summary>
	public enum ResearchColumnType
	{
		rctName,
		rctPriority,
		rctStatus,
		rctStartDate,
		rctStopDate,
		rctPercent,
		rctChangeDate
	}

	/// <summary>
	/// 
	/// </summary>
	public sealed class ResearchListColumns : ListColumns
	{
		protected override void InitColumnStatics()
		{
			this.AddStatic(LSID.LSID_Title, DataType.dtString, 300, true);
			this.AddStatic(LSID.LSID_Priority, DataType.dtString, 90, true);
			this.AddStatic(LSID.LSID_Status, DataType.dtString, 90, true);
			this.AddStatic(LSID.LSID_StartDate, DataType.dtString, 90, true);
			this.AddStatic(LSID.LSID_StopDate, DataType.dtString, 90, true);
			this.AddStatic(LSID.LSID_Percent, DataType.dtInteger, 90, true);
			this.AddStatic(LSID.LSID_Changed, DataType.dtDateTime, 150, true);
		}

		public ResearchListColumns() : base()
		{
			InitData(typeof(ResearchColumnType));
		}
	}

	/// <summary>
	/// 
	/// </summary>
	public sealed class ResearchListMan : ListManager
	{
		private GEDCOMResearchRecord fRec;

		public override bool CheckFilter(ShieldState shieldState)
		{
			bool res = (this.QuickFilter == "*" || IsMatchesMask(this.fRec.ResearchName, this.QuickFilter));

			res = res && base.CheckCommonFilter();

			return res;
		}

		public override void Fetch(GEDCOMRecord aRec)
		{
			this.fRec = (aRec as GEDCOMResearchRecord);
		}

		protected override object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
		{
			switch (colType) {
				case 0:
					return this.fRec.ResearchName;
				case 1:
					return LangMan.LS(GKData.PriorityNames[(int)this.fRec.Priority]);
				case 2:
					return LangMan.LS(GKData.StatusNames[(int)this.fRec.Status]);
				case 3:
					return GetDateValue(this.fRec.StartDate, isVisible);
				case 4:
					return GetDateValue(this.fRec.StopDate, isVisible);
				case 5:
					return this.fRec.Percent;
				case 6:
					return this.fRec.ChangeDate.ChangeDateTime;
				default:
					return null;
			}
		}

		public override Type GetColumnsEnum()
		{
			return typeof(ResearchColumnType);
		}

		protected override ListColumns GetDefaultListColumns()
		{
			return new ResearchListColumns();
		}

		public ResearchListMan(GEDCOMTree tree) : base(tree)
		{
		}
	}
}
