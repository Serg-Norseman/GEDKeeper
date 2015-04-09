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
	public enum ResearchColumnType : byte
	{
		rctName,
		rctPriority,
		rctStatus,
		rctStartDate,
		rctStopDate,
		rctPercent,
		rctChangeDate
	}

	public sealed class ResearchListColumns : ListColumns
	{
		protected override void InitColumnStatics()
		{
			this.AddStatic(LangMan.LS(LSID.LSID_Title), TDataType.dtString, 300, true);
			this.AddStatic(LangMan.LS(LSID.LSID_Priority), TDataType.dtString, 90, true);
			this.AddStatic(LangMan.LS(LSID.LSID_Status), TDataType.dtString, 90, true);
			this.AddStatic(LangMan.LS(LSID.LSID_StartDate), TDataType.dtString, 90, true);
			this.AddStatic(LangMan.LS(LSID.LSID_StopDate), TDataType.dtString, 90, true);
			this.AddStatic(LangMan.LS(LSID.LSID_Percent), TDataType.dtInteger, 90, true);
			this.AddStatic(LangMan.LS(LSID.LSID_Changed), TDataType.dtDateTime, 150, true);
		}

		public ResearchListColumns() : base()
		{
			InitData(typeof(ResearchColumnType));
		}
	}

	public sealed class ResearchListMan : ListManager
	{
		private GEDCOMResearchRecord fRec;

		public override bool CheckFilter(ShieldState aShieldState)
		{
			bool res = (this.QuickFilter == "*" || IsMatchesMask(this.fRec.ResearchName, this.QuickFilter));

			res = res && base.CheckNewFilter();

			return res;
		}

		public override void Fetch(GEDCOMRecord aRec)
		{
			this.fRec = (aRec as GEDCOMResearchRecord);
		}

		protected override object GetColumnValueEx(int colType, int colSubtype)
		{
			switch (colType) {
				case 0:
					return this.fRec.ResearchName;
				case 1:
					return LangMan.LS(GKData.PriorityNames[(int)this.fRec.Priority]);
				case 2:
					return LangMan.LS(GKData.StatusNames[(int)this.fRec.Status]);
				case 3:
					return GKUtils.GEDCOMDateToStr(this.fRec.StartDate, TfmGEDKeeper.Instance.Options.DefDateFormat);
				case 4:
					return GKUtils.GEDCOMDateToStr(this.fRec.StopDate, TfmGEDKeeper.Instance.Options.DefDateFormat);
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

		public override ListColumns GetDefaultListColumns()
		{
			return new ResearchListColumns();
		}

		public ResearchListMan(GEDCOMTree aTree) : base(aTree)
		{
		}
	}
}
