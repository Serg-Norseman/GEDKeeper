using System;

using GedCom551;
using GKCore;
using GKCore.Interfaces;

/// <summary>
/// 
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
			this.AddStatic(LangMan.LS(LSID.LSID_Title), TDataType.dtString, 300, true);
			this.AddStatic(LangMan.LS(LSID.LSID_Priority), TDataType.dtString, 90, true);
			this.AddStatic(LangMan.LS(LSID.LSID_Status), TDataType.dtString, 90, true);
			this.AddStatic(LangMan.LS(LSID.LSID_StartDate), TDataType.dtString, 90, true);
			this.AddStatic(LangMan.LS(LSID.LSID_StopDate), TDataType.dtString, 90, true);
			this.AddStatic(LangMan.LS(LSID.LSID_Percent), TDataType.dtInteger, 90, true);
			this.AddStatic(LangMan.LS(LSID.LSID_Changed), TDataType.dtDateTime, 150, true);
		}

		public TResearchListColumns() : base()
		{
			InitData(typeof(TResearchColumnType));
		}
	}

	public sealed class TResearchListMan : TListManager
	{
		private TGEDCOMResearchRecord FRec;

		public override bool CheckFilter(ShieldState aShieldState)
		{
			bool res = (this.QuickFilter == "*" || IsMatchesMask(this.FRec.ResearchName, this.QuickFilter));

			res = res && base.CheckNewFilter();

			return res;
		}

		public override void Fetch(TGEDCOMRecord aRec)
		{
			this.FRec = (aRec as TGEDCOMResearchRecord);
		}

		protected override object GetColumnValueEx(int colType, int colSubtype)
		{
			switch (colType) {
				case 0:
					return this.FRec.ResearchName;
				case 1:
					return LangMan.LS(GKData.PriorityNames[(int)this.FRec.Priority]);
				case 2:
					return LangMan.LS(GKData.StatusNames[(int)this.FRec.Status]);
				case 3:
					return GKUtils.GEDCOMDateToStr(this.FRec.StartDate, TfmGEDKeeper.Instance.Options.DefDateFormat);
				case 4:
					return GKUtils.GEDCOMDateToStr(this.FRec.StopDate, TfmGEDKeeper.Instance.Options.DefDateFormat);
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
