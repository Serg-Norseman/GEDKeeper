using System;

using GedCom551;
using GKCore;
using GKCore.Interfaces;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKUI.Lists
{
	public enum TCommunicationColumnType : byte
	{
		cctCommName,
		cctCorresponder,
		cctCommType,
		cctDate,		
		cctChangeDate
	}

	public sealed class TCommunicationListColumns : TListColumns
	{
		protected override void InitColumnStatics()
		{
			this.AddStatic(LangMan.LS(LSID.LSID_Theme), TDataType.dtString, 300, true);
			this.AddStatic(LangMan.LS(LSID.LSID_Corresponder), TDataType.dtString, 200, true);
			this.AddStatic(LangMan.LS(LSID.LSID_Type), TDataType.dtString, 90, true);
			this.AddStatic(LangMan.LS(LSID.LSID_Date), TDataType.dtString, 90, true);
			this.AddStatic(LangMan.LS(LSID.LSID_Changed), TDataType.dtDateTime, 150, true);
		}

		public TCommunicationListColumns() : base()
		{
			InitData(typeof(TCommunicationColumnType));
		}
	}

	public sealed class TCommunicationListMan : TListManager
	{
		private TGEDCOMCommunicationRecord FRec;

		public override bool CheckFilter(ShieldState aShieldState)
		{
			bool res = (this.QuickFilter == "*" || IsMatchesMask(this.FRec.CommName, this.QuickFilter));

			res = res && base.CheckNewFilter();

			return res;
		}

		public override void Fetch(TGEDCOMRecord aRec)
		{
			this.FRec = (aRec as TGEDCOMCommunicationRecord);
		}

		protected override object GetColumnValueEx(int colType, int colSubtype)
		{
			switch (colType) {
				case 0:
					return this.FRec.CommName;
				case 1:
					return GKUtils.GetCorresponderStr(this.fTree, this.FRec, false);
				case 2:
					return LangMan.LS(GKData.CommunicationNames[(int)this.FRec.CommunicationType]);
				case 3:
					return GKUtils.GEDCOMDateToStr(this.FRec.Date, TfmGEDKeeper.Instance.Options.DefDateFormat);
				case 4:
					return this.FRec.ChangeDate.ChangeDateTime;
				default:
					return null;
			}
		}

		public override Type GetColumnsEnum()
		{
			return typeof(TCommunicationColumnType);
		}

		public override TListColumns GetDefaultListColumns()
		{
			return new TCommunicationListColumns();
		}

		public TCommunicationListMan(TGEDCOMTree aTree) : base(aTree)
		{
		}
	}
}
