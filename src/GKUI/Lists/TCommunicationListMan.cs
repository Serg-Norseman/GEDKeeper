using System;

using GedCom551;
using GKCore;

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
			this.AddStatic(LangMan.LSList[183], TDataType.dtString, 300, true);
			this.AddStatic(LangMan.LSList[184], TDataType.dtString, 200, true);
			this.AddStatic(LangMan.LSList[113], TDataType.dtString, 90, true);
			this.AddStatic(LangMan.LSList[139], TDataType.dtString, 90, true);
			this.AddStatic(LangMan.LSList[317], TDataType.dtDateTime, 150, true);
		}

		public TCommunicationListColumns() : base()
		{
			InitData(typeof(TCommunicationColumnType));
		}
	}

	public sealed class TCommunicationListMan : TListManager
	{
		private TGEDCOMCommunicationRecord FRec;

		public override bool CheckFilter(TGenEngine.TShieldState aShieldState)
		{
			bool res = (this.QuickFilter == "*" || IsMatchesMask(this.FRec.CommName, this.QuickFilter));

			res = res && base.CheckNewFilter();

			return res;
		}

		public override void Fetch(TGEDCOMRecord aRec)
		{
			this.FRec = (aRec as TGEDCOMCommunicationRecord);
		}

		protected override object GetColumnValueEx(int col_type, int col_subtype)
		{
			switch (col_type) {
				case 0:
					return this.FRec.CommName;
				case 1:
					return TGenEngine.GetCorresponderStr(this.FTree, this.FRec, false);
				case 2:
					return LangMan.LSList[(int)TGenEngine.CommunicationNames[(int)this.FRec.CommunicationType] - 1];
				case 3:
					return TGenEngine.GEDCOMDateToStr(this.FRec.Date, GKUI.TfmGEDKeeper.Instance.Options.DefDateFormat);
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
