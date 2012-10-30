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

		public override object GetColumnValueEx(int col_index)
		{
			switch (col_index) {
				case 1:
					return this.FRec.CommName;
				case 2:
					return TGenEngine.GetCorresponderStr(this.FTree, this.FRec, false);
				case 3:
					return LangMan.LSList[(int)TGenEngine.CommunicationNames[(int)this.FRec.CommunicationType] - 1];
				case 4:
					return TGenEngine.GEDCOMDateToStr(this.FRec.Date, GKUI.TfmGEDKeeper.Instance.Options.DefDateFormat);
				case 5:
					return this.FRec.ChangeDate.ChangeDateTime;
				default:
					return null;
			}
		}

		protected override void InitColumnStatics()
		{
			this.ColumnStatics.Clear();
			this.ColumnStatics.Add(new TColumnStatic(LangMan.LSList[183], TDataType.dtString, 300));
			this.ColumnStatics.Add(new TColumnStatic(LangMan.LSList[184], TDataType.dtString, 200));
			this.ColumnStatics.Add(new TColumnStatic(LangMan.LSList[113], TDataType.dtString, 90));
			this.ColumnStatics.Add(new TColumnStatic(LangMan.LSList[139], TDataType.dtString, 90));
			this.ColumnStatics.Add(new TColumnStatic(LangMan.LSList[317], TDataType.dtDateTime, 150));
		}

		public override Type GetColumnsEnum()
		{
			return typeof(TCommunicationColumnType);
		}

		public override TListColumns GetDefaultListColumns()
		{
			return null;
		}

		public TCommunicationListMan(TGEDCOMTree aTree) : base(aTree)
		{
		}
	}
}
