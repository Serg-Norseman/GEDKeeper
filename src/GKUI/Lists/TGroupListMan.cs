using System;

using GedCom551;
using GKCore;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKUI.Lists
{
	public enum TGroupColumnType : byte
	{
		gctName,
		gctChangeDate
	}

	public sealed class TGroupListMan : TListManager
	{
		private TGEDCOMGroupRecord FRec;

		public override bool CheckFilter(TGenEngine.TShieldState aShieldState)
		{
			bool res = (this.QuickFilter == "*" || IsMatchesMask(this.FRec.GroupName, this.QuickFilter));

			res = res && base.CheckNewFilter();

			return res;
		}

		public override void Fetch(TGEDCOMRecord aRec)
		{
			this.FRec = (aRec as TGEDCOMGroupRecord);
		}

		public override object GetColumnValueEx(int col_index)
		{
			switch (col_index) {
				case 1:
					return this.FRec.GroupName;
				case 2:
					return this.FRec.ChangeDate.ChangeDateTime;
				default:
					return null;
			}
		}

		protected override void InitColumnStatics()
		{
			this.ColumnStatics.Clear();
			this.ColumnStatics.Add(new TColumnStatic(LangMan.LSList[185], TDataType.dtString, 400));
			this.ColumnStatics.Add(new TColumnStatic(LangMan.LSList[317], TDataType.dtDateTime, 150));
		}

		public override Type GetColumnsEnum()
		{
			return typeof(TGroupColumnType);
		}

		public override TListColumns GetDefaultListColumns()
		{
			return null;
		}

		public TGroupListMan(TGEDCOMTree aTree) : base(aTree)
		{
		}
	}
}
