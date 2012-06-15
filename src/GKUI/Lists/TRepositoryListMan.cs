using System;

using GedCom551;
using GKCore;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKUI.Lists
{
	public enum TRepositoryColumnType : byte
	{
		rctName,		
		rctChangeDate
	}

	public sealed class TRepositoryListMan : TListManager
	{
		private TGEDCOMRepositoryRecord FRec;

		public override bool CheckFilter(TPersonsFilter aFilter, TGenEngine.TShieldState aShieldState)
		{
			bool res = (this.QuickFilter == "*" || IsMatchesMask(this.FRec.RepositoryName, this.QuickFilter));

			res = res && base.CheckNewFilter();

			return res;
		}

		public override void Fetch(TGEDCOMRecord aRec)
		{
			this.FRec = (aRec as TGEDCOMRepositoryRecord);
		}

		public override object GetColumnValueEx(int col_index)
		{
			switch (col_index) {
				case 1:
					return this.FRec.RepositoryName;
				case 2:
					return this.FRec.ChangeDate.ChangeDateTime;
				default:
					return null;
			}
		}

		protected override void InitColumnStatics()
		{
			this.ColumnStatics.Clear();
			this.ColumnStatics.Add(new TColumnStatic(LangMan.LSList[134], TDataType.dtString, 400));
			this.ColumnStatics.Add(new TColumnStatic(LangMan.LSList[317], TDataType.dtDateTime, 150));
		}

		public override Type GetColumnsEnum()
		{
			return typeof(TRepositoryColumnType);
		}

		public override TListColumns GetDefaultListColumns()
		{
			return null;
		}

		public TRepositoryListMan(TGEDCOMTree aTree) : base(aTree)
		{
		}
	}
}
