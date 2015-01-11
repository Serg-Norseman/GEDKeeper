using System;

using GedCom551;
using GKCore;
using GKCore.Interfaces;

/// <summary>
/// 
/// </summary>

namespace GKUI.Lists
{
	public enum TRepositoryColumnType : byte
	{
		rctName,		
		rctChangeDate
	}

	public sealed class TRepositoryListColumns : TListColumns
	{
		protected override void InitColumnStatics()
		{
			this.AddStatic(LangMan.LS(LSID.LSID_Repository), TDataType.dtString, 400, true);
			this.AddStatic(LangMan.LS(LSID.LSID_Changed), TDataType.dtDateTime, 150, true);
		}

		public TRepositoryListColumns() : base()
		{
			InitData(typeof(TRepositoryColumnType));
		}
	}

	public sealed class TRepositoryListMan : TListManager
	{
		private TGEDCOMRepositoryRecord FRec;

		public override bool CheckFilter(ShieldState aShieldState)
		{
			bool res = (this.QuickFilter == "*" || IsMatchesMask(this.FRec.RepositoryName, this.QuickFilter));

			res = res && base.CheckNewFilter();

			return res;
		}

		public override void Fetch(TGEDCOMRecord aRec)
		{
			this.FRec = (aRec as TGEDCOMRepositoryRecord);
		}

		protected override object GetColumnValueEx(int colType, int colSubtype)
		{
			switch (colType) {
				case 0:
					return this.FRec.RepositoryName;
				case 1:
					return this.FRec.ChangeDate.ChangeDateTime;
				default:
					return null;
			}
		}

		public override Type GetColumnsEnum()
		{
			return typeof(TRepositoryColumnType);
		}

		public override TListColumns GetDefaultListColumns()
		{
			return new TRepositoryListColumns();
		}

		public TRepositoryListMan(TGEDCOMTree aTree) : base(aTree)
		{
		}
	}
}
