using System;

using GedCom551;
using GKCore;
using GKCore.Interfaces;

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

	public sealed class TGroupListColumns : TListColumns
	{
		protected override void InitColumnStatics()
		{
			this.AddStatic(LangMan.LS(LSID.LSID_Group), TDataType.dtString, 400, true);
			this.AddStatic(LangMan.LS(LSID.LSID_Changed), TDataType.dtDateTime, 150, true);
		}

		public TGroupListColumns() : base()
		{
			InitData(typeof(TGroupColumnType));
		}
	}

	public sealed class TGroupListMan : TListManager
	{
		private TGEDCOMGroupRecord FRec;

		public override bool CheckFilter(ShieldState aShieldState)
		{
			bool res = (this.QuickFilter == "*" || IsMatchesMask(this.FRec.GroupName, this.QuickFilter));

			res = res && base.CheckNewFilter();

			return res;
		}

		public override void Fetch(TGEDCOMRecord aRec)
		{
			this.FRec = (aRec as TGEDCOMGroupRecord);
		}

		protected override object GetColumnValueEx(int colType, int colSubtype)
		{
			switch (colType) {
				case 0:
					return this.FRec.GroupName;
				case 1:
					return this.FRec.ChangeDate.ChangeDateTime;
				default:
					return null;
			}
		}

		public override Type GetColumnsEnum()
		{
			return typeof(TGroupColumnType);
		}

		public override TListColumns GetDefaultListColumns()
		{
			return new TGroupListColumns();
		}

		public TGroupListMan(TGEDCOMTree aTree) : base(aTree)
		{
		}
	}
}
