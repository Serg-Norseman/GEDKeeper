using System;

using GedCom551;
using GKCore;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKUI.Lists
{
	public enum TSourceColumnType : byte
	{
		sctShortName,
		sctAuthor,
		sctTitle,
		sctChangeDate
	}

	public sealed class TSourceListColumns : TListColumns
	{
		protected override void InitColumnStatics()
		{
			this.AddStatic(LangMan.LSList[141], TDataType.dtString, 120, true);
			this.AddStatic(LangMan.LSList[142], TDataType.dtString, 200, true);
			this.AddStatic(LangMan.LSList[125], TDataType.dtString, 200, true);
			this.AddStatic(LangMan.LSList[317], TDataType.dtDateTime, 150, true);
		}

		public TSourceListColumns() : base()
		{
			InitData(typeof(TSourceColumnType));
		}
	}

	public sealed class TSourceListMan : TListManager
	{
		private TGEDCOMSourceRecord FRec;

		public override bool CheckFilter(TShieldState aShieldState)
		{
			bool res = (this.QuickFilter == "*" || IsMatchesMask(this.FRec.FiledByEntry, this.QuickFilter));

			res = res && base.CheckNewFilter();

			return res;
		}

		public override void Fetch(TGEDCOMRecord aRec)
		{
			this.FRec = (aRec as TGEDCOMSourceRecord);
		}

		protected override object GetColumnValueEx(int col_type, int col_subtype)
		{
			switch (col_type) {
				case 0:
					return this.FRec.FiledByEntry.Trim();
				case 1:
					return this.FRec.Originator.Text.Trim();
				case 2:
					return this.FRec.Title.Text.Trim();
				case 3:
					return this.FRec.ChangeDate.ChangeDateTime;
				default:
					return null;
			}
		}

		public override Type GetColumnsEnum()
		{
			return typeof(TSourceColumnType);
		}

		public override TListColumns GetDefaultListColumns()
		{
			return new TSourceListColumns();
		}

		public TSourceListMan(TGEDCOMTree aTree) : base(aTree)
		{
		}
	}
}
