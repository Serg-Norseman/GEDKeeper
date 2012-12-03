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
		protected override void InitDefaultColumns()
		{
			TColumnProps[] array1 = new TColumnProps[4];
			array1[0] = new TColumnProps(TSourceColumnType.sctShortName, true);
			array1[1] = new TColumnProps(TSourceColumnType.sctAuthor, true);
			array1[2] = new TColumnProps(TSourceColumnType.sctTitle, true);
			array1[3] = new TColumnProps(TSourceColumnType.sctChangeDate, true);
			DefColumns = array1;
		}

		public TSourceListColumns()
		{
			InitData(typeof(TSourceColumnType));
		}
	}

	public sealed class TSourceListMan : TListManager
	{
		private TGEDCOMSourceRecord FRec;

		public override bool CheckFilter(TGenEngine.TShieldState aShieldState)
		{
			bool res = (this.QuickFilter == "*" || IsMatchesMask(this.FRec.FiledByEntry, this.QuickFilter));

			res = res && base.CheckNewFilter();

			return res;
		}

		public override void Fetch(TGEDCOMRecord aRec)
		{
			this.FRec = (aRec as TGEDCOMSourceRecord);
		}

		public override object GetColumnValueDirect(int col_type, int col_subtype)
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

		protected override void InitColumnStatics()
		{
			this.ColumnStatics.Clear();
			this.ColumnStatics.Add(new TColumnStatic(LangMan.LSList[141], TDataType.dtString, 120));
			this.ColumnStatics.Add(new TColumnStatic(LangMan.LSList[142], TDataType.dtString, 200));
			this.ColumnStatics.Add(new TColumnStatic(LangMan.LSList[125], TDataType.dtString, 200));
			this.ColumnStatics.Add(new TColumnStatic(LangMan.LSList[317], TDataType.dtDateTime, 150));
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
