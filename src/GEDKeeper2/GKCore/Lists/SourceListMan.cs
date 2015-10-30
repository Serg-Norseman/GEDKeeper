using System;
using GKCommon.GEDCOM;
using GKCore.Interfaces;
using GKCore.Types;

namespace GKCore.Lists
{
	/// <summary>
	/// 
	/// </summary>
	public enum SourceColumnType
	{
		sctShortName,
		sctAuthor,
		sctTitle,
		sctChangeDate
	}

	/// <summary>
	/// 
	/// </summary>
	public sealed class SourceListColumns : ListColumns
	{
		protected override void InitColumnStatics()
		{
			this.AddStatic(LSID.LSID_ShortTitle, TDataType.dtString, 120, true);
			this.AddStatic(LSID.LSID_Author, TDataType.dtString, 200, true);
			this.AddStatic(LSID.LSID_Title, TDataType.dtString, 200, true);
			this.AddStatic(LSID.LSID_Changed, TDataType.dtDateTime, 150, true);
		}

		public SourceListColumns() : base()
		{
			InitData(typeof(SourceColumnType));
		}
	}

	/// <summary>
	/// 
	/// </summary>
	public sealed class SourceListMan : ListManager
	{
		private GEDCOMSourceRecord fRec;

		public override bool CheckFilter(ShieldState shieldState)
		{
			bool res = (this.QuickFilter == "*" || IsMatchesMask(this.fRec.FiledByEntry, this.QuickFilter));

			res = res && base.CheckCommonFilter();

			return res;
		}

		public override void Fetch(GEDCOMRecord aRec)
		{
			this.fRec = (aRec as GEDCOMSourceRecord);
		}

		protected override object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
		{
			switch (colType) {
				case 0:
					return this.fRec.FiledByEntry.Trim();
				case 1:
					return this.fRec.Originator.Text.Trim();
				case 2:
					return this.fRec.Title.Text.Trim();
				case 3:
					return this.fRec.ChangeDate.ChangeDateTime;
				default:
					return null;
			}
		}

		public override Type GetColumnsEnum()
		{
			return typeof(SourceColumnType);
		}

		protected override ListColumns GetDefaultListColumns()
		{
			return new SourceListColumns();
		}

		public SourceListMan(GEDCOMTree tree) : base(tree)
		{
		}
	}
}
