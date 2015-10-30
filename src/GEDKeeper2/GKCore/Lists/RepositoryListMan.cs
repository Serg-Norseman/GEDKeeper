using System;
using GKCommon.GEDCOM;
using GKCore.Interfaces;
using GKCore.Types;

namespace GKCore.Lists
{
	/// <summary>
	/// 
	/// </summary>
	public enum RepositoryColumnType
	{
		rctName,		
		rctChangeDate
	}

	/// <summary>
	/// 
	/// </summary>
	public sealed class RepositoryListColumns : ListColumns
	{
		protected override void InitColumnStatics()
		{
			this.AddStatic(LSID.LSID_Repository, TDataType.dtString, 400, true);
			this.AddStatic(LSID.LSID_Changed, TDataType.dtDateTime, 150, true);
		}

		public RepositoryListColumns() : base()
		{
			InitData(typeof(RepositoryColumnType));
		}
	}

	/// <summary>
	/// 
	/// </summary>
	public sealed class RepositoryListMan : ListManager
	{
		private GEDCOMRepositoryRecord fRec;

		public override bool CheckFilter(ShieldState shieldState)
		{
			bool res = (this.QuickFilter == "*" || IsMatchesMask(this.fRec.RepositoryName, this.QuickFilter));

			res = res && base.CheckCommonFilter();

			return res;
		}

		public override void Fetch(GEDCOMRecord aRec)
		{
			this.fRec = (aRec as GEDCOMRepositoryRecord);
		}

		protected override object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
		{
			switch (colType) {
				case 0:
					return this.fRec.RepositoryName;
				case 1:
					return this.fRec.ChangeDate.ChangeDateTime;
				default:
					return null;
			}
		}

		public override Type GetColumnsEnum()
		{
			return typeof(RepositoryColumnType);
		}

		protected override ListColumns GetDefaultListColumns()
		{
			return new RepositoryListColumns();
		}

		public RepositoryListMan(GEDCOMTree tree) : base(tree)
		{
		}
	}
}
