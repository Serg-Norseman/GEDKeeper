using System;
using GKCommon.GEDCOM;
using GKCore.Interfaces;
using GKCore.Types;

namespace GKCore.Lists
{
	/// <summary>
	/// 
	/// </summary>
	public enum GroupColumnType
	{
		gctName,
		gctChangeDate
	}

	/// <summary>
	/// 
	/// </summary>
	public sealed class GroupListColumns : ListColumns
	{
		protected override void InitColumnStatics()
		{
			this.AddStatic(LSID.LSID_Group, TDataType.dtString, 400, true);
			this.AddStatic(LSID.LSID_Changed, TDataType.dtDateTime, 150, true);
		}

		public GroupListColumns() : base()
		{
			InitData(typeof(GroupColumnType));
		}
	}

	/// <summary>
	/// 
	/// </summary>
	public sealed class GroupListMan : ListManager
	{
		private GEDCOMGroupRecord fRec;

		public override bool CheckFilter(ShieldState shieldState)
		{
			bool res = (this.QuickFilter == "*" || IsMatchesMask(this.fRec.GroupName, this.QuickFilter));

			res = res && base.CheckCommonFilter();

			return res;
		}

		public override void Fetch(GEDCOMRecord aRec)
		{
			this.fRec = (aRec as GEDCOMGroupRecord);
		}

		protected override object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
		{
			switch (colType) {
				case 0:
					return this.fRec.GroupName;
				case 1:
					return this.fRec.ChangeDate.ChangeDateTime;
				default:
					return null;
			}
		}

		public override Type GetColumnsEnum()
		{
			return typeof(GroupColumnType);
		}

		protected override ListColumns GetDefaultListColumns()
		{
			return new GroupListColumns();
		}

		public GroupListMan(GEDCOMTree tree) : base(tree)
		{
		}
	}
}
