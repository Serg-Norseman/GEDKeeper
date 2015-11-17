using System;
using GKCommon.GEDCOM;
using GKCommon.GEDCOM.Enums;
using GKCore.Interfaces;
using GKCore.Options;
using GKCore.Types;

namespace GKCore.Lists
{
	/// <summary>
	/// 
	/// </summary>
	public enum FamilyColumnType
	{
		fctFamilyStr,
		fctMarriageDate,		
		fctChangeDate
	}

	/// <summary>
	/// 
	/// </summary>
	public sealed class FamilyListColumns : ListColumns
	{
		protected override void InitColumnStatics()
		{
			this.AddStatic(LSID.LSID_Spouses, DataType.dtString, 300, true);
			this.AddStatic(LSID.LSID_MarriageDate, DataType.dtString, 100, true);
			this.AddStatic(LSID.LSID_Changed, DataType.dtDateTime, 150, true);
		}

		public FamilyListColumns() : base()
		{
			InitData(typeof(FamilyColumnType));
		}
	}

	/// <summary>
	/// 
	/// </summary>
	public sealed class FamilyListMan : ListManager
	{
		private GEDCOMFamilyRecord fRec;

		public override bool CheckFilter(ShieldState shieldState)
		{
			bool res = (GKUtils.IsRecordAccess(this.fRec.Restriction, shieldState)
			            && (this.QuickFilter == "*" || IsMatchesMask(GKUtils.GetFamilyString(this.fRec), this.QuickFilter)));

			res = res && base.CheckCommonFilter();

			return res;
		}

		public override void Fetch(GEDCOMRecord aRec)
		{
			this.fRec = (aRec as GEDCOMFamilyRecord);
		}

		protected override object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
		{
			switch (colType) {
				case 0:
					return GKUtils.GetFamilyString(this.fRec);
				case 1:
					return GetDateValue(GKUtils.GetMarriageDate(this.fRec), isVisible);
				case 2:
					return this.fRec.ChangeDate.ChangeDateTime;
				default:
					return null;
			}
		}

		public override Type GetColumnsEnum()
		{
			return typeof(FamilyColumnType);
		}

		protected override ListColumns GetDefaultListColumns()
		{
			return new FamilyListColumns();
		}

		public FamilyListMan(GEDCOMTree tree) : base(tree)
		{
		}
	}
}
