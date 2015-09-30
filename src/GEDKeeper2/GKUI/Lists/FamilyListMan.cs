using System;
using GKCommon.GEDCOM;
using GKCommon.GEDCOM.Enums;
using GKCore;
using GKCore.Interfaces;
using GKCore.Types;

/// <summary>
/// 
/// </summary>

namespace GKUI.Lists
{
	public enum FamilyColumnType
	{
		fctFamilyStr,
		fctMarriageDate,		
		fctChangeDate
	}

	public sealed class FamilyListColumns : ListColumns
	{
		protected override void InitColumnStatics()
		{
			this.AddStatic(LSID.LSID_Spouses, TDataType.dtString, 300, true);
			this.AddStatic(LSID.LSID_MarriageDate, TDataType.dtString, 100, true);
			this.AddStatic(LSID.LSID_Changed, TDataType.dtDateTime, 150, true);
		}

		public FamilyListColumns() : base()
		{
			InitData(typeof(FamilyColumnType));
		}
	}

	public sealed class FamilyListMan : ListManager
	{
		private GEDCOMFamilyRecord fRec;

		public override bool CheckFilter(ShieldState aShieldState)
		{
			bool res = ((this.fRec.Restriction != GEDCOMRestriction.rnPrivacy || aShieldState == ShieldState.ssNone) && (this.QuickFilter == "*" || IsMatchesMask(GKUtils.GetFamilyString(this.fRec), this.QuickFilter)));

			res = res && base.CheckNewFilter();

			return res;
		}

		public override void Fetch(GEDCOMRecord aRec)
		{
			this.fRec = (aRec as GEDCOMFamilyRecord);
		}

		protected override object GetColumnValueEx(int colType, int colSubtype)
		{
			switch (colType) {
				case 0:
					return GKUtils.GetFamilyString(this.fRec);
				case 1:
					return GKUtils.GetMarriageDate(this.fRec, TfmGEDKeeper.Instance.Options.DefDateFormat);
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

		public override ListColumns GetDefaultListColumns()
		{
			return new FamilyListColumns();
		}

		public FamilyListMan(GEDCOMTree aTree) : base(aTree)
		{
		}
	}
}
