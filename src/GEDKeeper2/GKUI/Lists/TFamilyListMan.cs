using System;

using GedCom551;
using GKCore;
using GKCore.Interfaces;

/// <summary>
/// 
/// </summary>

namespace GKUI.Lists
{
	public enum TFamilyColumnType : byte
	{
		fctFamilyStr,
		fctMarriageDate,		
		fctChangeDate
	}

	public sealed class TFamilyListColumns : TListColumns
	{
		protected override void InitColumnStatics()
		{
			this.AddStatic(LangMan.LS(LSID.LSID_Spouses), TDataType.dtString, 300, true);
			this.AddStatic(LangMan.LS(LSID.LSID_MarriageDate), TDataType.dtString, 100, true);
			this.AddStatic(LangMan.LS(LSID.LSID_Changed), TDataType.dtDateTime, 150, true);
		}

		public TFamilyListColumns() : base()
		{
			InitData(typeof(TFamilyColumnType));
		}
	}

	public sealed class TFamilyListMan : TListManager
	{
		private TGEDCOMFamilyRecord FRec;

		public override bool CheckFilter(ShieldState aShieldState)
		{
			bool res = ((this.FRec.Restriction != TGEDCOMRestriction.rnPrivacy || aShieldState == ShieldState.ssNone) && (this.QuickFilter == "*" || IsMatchesMask(GKUtils.aux_GetFamilyStr(this.FRec), this.QuickFilter)));

			res = res && base.CheckNewFilter();

			return res;
		}

		public override void Fetch(TGEDCOMRecord aRec)
		{
			this.FRec = (aRec as TGEDCOMFamilyRecord);
		}

		protected override object GetColumnValueEx(int colType, int colSubtype)
		{
			switch (colType) {
				case 0:
					return GKUtils.aux_GetFamilyStr(this.FRec);
				case 1:
					return GKUtils.GetMarriageDate(this.FRec, TfmGEDKeeper.Instance.Options.DefDateFormat);
				case 2:
					return this.FRec.ChangeDate.ChangeDateTime;
				default:
					return null;
			}
		}

		public override Type GetColumnsEnum()
		{
			return typeof(TFamilyColumnType);
		}

		public override TListColumns GetDefaultListColumns()
		{
			return new TFamilyListColumns();
		}

		public TFamilyListMan(TGEDCOMTree aTree) : base(aTree)
		{
		}
	}
}
