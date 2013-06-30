using System;

using GedCom551;
using GKCore;

/// <summary>
/// Localization: clean
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
			this.AddStatic(LangMan.LSList[153], TDataType.dtString, 300, true);
			this.AddStatic(LangMan.LSList[217], TDataType.dtString, 100, true);
			this.AddStatic(LangMan.LSList[317], TDataType.dtDateTime, 150, true);
		}

		public TFamilyListColumns() : base()
		{
			InitData(typeof(TFamilyColumnType));
		}
	}

	public sealed class TFamilyListMan : TListManager
	{
		private TGEDCOMFamilyRecord FRec;

		public override bool CheckFilter(TGenEngine.TShieldState aShieldState)
		{
			bool res = ((this.FRec.Restriction != TGEDCOMRestriction.rnPrivacy || aShieldState == TGenEngine.TShieldState.ssNone) && (this.QuickFilter == "*" || IsMatchesMask(TGenEngine.aux_GetFamilyStr(this.FRec), this.QuickFilter)));

			res = res && base.CheckNewFilter();

			return res;
		}

		public override void Fetch(TGEDCOMRecord aRec)
		{
			this.FRec = (aRec as TGEDCOMFamilyRecord);
		}

		protected override object GetColumnValueEx(int col_type, int col_subtype)
		{
			switch (col_type) {
				case 0:
					return TGenEngine.aux_GetFamilyStr(this.FRec);
				case 1:
					return TGenEngine.GetMarriageDate(this.FRec, GKUI.TfmGEDKeeper.Instance.Options.DefDateFormat);
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
