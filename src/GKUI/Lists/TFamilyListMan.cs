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

	public sealed class TFamilyListMan : TListManager
	{
		private TGEDCOMFamilyRecord FRec;

		public override bool CheckFilter(TPersonsFilter aFilter, TGenEngine.TShieldState aShieldState)
		{
			bool res = ((this.FRec.Restriction != TGEDCOMRestriction.rnPrivacy || aShieldState == TGenEngine.TShieldState.ssNone) && (this.QuickFilter == "*" || IsMatchesMask(TGenEngine.aux_GetFamilyStr(this.FRec), this.QuickFilter)));

			res = res && base.CheckNewFilter();

			return res;
		}

		public override void Fetch(TGEDCOMRecord aRec)
		{
			this.FRec = (aRec as TGEDCOMFamilyRecord);
		}

		public override object GetColumnValueEx(int col_index)
		{
			switch (col_index) {
				case 1:
					return TGenEngine.aux_GetFamilyStr(this.FRec);
				case 2:
					return TGenEngine.GetMarriageDate(this.FRec, GKUI.TfmGEDKeeper.Instance.Options.DefDateFormat);
				case 3:
					return this.FRec.ChangeDate.ChangeDateTime;
				default:
					return null;
			}
		}

		protected override void InitColumnStatics()
		{
			this.ColumnStatics.Clear();
			this.ColumnStatics.Add(new TColumnStatic(LangMan.LSList[153], TDataType.dtString, 300));
			this.ColumnStatics.Add(new TColumnStatic(LangMan.LSList[217], TDataType.dtString, 100));
			this.ColumnStatics.Add(new TColumnStatic(LangMan.LSList[317], TDataType.dtDateTime, 150));
		}

		public override Type GetColumnsEnum()
		{
			return typeof(TFamilyColumnType);
		}

		public override TListColumns GetDefaultListColumns()
		{
			return null;
		}

		public TFamilyListMan(TGEDCOMTree aTree) : base(aTree)
		{
		}
	}
}
