using System;

using GedCom551;
using GKCore;
using GKUI.Controls;

/// <summary>
/// Localization: unknown
/// </summary>

namespace GKUI.Lists
{
	public sealed class TFamilyListMan : TListManager
	{
		private TGEDCOMFamilyRecord FRec;

		public override bool CheckFilter(TPersonsFilter aFilter, TGenEngine.TShieldState aShieldState)
		{
			bool Result = false;
			if ((this.FRec.Restriction != TGEDCOMRestriction.rnPrivacy || aShieldState == TGenEngine.TShieldState.ssNone) && (aFilter.List != TPersonsFilter.TListFilterMode.flSelector || aFilter.Name == "*" || IsMatchesMask(TGenEngine.GetFamilyStr(this.FRec), aFilter.Name)))
			{
				Result = true;
			}
			return Result;
		}

		public override void Fetch(TGEDCOMRecord aRec)
		{
			this.FRec = (aRec as TGEDCOMFamilyRecord);
		}

		public override string GetColumnValue(int aColIndex, bool isMain)
		{
			string result;
			switch (aColIndex) {
				case 1:
					result = TGenEngine.GetFamilyStr(this.FRec);
					break;
				case 2:
					result = TGenEngine.GetMarriageDate(this.FRec, GKUI.TfmGEDKeeper.Instance.Options.DefDateFormat);
					break;
				case 3:
					result = this.FRec.ChangeDate.ToString();
					break;
				default:
					result = "";
					break;
			}
			return result;
		}

		public override void UpdateItem(TExtListItem aItem, bool isMain)
		{
			aItem.SubItems.Add(TGenEngine.GetFamilyStr(this.FRec));
			aItem.SubItems.Add(TGenEngine.GetMarriageDate(this.FRec, GKUI.TfmGEDKeeper.Instance.Options.DefDateFormat));
			if (isMain)
			{
				aItem.SubItems.Add(this.FRec.ChangeDate.ToString());
			}
		}

		public override void UpdateColumns(TGKListView aList, bool isMain)
		{
			aList.AddListColumn("№", 50, false);
			aList.AddListColumn(LangMan.LSList[153], 300, false);
			aList.AddListColumn(LangMan.LSList[217], 100, false);
			if (isMain)
			{
				aList.AddListColumn(LangMan.LSList[317], 150, false);
			}
		}

		public TFamilyListMan(TGEDCOMTree aTree) : base(aTree)
		{
		}
	}
}
