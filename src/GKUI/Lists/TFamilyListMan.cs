using System;

using GedCom551;
using GKCore;
using GKCore.Sys;
using GKUI.Controls;

namespace GKUI.Lists
{
	public class TFamilyListMan : TListManager
	{
		private TGEDCOMFamilyRecord FRec;

		public override bool CheckFilter(TPersonsFilter aFilter, TGenEngine.TShieldState aShieldState)
		{
			bool Result = false;
			if ((this.FRec.Restriction != TGEDCOMRestriction.rnPrivacy || aShieldState == TGenEngine.TShieldState.ssNone) && (aFilter.List != TPersonsFilter.TListFilterMode.flSelector || aFilter.Name == "*" || TGenEngine.IsMatchesMask(TGenEngine.GetFamilyStr(this.FRec), aFilter.Name)))
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
			string Result;
			if (aColIndex != 1)
			{
				if (aColIndex != 2)
				{
					if (aColIndex != 3)
					{
						Result = "";
					}
					else
					{
						Result = this.FRec.ChangeDate.ToString();
					}
				}
				else
				{
					Result = TGenEngine.GetMarriageDate(this.FRec, GKUI.TfmGEDKeeper.Instance.Options.DefDateFormat);
				}
			}
			else
			{
				Result = TGenEngine.GetFamilyStr(this.FRec);
			}
			return Result;
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
			aList.AddListColumn(GKL.LSList[153], 300, false);
			aList.AddListColumn(GKL.LSList[217], 100, false);
			if (isMain)
			{
				aList.AddListColumn(GKL.LSList[317], 150, false);
			}
		}

		public TFamilyListMan(TGEDCOMTree aTree) : base(aTree)
		{
		}
	}
}
