using GedCom551;
using GKCore;
using GKSys;
using GKUI.Controls;
using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GKUI.Lists
{
	public class TFamilyListMan : TListManager
	{
		internal TGEDCOMFamilyRecord FRec;

		public override bool CheckFilter(TPersonsFilter aFilter, TGenEngine.TShieldState aShieldState)
		{
			bool Result = false;
			if ((this.FRec.Restriction != TGEDCOMObject.TGEDCOMRestriction.rnPrivacy || aShieldState == TGenEngine.TShieldState.ssNone) && (aFilter.List != TPersonsFilter.TListFilterMode.flSelector || BDSSystem.WStrCmp(aFilter.Name, "*") == 0 || TGenEngine.IsMatchesMask(TGenEngine.GetFamilyStr(this.FRec), aFilter.Name)))
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
					Result = TGenEngine.GetMarriageDate(this.FRec, GKL.fmGEDKeeper.Options.DefDateFormat);
				}
			}
			else
			{
				Result = TGenEngine.GetFamilyStr(this.FRec);
			}
			return Result;
		}
		public override void GetRow(TGEDCOMRecord aRec, bool isMain, ref string aRow)
		{
			base.GetRow(aRec, isMain, ref aRow);
			aRow = aRow + "\0" + TGenEngine.GetFamilyStr(this.FRec);
			aRow = aRow + "\0" + TGenEngine.GetMarriageDate(this.FRec, GKL.fmGEDKeeper.Options.DefDateFormat);
			if (isMain)
			{
				aRow = aRow + "\0" + this.FRec.ChangeDate.ToString();
			}
		}
		public override void UpdateItem(TExtListItem aItem, bool isMain)
		{
			aItem.SubItems.Add(TGenEngine.GetFamilyStr(this.FRec));
			aItem.SubItems.Add(TGenEngine.GetMarriageDate(this.FRec, GKL.fmGEDKeeper.Options.DefDateFormat));
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
