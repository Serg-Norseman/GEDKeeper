using System;

using GedCom551;
using GKCore;
using GKUI.Controls;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKUI.Lists
{
	public sealed class TGroupListMan : TListManager
	{
		private TGEDCOMGroupRecord FRec;

		public override bool CheckFilter(TPersonsFilter aFilter, TGenEngine.TShieldState aShieldState)
		{
			bool Result = false;
			if (aFilter.List != TPersonsFilter.TListFilterMode.flSelector || aFilter.Name == "*" || IsMatchesMask(this.FRec.GroupName, aFilter.Name))
			{
				Result = true;
			}
			return Result;
		}

		public override void Fetch(TGEDCOMRecord aRec)
		{
			this.FRec = (aRec as TGEDCOMGroupRecord);
		}

		public override string GetColumnValue(int aColIndex, bool isMain)
		{
			string Result;
			if (aColIndex != 1)
			{
				if (aColIndex != 2)
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
				Result = this.FRec.GroupName;
			}
			return Result;
		}

		public override void UpdateItem(GKListItem aItem, bool isMain)
		{
			aItem.SubItems.Add(this.FRec.GroupName);
			if (isMain)
			{
				aItem.SubItems.Add(this.FRec.ChangeDate.ToString());
			}
		}

		public override void UpdateColumns(GKListView aList, bool isMain)
		{
			aList.AddListColumn("№", 50, false);
			aList.AddListColumn(LangMan.LSList[185], 400, false);
			if (isMain)
			{
				aList.AddListColumn(LangMan.LSList[317], 150, false);
			}
		}

		public TGroupListMan(TGEDCOMTree aTree) : base(aTree)
		{
		}
	}
}
