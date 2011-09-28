using System;

using GedCom551;
using GKCore;
using GKCore.Sys;
using GKUI.Controls;

namespace GKUI.Lists
{
	public class TSourceListMan : TListManager
	{
		private TGEDCOMSourceRecord FRec;

		public override bool CheckFilter(TPersonsFilter aFilter, TGenEngine.TShieldState aShieldState)
		{
			bool Result = false;
			if (aFilter.List != TPersonsFilter.TListFilterMode.flSelector || aFilter.Name == "*" || TGenEngine.IsMatchesMask(this.FRec.FiledByEntry, aFilter.Name))
			{
				Result = true;
			}
			return Result;
		}

		public override void Fetch(TGEDCOMRecord aRec)
		{
			this.FRec = (aRec as TGEDCOMSourceRecord);
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
						if (aColIndex != 4)
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
						Result = this.FRec.Title.Text.Trim();
					}
				}
				else
				{
					Result = this.FRec.Originator.Text.Trim();
				}
			}
			else
			{
				Result = this.FRec.FiledByEntry.Trim();
			}
			return Result;
		}

		public override void UpdateItem(TExtListItem aItem, bool isMain)
		{
			aItem.SubItems.Add(this.FRec.FiledByEntry.Trim());
			if (isMain)
			{
				aItem.SubItems.Add(this.FRec.Originator.Text.Trim());
				aItem.SubItems.Add(this.FRec.Title.Text.Trim());
				aItem.SubItems.Add(this.FRec.ChangeDate.ToString());
			}
		}

		public override void UpdateColumns(TGKListView aList, bool isMain)
		{
			aList.AddListColumn("№", 50, false);
			aList.AddListColumn(GKL.LSList[141], 120, false);
			if (isMain)
			{
				aList.AddListColumn(GKL.LSList[142], 200, false);
				aList.AddListColumn(GKL.LSList[125], 200, false);
				aList.AddListColumn(GKL.LSList[317], 150, false);
			}
		}

		public TSourceListMan(TGEDCOMTree aTree) : base(aTree)
		{
		}
	}
}
