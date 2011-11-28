using System;

using GedCom551;
using GKCore;
using GKUI.Controls;

namespace GKUI.Lists
{
	public sealed class TSourceListMan : TListManager
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
			string result;
			switch (aColIndex) {
				case 1:
					result = this.FRec.FiledByEntry.Trim();
					break;
				case 2:
					result = this.FRec.Originator.Text.Trim();
					break;
				case 3:
					result = this.FRec.Title.Text.Trim();
					break;
				case 4:
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
