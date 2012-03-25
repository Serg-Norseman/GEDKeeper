using System;

using GedCom551;
using GKCore;
using GKUI.Controls;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKUI.Lists
{
	public sealed class TLocationListMan : TListManager
	{
		private TGEDCOMLocationRecord FRec;

		public override bool CheckFilter(TPersonsFilter aFilter, TGenEngine.TShieldState aShieldState)
		{
			bool Result = false;
			if (aFilter.List != TPersonsFilter.TListFilterMode.flSelector || aFilter.Name == "*" || IsMatchesMask(this.FRec.LocationName, aFilter.Name))
			{
				Result = true;
			}
			return Result;
		}

		public override void Fetch(TGEDCOMRecord aRec)
		{
			this.FRec = (aRec as TGEDCOMLocationRecord);
		}

		public override string GetColumnValue(int aColIndex, bool isMain)
		{
			string result;
			switch (aColIndex) {
				case 1:
					result = this.FRec.LocationName;
					break;
				case 2:
					result = this.FRec.Map.Lati;
					break;
				case 3:
					result = this.FRec.Map.Long;
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

		public override void UpdateItem(GKListItem aItem, bool isMain)
		{
			aItem.SubItems.Add(this.FRec.LocationName);
			aItem.SubItems.Add(this.FRec.Map.Lati);
			aItem.SubItems.Add(this.FRec.Map.Long);
			if (isMain)
			{
				aItem.SubItems.Add(this.FRec.ChangeDate.ToString());
			}
		}

		public override void UpdateColumns(GKListView aList, bool isMain)
		{
			aList.AddListColumn("№", 50, false);
			aList.AddListColumn(LangMan.LSList[125], 300, false);
			aList.AddListColumn(LangMan.LSList[171], 120, false);
			aList.AddListColumn(LangMan.LSList[172], 120, false);
			if (isMain)
			{
				aList.AddListColumn(LangMan.LSList[317], 150, false);
			}
		}

		public TLocationListMan(TGEDCOMTree aTree) : base(aTree)
		{
		}
	}
}
