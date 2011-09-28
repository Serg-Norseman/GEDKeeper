using GedCom551;
using GKCore;
using GKCore.Sys;
using GKUI.Controls;
using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GKUI.Lists
{
	public class TLocationListMan : TListManager
	{
		private TGEDCOMLocationRecord FRec;

		public override bool CheckFilter(TPersonsFilter aFilter, TGenEngine.TShieldState aShieldState)
		{
			bool Result = false;
			if (aFilter.List != TPersonsFilter.TListFilterMode.flSelector || aFilter.Name == "*" || TGenEngine.IsMatchesMask(this.FRec.LocationName, aFilter.Name))
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
						Result = this.FRec.Map.Long;
					}
				}
				else
				{
					Result = this.FRec.Map.Lati;
				}
			}
			else
			{
				Result = this.FRec.LocationName;
			}
			return Result;
		}

		public override void UpdateItem(TExtListItem aItem, bool isMain)
		{
			aItem.SubItems.Add(this.FRec.LocationName);
			aItem.SubItems.Add(this.FRec.Map.Lati);
			aItem.SubItems.Add(this.FRec.Map.Long);
			if (isMain)
			{
				aItem.SubItems.Add(this.FRec.ChangeDate.ToString());
			}
		}

		public override void UpdateColumns(TGKListView aList, bool isMain)
		{
			aList.AddListColumn("№", 50, false);
			aList.AddListColumn(GKL.LSList[125], 300, false);
			aList.AddListColumn(GKL.LSList[171], 120, false);
			aList.AddListColumn(GKL.LSList[172], 120, false);
			if (isMain)
			{
				aList.AddListColumn(GKL.LSList[317], 150, false);
			}
		}

		public TLocationListMan(TGEDCOMTree aTree) : base(aTree)
		{
		}
	}
}
