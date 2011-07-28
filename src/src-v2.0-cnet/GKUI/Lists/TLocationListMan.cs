using GedCom551;
using GKCore;
using GKSys;
using GKUI.Controls;
using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GKUI.Lists
{
	public class TLocationListMan : TListManager
	{
		internal TGEDCOMLocationRecord FRec;

		public override bool CheckFilter(TPersonsFilter aFilter, TGenEngine.TShieldState aShieldState)
		{
			bool Result = false;
			if (aFilter.List != TPersonsFilter.TListFilterMode.flSelector || BDSSystem.WStrCmp(aFilter.Name, "*") == 0 || TGenEngine.IsMatchesMask(this.FRec.Name, aFilter.Name))
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
				Result = this.FRec.Name;
			}
			return Result;
		}

		public override void GetRow(TGEDCOMRecord aRec, bool isMain, ref string aRow)
		{
			base.GetRow(aRec, isMain, ref aRow);
			aRow = aRow + "\0" + this.FRec.Name;
			aRow = aRow + "\0" + this.FRec.Map.Lati;
			aRow = aRow + "\0" + this.FRec.Map.Long;
			if (isMain)
			{
				aRow = aRow + "\0" + this.FRec.ChangeDate.ToString();
			}
		}

		public override void UpdateItem(TExtListItem aItem, bool isMain)
		{
			aItem.SubItems.Add(this.FRec.Name);
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
