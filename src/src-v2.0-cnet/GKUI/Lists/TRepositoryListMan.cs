using GedCom551;
using GKCore;
using GKSys;
using GKUI.Controls;
using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GKUI.Lists
{
	public class TRepositoryListMan : TListManager
	{
		internal TGEDCOMRepositoryRecord FRec;

		public override bool CheckFilter(TPersonsFilter aFilter, TGenEngine.TShieldState aShieldState)
		{
			bool Result = false;
			if (aFilter.List != TPersonsFilter.TListFilterMode.flSelector || BDSSystem.WStrCmp(aFilter.Name, "*") == 0 || TGenEngine.IsMatchesMask(this.FRec.RepositoryName, aFilter.Name))
			{
				Result = true;
			}
			return Result;
		}

		public override void Fetch(TGEDCOMRecord aRec)
		{
			this.FRec = (aRec as TGEDCOMRepositoryRecord);
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
				Result = this.FRec.RepositoryName;
			}
			return Result;
		}

		public override void GetRow(TGEDCOMRecord aRec, bool isMain, ref string aRow)
		{
			base.GetRow(aRec, isMain, ref aRow);
			aRow = aRow + "\0" + this.FRec.RepositoryName;
			if (isMain)
			{
				aRow = aRow + "\0" + this.FRec.ChangeDate.ToString();
			}
		}

		public override void UpdateItem(TExtListItem aItem, bool isMain)
		{
			aItem.SubItems.Add(this.FRec.RepositoryName);
			if (isMain)
			{
				aItem.SubItems.Add(this.FRec.ChangeDate.ToString());
			}
		}

		public override void UpdateColumns(TGKListView aList, bool isMain)
		{
			aList.AddListColumn("№", 50, false);
			aList.AddListColumn(GKL.LSList[134], 400, false);
			if (isMain)
			{
				aList.AddListColumn(GKL.LSList[317], 150, false);
			}
		}

		public TRepositoryListMan(TGEDCOMTree aTree) : base(aTree)
		{
		}
	}
}
