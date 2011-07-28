using GedCom551;
using GKCore;
using GKSys;
using GKUI.Controls;
using System;
using System.Runtime.CompilerServices;

namespace GKUI.Lists
{
	public class TTaskListMan : TListManager
	{
		internal TGEDCOMTaskRecord FRec;

		public override bool CheckFilter(TPersonsFilter aFilter, TGenEngine.TShieldState aShieldState)
		{
			bool Result = false;
			if (aFilter.List != TPersonsFilter.TListFilterMode.flSelector || BDSSystem.WStrCmp(aFilter.Name, "*") == 0 || TGenEngine.IsMatchesMask(TGenEngine.GetTaskGoalStr(this.FTree, this.FRec), aFilter.Name))
			{
				Result = true;
			}
			return Result;
		}

		public override void Fetch(TGEDCOMRecord aRec)
		{
			this.FRec = (aRec as TGEDCOMTaskRecord);
		}

		public override string GetColumnValue(int aColIndex, bool isMain)
		{
			string Result;
			switch (aColIndex)
			{
				case 1:
				{
					Result = TGenEngine.GetTaskGoalStr(this.FTree, this.FRec);
					return Result;
				}
				case 2:
				{
					Result = GKL.LSList[(int)TGenEngine.PriorityNames[(int)this.FRec.Priority] - 1];
					return Result;
				}
				case 3:
				{
					Result = TGenEngine.GEDCOMDateToStr(this.FRec.StartDate, GKL.fmGEDKeeper.Options.DefDateFormat);
					return Result;
				}
				case 4:
				{
					Result = TGenEngine.GEDCOMDateToStr(this.FRec.StopDate, GKL.fmGEDKeeper.Options.DefDateFormat);
					return Result;
				}
				case 5:
				{
					Result = this.FRec.ChangeDate.ToString();
					return Result;
				}
			}
			Result = "";
			return Result;
		}

		public override void GetRow(TGEDCOMRecord aRec, bool isMain, ref string aRow)
		{
			base.GetRow(aRec, isMain, ref aRow);
			aRow = aRow + "\0" + TGenEngine.GetTaskGoalStr(this.FTree, this.FRec);
			aRow = aRow + "\0" + GKL.LSList[(int)TGenEngine.PriorityNames[(int)this.FRec.Priority] - 1];
			aRow = aRow + "\0" + TGenEngine.GEDCOMDateToStr(this.FRec.StartDate, GKL.fmGEDKeeper.Options.DefDateFormat);
			aRow = aRow + "\0" + TGenEngine.GEDCOMDateToStr(this.FRec.StopDate, GKL.fmGEDKeeper.Options.DefDateFormat);
			if (isMain)
			{
				aRow = aRow + "\0" + this.FRec.ChangeDate.ToString();
			}
		}

		public override void UpdateItem(TExtListItem aItem, bool isMain)
		{
			aItem.SubItems.Add(TGenEngine.GetTaskGoalStr(this.FTree, this.FRec));
			aItem.SubItems.Add(GKL.LSList[(int)TGenEngine.PriorityNames[(int)this.FRec.Priority] - 1]);
			aItem.SubItems.Add(TGenEngine.GEDCOMDateToStr(this.FRec.StartDate, GKL.fmGEDKeeper.Options.DefDateFormat));
			aItem.SubItems.Add(TGenEngine.GEDCOMDateToStr(this.FRec.StopDate, GKL.fmGEDKeeper.Options.DefDateFormat));
			if (isMain)
			{
				aItem.SubItems.Add(this.FRec.ChangeDate.ToString());
			}
		}

		public override void UpdateColumns(TGKListView aList, bool isMain)
		{
			aList.AddListColumn("№", 50, false);
			aList.AddListColumn(GKL.LSList[182], 300, false);
			aList.AddListColumn(GKL.LSList[178], 90, false);
			aList.AddListColumn(GKL.LSList[180], 90, false);
			aList.AddListColumn(GKL.LSList[181], 90, false);
			if (isMain)
			{
				aList.AddListColumn(GKL.LSList[317], 150, false);
			}
		}

		public TTaskListMan(TGEDCOMTree aTree) : base(aTree)
		{
		}
	}
}
