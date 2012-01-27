using System;

using GedCom551;
using GKCore;
using GKUI.Controls;

/// <summary>
/// Localization: unknown
/// </summary>

namespace GKUI.Lists
{
	public sealed class TTaskListMan : TListManager
	{
		private TGEDCOMTaskRecord FRec;

		public override bool CheckFilter(TPersonsFilter aFilter, TGenEngine.TShieldState aShieldState)
		{
			bool Result = false;
			if (aFilter.List != TPersonsFilter.TListFilterMode.flSelector || aFilter.Name == "*" || IsMatchesMask(TGenEngine.GetTaskGoalStr(this.FTree, this.FRec), aFilter.Name))
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
					Result = LangMan.LSList[(int)TGenEngine.PriorityNames[(int)this.FRec.Priority] - 1];
					return Result;
				}
				case 3:
				{
					Result = TGenEngine.GEDCOMDateToStr(this.FRec.StartDate, GKUI.TfmGEDKeeper.Instance.Options.DefDateFormat);
					return Result;
				}
				case 4:
				{
					Result = TGenEngine.GEDCOMDateToStr(this.FRec.StopDate, GKUI.TfmGEDKeeper.Instance.Options.DefDateFormat);
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

		public override void UpdateItem(TExtListItem aItem, bool isMain)
		{
			aItem.SubItems.Add(TGenEngine.GetTaskGoalStr(this.FTree, this.FRec));
			aItem.SubItems.Add(LangMan.LSList[(int)TGenEngine.PriorityNames[(int)this.FRec.Priority] - 1]);
			aItem.SubItems.Add(TGenEngine.GEDCOMDateToStr(this.FRec.StartDate, GKUI.TfmGEDKeeper.Instance.Options.DefDateFormat));
			aItem.SubItems.Add(TGenEngine.GEDCOMDateToStr(this.FRec.StopDate, GKUI.TfmGEDKeeper.Instance.Options.DefDateFormat));
			if (isMain)
			{
				aItem.SubItems.Add(this.FRec.ChangeDate.ToString());
			}
		}

		public override void UpdateColumns(TGKListView aList, bool isMain)
		{
			aList.AddListColumn("№", 50, false);
			aList.AddListColumn(LangMan.LSList[182], 300, false);
			aList.AddListColumn(LangMan.LSList[178], 90, false);
			aList.AddListColumn(LangMan.LSList[180], 90, false);
			aList.AddListColumn(LangMan.LSList[181], 90, false);
			if (isMain)
			{
				aList.AddListColumn(LangMan.LSList[317], 150, false);
			}
		}

		public TTaskListMan(TGEDCOMTree aTree) : base(aTree)
		{
		}
	}
}
