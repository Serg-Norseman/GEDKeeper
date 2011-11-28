using System;

using GedCom551;
using GKCore;
using GKUI.Controls;

namespace GKUI.Lists
{
	public sealed class TResearchListMan : TListManager
	{
		private TGEDCOMResearchRecord FRec;

		public override bool CheckFilter(TPersonsFilter aFilter, TGenEngine.TShieldState aShieldState)
		{
			bool Result = false;
			if (aFilter.List != TPersonsFilter.TListFilterMode.flSelector || aFilter.Name == "*" || TGenEngine.IsMatchesMask(this.FRec.ResearchName, aFilter.Name))
			{
				Result = true;
			}
			return Result;
		}

		public override void Fetch(TGEDCOMRecord aRec)
		{
			this.FRec = (aRec as TGEDCOMResearchRecord);
		}

		public override string GetColumnValue(int aColIndex, bool isMain)
		{
			string Result;
			switch (aColIndex)
			{
				case 1:
				{
					Result = this.FRec.ResearchName;
					return Result;
				}
				case 2:
				{
					Result = GKL.LSList[(int)TGenEngine.PriorityNames[(int)this.FRec.Priority] - 1];
					return Result;
				}
				case 3:
				{
					Result = GKL.LSList[(int)TGenEngine.StatusNames[(int)this.FRec.Status] - 1];
					return Result;
				}
				case 4:
				{
					Result = TGenEngine.GEDCOMDateToStr(this.FRec.StartDate, GKUI.TfmGEDKeeper.Instance.Options.DefDateFormat);
					return Result;
				}
				case 5:
				{
					Result = TGenEngine.GEDCOMDateToStr(this.FRec.StopDate, GKUI.TfmGEDKeeper.Instance.Options.DefDateFormat);
					return Result;
				}
				case 6:
				{
					Result = this.FRec.Percent.ToString();
					return Result;
				}
				case 7:
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
			aItem.SubItems.Add(this.FRec.ResearchName);
			aItem.SubItems.Add(GKL.LSList[(int)TGenEngine.PriorityNames[(int)this.FRec.Priority] - 1]);
			aItem.SubItems.Add(GKL.LSList[(int)TGenEngine.StatusNames[(int)this.FRec.Status] - 1]);
			aItem.SubItems.Add(TGenEngine.GEDCOMDateToStr(this.FRec.StartDate, GKUI.TfmGEDKeeper.Instance.Options.DefDateFormat));
			aItem.SubItems.Add(TGenEngine.GEDCOMDateToStr(this.FRec.StopDate, GKUI.TfmGEDKeeper.Instance.Options.DefDateFormat));
			aItem.SubItems.Add(this.FRec.Percent.ToString());
			if (isMain)
			{
				aItem.SubItems.Add(this.FRec.ChangeDate.ToString());
			}
		}

		public override void UpdateColumns(TGKListView aList, bool isMain)
		{
			aList.AddListColumn("№", 50, false);
			aList.AddListColumn(GKL.LSList[125], 300, false);
			aList.AddListColumn(GKL.LSList[178], 90, false);
			aList.AddListColumn(GKL.LSList[117], 90, false);
			aList.AddListColumn(GKL.LSList[180], 90, false);
			aList.AddListColumn(GKL.LSList[181], 90, false);
			aList.AddListColumn(GKL.LSList[179], 90, false);
			if (isMain)
			{
				aList.AddListColumn(GKL.LSList[317], 150, false);
			}
		}

		public TResearchListMan(TGEDCOMTree aTree) : base(aTree)
		{
		}
	}
}
