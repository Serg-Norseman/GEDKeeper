using System;

using GedCom551;
using GKCore;
using GKUI.Controls;

namespace GKUI.Lists
{
	public sealed class TCommunicationListMan : TListManager
	{
		private TGEDCOMCommunicationRecord FRec;

		public override bool CheckFilter(TPersonsFilter aFilter, TGenEngine.TShieldState aShieldState)
		{
			bool Result = false;
			if (aFilter.List != TPersonsFilter.TListFilterMode.flSelector || aFilter.Name == "*" || TGenEngine.IsMatchesMask(this.FRec.CommName, aFilter.Name))
			{
				Result = true;
			}
			return Result;
		}

		public override void Fetch(TGEDCOMRecord aRec)
		{
			this.FRec = (aRec as TGEDCOMCommunicationRecord);
		}

		public override string GetColumnValue(int aColIndex, bool isMain)
		{
			string result;
			switch (aColIndex) {
				case 1:
					result = this.FRec.CommName;
					return result;
				case 2:
					result = TGenEngine.GetCorresponderStr(this.FTree, this.FRec, false);
					return result;
				case 3:
					result = GKL.LSList[(int)TGenEngine.CommunicationNames[(int)this.FRec.CommunicationType] - 1];
					return result;
				case 4:
					result = TGenEngine.GEDCOMDateToStr(this.FRec.Date, GKUI.TfmGEDKeeper.Instance.Options.DefDateFormat);
					return result;
				case 5:
					result = this.FRec.ChangeDate.ToString();
					return result;
			}
			result = "";
			return result;
		}

		public override void UpdateItem(TExtListItem aItem, bool isMain)
		{
			aItem.SubItems.Add(this.FRec.CommName);
			aItem.SubItems.Add(TGenEngine.GetCorresponderStr(this.FTree, this.FRec, false));
			aItem.SubItems.Add(GKL.LSList[(int)TGenEngine.CommunicationNames[(int)this.FRec.CommunicationType] - 1]);
			aItem.SubItems.Add(TGenEngine.GEDCOMDateToStr(this.FRec.Date, GKUI.TfmGEDKeeper.Instance.Options.DefDateFormat));
			if (isMain)
			{
				aItem.SubItems.Add(this.FRec.ChangeDate.ToString());
			}
		}
		public override void UpdateColumns(TGKListView aList, bool isMain)
		{
			aList.AddListColumn("№", 50, false);
			aList.AddListColumn(GKL.LSList[183], 300, false);
			aList.AddListColumn(GKL.LSList[184], 200, false);
			aList.AddListColumn(GKL.LSList[113], 90, false);
			aList.AddListColumn(GKL.LSList[139], 90, false);
			if (isMain)
			{
				aList.AddListColumn(GKL.LSList[317], 150, false);
			}
		}

		public TCommunicationListMan(TGEDCOMTree aTree) : base(aTree)
		{
		}
	}
}
