using GedCom551;
using GKCore;
using GKSys;
using GKUI.Controls;
using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GKUI.Lists
{
	public class TCommunicationListMan : TListManager
	{
		internal TGEDCOMCommunicationRecord FRec;

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
			this.FRec = (aRec as TGEDCOMCommunicationRecord);
		}

		public override string GetColumnValue(int aColIndex, bool isMain)
		{
			string Result;
			switch (aColIndex)
			{
				case 1:
				{
					Result = this.FRec.Name;
					return Result;
				}
				case 2:
				{
					Result = TGenEngine.GetCorresponderStr(this.FTree, this.FRec, false);
					return Result;
				}
				case 3:
				{
					Result = GKL.LSList[(int)TGenEngine.CommunicationNames[(int)this.FRec.CommunicationType] - 1];
					return Result;
				}
				case 4:
				{
					Result = TGenEngine.GEDCOMDateToStr(this.FRec.Date, GKL.fmGEDKeeper.Options.DefDateFormat);
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
			aRow = aRow + "\0" + this.FRec.Name;
			aRow = aRow + "\0" + TGenEngine.GetCorresponderStr(this.FTree, this.FRec, false);
			aRow = aRow + "\0" + GKL.LSList[(int)TGenEngine.CommunicationNames[(int)this.FRec.CommunicationType] - 1];
			aRow = aRow + "\0" + TGenEngine.GEDCOMDateToStr(this.FRec.Date, GKL.fmGEDKeeper.Options.DefDateFormat);
			if (isMain)
			{
				aRow = aRow + "\0" + this.FRec.ChangeDate.ToString();
			}
		}
		public override void UpdateItem(TExtListItem aItem, bool isMain)
		{
			aItem.SubItems.Add(this.FRec.Name);
			aItem.SubItems.Add(TGenEngine.GetCorresponderStr(this.FTree, this.FRec, false));
			aItem.SubItems.Add(GKL.LSList[(int)TGenEngine.CommunicationNames[(int)this.FRec.CommunicationType] - 1]);
			aItem.SubItems.Add(TGenEngine.GEDCOMDateToStr(this.FRec.Date, GKL.fmGEDKeeper.Options.DefDateFormat));
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
