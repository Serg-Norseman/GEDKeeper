using System;

using GedCom551;
using GKCore;
using GKUI.Controls;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKUI.Lists
{
	public sealed class TRepositoryListMan : TListManager
	{
		private TGEDCOMRepositoryRecord FRec;

		public override bool CheckFilter(TPersonsFilter aFilter, TGenEngine.TShieldState aShieldState)
		{
			bool Result = false;
			if (aFilter.List != TPersonsFilter.TListFilterMode.flSelector || aFilter.Name == "*" || IsMatchesMask(this.FRec.RepositoryName, aFilter.Name))
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

			switch (aColIndex) {
				case 1:
					Result = this.FRec.RepositoryName;
					break;
				case 2:
					Result = this.FRec.ChangeDate.ToString();
					break;
				default:
					Result = "";
					break;
			}

			return Result;
		}

		public override void UpdateItem(GKListItem aItem, bool isMain)
		{
			aItem.SubItems.Add(this.FRec.RepositoryName);
			if (isMain)
			{
				aItem.SubItems.Add(this.FRec.ChangeDate.ToString());
			}
		}

		public override void UpdateColumns(GKListView aList, bool isMain)
		{
			aList.AddListColumn("№", 50, false);
			aList.AddListColumn(LangMan.LSList[134], 400, false);
			if (isMain)
			{
				aList.AddListColumn(LangMan.LSList[317], 150, false);
			}
		}

		public TRepositoryListMan(TGEDCOMTree aTree) : base(aTree)
		{
		}
	}
}
