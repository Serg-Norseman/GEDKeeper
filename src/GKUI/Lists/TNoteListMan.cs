using System;

using GedCom551;
using GKCore;
using GKUI.Controls;

namespace GKUI.Lists
{
	public sealed class TNoteListMan : TListManager
	{
		private TGEDCOMNoteRecord FRec;

		public override bool CheckFilter(TPersonsFilter aFilter, TGenEngine.TShieldState aShieldState)
		{
			return true;
		}

		public override void Fetch(TGEDCOMRecord aRec)
		{
			this.FRec = (aRec as TGEDCOMNoteRecord);
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
				string st;
				if (this.FRec.Note.Count > 0)
				{
					st = this.FRec.Note[0].Trim();
					if (st == "" && this.FRec.Note.Count > 1)
					{
						st = this.FRec.Note[1].Trim();
					}
				}
				else
				{
					st = "";
				}
				Result = st;
			}
			return Result;
		}

		public override void UpdateItem(TExtListItem aItem, bool isMain)
		{
			string st;
			if (this.FRec.Note.Count > 0)
			{
				st = this.FRec.Note[0].Trim();
				if (st == "" && this.FRec.Note.Count > 1)
				{
					st = this.FRec.Note[1].Trim();
				}
			}
			else
			{
				st = "";
			}
			aItem.SubItems.Add(st);
			if (isMain)
			{
				aItem.SubItems.Add(this.FRec.ChangeDate.ToString());
			}
		}

		public override void UpdateColumns(TGKListView aList, bool isMain)
		{
			aList.AddListColumn("№", 50, false);
			aList.AddListColumn(GKL.LSList[108], 400, false);
			if (isMain)
			{
				aList.AddListColumn(GKL.LSList[317], 150, false);
			}
		}

		public TNoteListMan(TGEDCOMTree aTree) : base(aTree)
		{
		}
	}
}
