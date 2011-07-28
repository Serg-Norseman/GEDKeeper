using GedCom551;
using GKCore;
using GKSys;
using GKUI.Controls;
using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GKUI.Lists
{
	public class TNoteListMan : TListManager
	{
		internal TGEDCOMNoteRecord FRec;

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
				if (this.FRec.Notes.Count > 0)
				{
					st = this.FRec.Notes[0].Trim();
					if (BDSSystem.WStrCmp(st, "") == 0 && this.FRec.Notes.Count > 1)
					{
						st = this.FRec.Notes[1].Trim();
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
		public override void GetRow(TGEDCOMRecord aRec, bool isMain, ref string aRow)
		{
			base.GetRow(aRec, isMain, ref aRow);
			string st;
			if (this.FRec.Notes.Count > 0)
			{
				st = this.FRec.Notes[0].Trim();
				if (BDSSystem.WStrCmp(st, "") == 0 && this.FRec.Notes.Count > 1)
				{
					st = this.FRec.Notes[1].Trim();
				}
			}
			else
			{
				st = "";
			}
			aRow = aRow + "\0" + st;
			if (isMain)
			{
				aRow = aRow + "\0" + this.FRec.ChangeDate.ToString();
			}
		}
		public override void UpdateItem(TExtListItem aItem, bool isMain)
		{
			string st;
			if (this.FRec.Notes.Count > 0)
			{
				st = this.FRec.Notes[0].Trim();
				if (BDSSystem.WStrCmp(st, "") == 0 && this.FRec.Notes.Count > 1)
				{
					st = this.FRec.Notes[1].Trim();
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
