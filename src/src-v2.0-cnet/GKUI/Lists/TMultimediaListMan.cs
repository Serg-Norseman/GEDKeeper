using GedCom551;
using GKCore;
using GKSys;
using GKUI.Controls;
using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GKUI.Lists
{
	public class TMultimediaListMan : TListManager
	{
		internal TGEDCOMMultimediaRecord FRec;

		public override bool CheckFilter(TPersonsFilter aFilter, TGenEngine.TShieldState aShieldState)
		{
			bool Result = false;
			TGEDCOMFileReferenceWithTitle file_ref = this.FRec.GetFileReference(0);
			if (aFilter.List != TPersonsFilter.TListFilterMode.flSelector || BDSSystem.WStrCmp(aFilter.Name, "*") == 0 || TGenEngine.IsMatchesMask(file_ref.Title, aFilter.Name))
			{
				Result = true;
			}
			return Result;
		}

		public override void Fetch(TGEDCOMRecord aRec)
		{
			this.FRec = (aRec as TGEDCOMMultimediaRecord);
		}

		public override string GetColumnValue(int aColIndex, bool isMain)
		{
			TGEDCOMFileReferenceWithTitle file_ref = this.FRec.GetFileReference(0);
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
						Result = file_ref.StringValue;
					}
				}
				else
				{
					Result = GKL.LSList[(int)TGenEngine.MediaTypes[(int)file_ref.MediaType] - 1];
				}
			}
			else
			{
				Result = file_ref.Title;
			}
			return Result;
		}
		public override void GetRow(TGEDCOMRecord aRec, bool isMain, ref string aRow)
		{
			base.GetRow(aRec, isMain, ref aRow);
			TGEDCOMFileReferenceWithTitle file_ref = this.FRec.GetFileReference(0);
			if (file_ref == null)
			{
				aRow = aRow + "\0" + "error " + this.FRec.XRef;
			}
			else
			{
				aRow = aRow + "\0" + file_ref.Title;
				aRow = aRow + "\0" + GKL.LSList[(int)TGenEngine.MediaTypes[(int)file_ref.MediaType] - 1];
				if (isMain)
				{
					aRow = aRow + "\0" + file_ref.StringValue;
					aRow = aRow + "\0" + this.FRec.ChangeDate.ToString();
				}
			}
		}
		public override void UpdateItem(TExtListItem aItem, bool isMain)
		{
			TGEDCOMFileReferenceWithTitle file_ref = this.FRec.GetFileReference(0);
			if (file_ref == null)
			{
				aItem.SubItems.Add("error " + this.FRec.XRef);
			}
			else
			{
				aItem.SubItems.Add(file_ref.Title);
				aItem.SubItems.Add(GKL.LSList[(int)TGenEngine.MediaTypes[(int)file_ref.MediaType] - 1]);
				if (isMain)
				{
					aItem.SubItems.Add(file_ref.StringValue);
					aItem.SubItems.Add(this.FRec.ChangeDate.ToString());
				}
			}
		}
		public override void UpdateColumns(TGKListView aList, bool isMain)
		{
			aList.AddListColumn("№", 50, false);
			aList.AddListColumn(GKL.LSList[125], 150, false);
			aList.AddListColumn(GKL.LSList[113], 85, false);
			if (isMain)
			{
				aList.AddListColumn(GKL.LSList[147], 300, false);
				aList.AddListColumn(GKL.LSList[317], 150, false);
			}
		}

		public TMultimediaListMan(TGEDCOMTree aTree) : base(aTree)
		{
		}
	}
}
