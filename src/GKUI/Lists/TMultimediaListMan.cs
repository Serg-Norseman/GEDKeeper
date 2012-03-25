using System;

using GedCom551;
using GKCore;
using GKUI.Controls;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKUI.Lists
{
	public sealed class TMultimediaListMan : TListManager
	{
		private TGEDCOMMultimediaRecord FRec;

		public override bool CheckFilter(TPersonsFilter aFilter, TGenEngine.TShieldState aShieldState)
		{
			bool Result = false;
			TGEDCOMFileReferenceWithTitle file_ref = this.FRec.FileReferences[0];
			if (aFilter.List != TPersonsFilter.TListFilterMode.flSelector || aFilter.Name == "*" || IsMatchesMask(file_ref.Title, aFilter.Name))
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
			TGEDCOMFileReferenceWithTitle file_ref = this.FRec.FileReferences[0];
			string result;
			switch (aColIndex) {
				case 1:
					result = file_ref.Title;
					break;
				case 2:
					result = LangMan.LSList[(int)TGenEngine.MediaTypes[(int)file_ref.MediaType] - 1];
					break;
				case 3:
					result = file_ref.StringValue;
					break;
				case 4:
					result = this.FRec.ChangeDate.ToString();
					break;
				default:
					result = "";
					break;
			}
			return result;
		}

		public override void UpdateItem(GKListItem aItem, bool isMain)
		{
			TGEDCOMFileReferenceWithTitle file_ref = this.FRec.FileReferences[0];
			if (file_ref == null)
			{
				aItem.SubItems.Add("error " + this.FRec.XRef);
			}
			else
			{
				aItem.SubItems.Add(file_ref.Title);
				aItem.SubItems.Add(LangMan.LSList[(int)TGenEngine.MediaTypes[(int)file_ref.MediaType] - 1]);
				if (isMain)
				{
					aItem.SubItems.Add(file_ref.StringValue);
					aItem.SubItems.Add(this.FRec.ChangeDate.ToString());
				}
			}
		}
		public override void UpdateColumns(GKListView aList, bool isMain)
		{
			aList.AddListColumn("№", 50, false);
			aList.AddListColumn(LangMan.LSList[125], 150, false);
			aList.AddListColumn(LangMan.LSList[113], 85, false);
			if (isMain)
			{
				aList.AddListColumn(LangMan.LSList[147], 300, false);
				aList.AddListColumn(LangMan.LSList[317], 150, false);
			}
		}

		public TMultimediaListMan(TGEDCOMTree aTree) : base(aTree)
		{
		}
	}
}
