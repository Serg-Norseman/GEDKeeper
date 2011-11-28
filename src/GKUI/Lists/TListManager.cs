using System;

using GedCom551;
using GKCore;
using GKCore.Sys;
using GKUI.Controls;

namespace GKUI.Lists
{
	public abstract class TListManager : IDisposable
	{
		protected TGEDCOMTree FTree;
		protected bool Disposed_;

		public TListManager(TGEDCOMTree aTree)
		{
			this.FTree = aTree;
		}

		public void Dispose()
		{
			if (!this.Disposed_)
			{
				this.Disposed_ = true;
			}
		}

		public void UpdateTitles(TGKListView aList, bool isMain)
		{
			aList.BeginUpdate();
			try
			{
				aList.Columns.Clear();
				this.UpdateColumns(aList, isMain);
			}
			finally
			{
				aList.EndUpdate();
			}
		}

		public abstract bool CheckFilter(TPersonsFilter aFilter, TGenEngine.TShieldState aShieldState);
		public abstract void Fetch(TGEDCOMRecord aRec);

		public virtual string GetColumnValue(int aColIndex, bool isMain)
		{
			return "";
		}

		public virtual void InitFilter(TPersonsFilter aFilter)
		{
		}

		public abstract void UpdateItem(TExtListItem aItem, bool isMain);
		public abstract void UpdateColumns(TGKListView aList, bool isMain);
	}
}
