using System;

using Ext.Utils;
using GedCom551;
using GKCore;
using GKUI.Controls;

/// <summary>
/// Localization: clean
/// </summary>

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

		public bool IsMatchesMask(string S, string Mask)
		{
			bool result = false;
			if (S != null && Mask != null && S != "" && Mask != "")
			{
				string stx = S.ToLower();
				string[] masks = Mask.ToLower().Split(new char[] { '|' });

				int num = masks.Length - 1;
				for (int i = 0; i <= num; i++)
				{
					result = (result || TGenEngine.MatchesMask(stx, masks[i]));
				}
			}
			return result;
		}

		public void UpdateTitles(GKListView aList, bool isMain)
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

		public abstract void UpdateItem(GKListItem aItem, bool isMain);
		public abstract void UpdateColumns(GKListView aList, bool isMain);
	}
}
