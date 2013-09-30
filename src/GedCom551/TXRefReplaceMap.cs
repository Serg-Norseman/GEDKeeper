using System;
using System.Collections.Generic;

namespace GedCom551
{
	public sealed class TXRefReplaceMap : TGEDCOMObject
	{
		public struct TXRefRec
		{
			public TGEDCOMRecord Rec;
			public string OldXRef;
			public string NewXRef;

			public TXRefRec(TGEDCOMRecord aRec, string aOldXRef, string aNewXRef) {
				this.Rec = aRec;
				this.OldXRef = aOldXRef;
				this.NewXRef = aNewXRef;
			}
		}

		private List<TXRefRec> FList = new List<TXRefRec>();

		public int Count
		{
			get	{ return this.FList.Count; }
		}

		public TXRefRec this[int Index]
		{
			get { return this.FList[Index]; }
		}

		public void AddXRef(TGEDCOMRecord rec, string oldXRef, string newXRef)
		{
			this.FList.Add(new TXRefRec(rec, oldXRef, newXRef));
		}

		public string FindNewXRef(string oldXRef)
		{
			string result = oldXRef;

			int num = this.FList.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				if (GEDCOMUtils.CleanXRef(this.FList[i].OldXRef) == GEDCOMUtils.CleanXRef(oldXRef))
				{
					result = this.FList[i].NewXRef;
					break;
				}
			}

			return result;
		}
	}
}
