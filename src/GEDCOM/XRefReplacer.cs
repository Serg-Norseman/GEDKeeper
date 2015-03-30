using System;
using System.Collections.Generic;

namespace GedCom551
{
	public sealed class XRefReplacer : GEDCOMObject
	{
		public struct XRefEntry
		{
			public readonly TGEDCOMRecord Rec;
			public readonly string OldXRef;
			public readonly string NewXRef;

			public XRefEntry(TGEDCOMRecord aRec, string aOldXRef, string aNewXRef) {
				this.Rec = aRec;
				this.OldXRef = aOldXRef;
				this.NewXRef = aNewXRef;
			}
		}

		private readonly List<XRefEntry> fList;

		public int Count
		{
			get	{ return this.fList.Count; }
		}

		public XRefEntry this[int index]
		{
			get { return this.fList[index]; }
		}

        public XRefReplacer()
        {
            this.fList = new List<XRefEntry>();
        }

		public void AddXRef(TGEDCOMRecord rec, string oldXRef, string newXRef)
		{
			this.fList.Add(new XRefEntry(rec, oldXRef, newXRef));
		}

		public string FindNewXRef(string oldXRef)
		{
			string result = oldXRef;

			foreach (XRefEntry entry in this.fList)
			{
				if (GEDCOMUtils.CleanXRef(entry.OldXRef) == GEDCOMUtils.CleanXRef(oldXRef))
				{
					result = entry.NewXRef;
					break;
				}
			}

			return result;
		}
	}
}
