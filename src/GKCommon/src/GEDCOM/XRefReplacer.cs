using System.Collections.Generic;

namespace GKCommon.GEDCOM
{
	public sealed class XRefReplacer : GEDCOMObject
	{
		public sealed class XRefEntry
		{
			public readonly GEDCOMRecord Rec;
			public readonly string OldXRef;
			public readonly string NewXRef;

			public XRefEntry(GEDCOMRecord aRec, string aOldXRef, string aNewXRef) {
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

		public void AddXRef(GEDCOMRecord rec, string oldXRef, string newXRef)
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
