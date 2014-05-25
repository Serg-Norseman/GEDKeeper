using System;

namespace GedCom551
{
	public class TGEDCOMPointerWithNotes : TGEDCOMPointer
	{
		private GEDCOMList<TGEDCOMNotes> fNotes;

		public GEDCOMList<TGEDCOMNotes> Notes
		{
			get { return this.fNotes; }
		}

		protected override void CreateObj(TGEDCOMTree owner, GEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.fNotes = new GEDCOMList<TGEDCOMNotes>(this);
		}

		protected override void Dispose(bool disposing)
		{
			if (disposing)
			{
				this.fNotes.Dispose();
			}
			base.Dispose(disposing);
		}

		public override TGEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
		{
			TGEDCOMTag result;

			if (tagName == "NOTE")
			{
				result = this.fNotes.Add(new TGEDCOMNotes(base.Owner, this, tagName, tagValue));
			}
			else
			{
				result = base.AddTag(tagName, tagValue, tagConstructor);
			}

			return result;
		}

		public override void Clear()
		{
			base.Clear();
			this.fNotes.Clear();
		}

		public override bool IsEmpty()
		{
			return base.IsEmpty() && base.Count == 0 && this.fNotes.Count == 0;
		}

		public override void ReplaceXRefs(XRefReplacer map)
		{
            base.ReplaceXRefs(map);
            this.fNotes.ReplaceXRefs(map);
		}

		public override void ResetOwner(TGEDCOMTree newOwner)
		{
			base.ResetOwner(newOwner);
			this.fNotes.ResetOwner(newOwner);
		}

		public TGEDCOMPointerWithNotes(TGEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}
	}
}
