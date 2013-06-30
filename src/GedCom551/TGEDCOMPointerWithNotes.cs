using System;

namespace GedCom551
{
	public class TGEDCOMPointerWithNotes : TGEDCOMPointer
	{
		private GEDCOMList<TGEDCOMNotes> _Notes;

		public GEDCOMList<TGEDCOMNotes> Notes
		{
			get { return this._Notes; }
		}

		protected override void CreateObj(TGEDCOMTree owner, TGEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this._Notes = new GEDCOMList<TGEDCOMNotes>(this);
		}

		public override void Dispose()
		{
			if (!this.Disposed_)
			{
				this._Notes.Dispose();

				base.Dispose();
				this.Disposed_ = true;
			}
		}

		public override TGEDCOMTag AddTag(string ATag, string AValue, TagConstructor ATagConstructor)
		{
			TGEDCOMTag Result;
			if (ATag == "NOTE")
			{
				Result = this._Notes.Add(new TGEDCOMNotes(base.Owner, this, ATag, AValue));
			}
			else
			{
				Result = base.AddTag(ATag, AValue, ATagConstructor);
			}
			return Result;
		}

		public override void Clear()
		{
			base.Clear();
			this._Notes.Clear();
		}

		public override bool IsEmpty()
		{
			return base.IsEmpty() && base.Count == 0 && this._Notes.Count == 0;
		}

		public override void ReplaceXRefs(TXRefReplaceMap aMap)
		{
			base.ReplaceXRefs(aMap);
			this._Notes.ReplaceXRefs(aMap);
		}

		public override void ResetOwner(TGEDCOMTree AOwner)
		{
			base.ResetOwner(AOwner);
			this._Notes.ResetOwner(AOwner);
		}

		public TGEDCOMPointerWithNotes(TGEDCOMTree owner, TGEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}
	}
}
