using System;
using System.Runtime.InteropServices;

namespace GedCom551
{
	public class TGEDCOMPointerWithNotes : TGEDCOMPointer
	{
		private TGEDCOMListEx<TGEDCOMNotes> _Notes;

		public TGEDCOMListEx<TGEDCOMNotes> Notes
		{
			get { return this._Notes; }
		}

		protected override void CreateObj(TGEDCOMTree AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			this._Notes = new TGEDCOMListEx<TGEDCOMNotes>(this);
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

		public override TGEDCOMTag AddTag([In] string ATag, [In] string AValue, TagConstructor ATagConstructor)
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

		public TGEDCOMPointerWithNotes(TGEDCOMTree AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
