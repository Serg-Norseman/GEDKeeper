using System;
using System.IO;

namespace GedCom551
{
	public sealed class TGEDCOMMultimediaLink : TGEDCOMPointer
	{
		private GEDCOMList<TGEDCOMFileReference> _FileReferences;

		public GEDCOMList<TGEDCOMFileReference> FileReferences
		{
			get { return this._FileReferences; }
		}

		public bool IsPointer
		{
			get { return (!string.IsNullOrEmpty(base.XRef)); }
		}

		public string Title
		{
			get { return base.GetTagStringValue("TITL"); }
			set { base.SetTagStringValue("TITL", value); }
		}

		public bool IsPrimary
		{
			get {
				TGEDCOMTag tag = base.FindTag("_PRIM", 0);
				return (tag != null) && (tag.StringValue == "Y");
			}
			set {
				if (value) {
					TGEDCOMTag tag = base.FindTag("_PRIM", 0);
					if (tag == null) {
						tag = this.AddTag("_PRIM", "", null);
					}
					tag.StringValue = "Y";
				} else {
					base.DeleteTag("_PRIM");
				}
			}
		}

		protected override void CreateObj(TGEDCOMTree owner, TGEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.FName = "OBJE";
			this._FileReferences = new GEDCOMList<TGEDCOMFileReference>(this);
		}

		protected override string GetStringValue()
		{
			string Result;
			if (this.IsPointer) {
				Result = base.GetStringValue();
			} else {
				Result = this.FStringValue;
			}
			return Result;
		}

		public override void Dispose()
		{
			if (!this.Disposed_)
			{
				this._FileReferences.Dispose();

				base.Dispose();
				this.Disposed_ = true;
			}
		}

		public override TGEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
		{
			TGEDCOMTag result;

			if (tagName == "FILE") {
				result = this._FileReferences.Add(new TGEDCOMFileReference(base.Owner, this, tagName, tagValue));
			} else {
				result = base.AddTag(tagName, tagValue, tagConstructor);
			}

			return result;
		}

		public override void Clear()
		{
			base.Clear();
			this._FileReferences.Clear();
		}

		public override bool IsEmpty()
		{
			bool Result;
			if (this.IsPointer) {
				Result = base.IsEmpty();
			} else {
				Result = (base.Count == 0 && (this._FileReferences.Count == 0));
			}
			return Result;
		}

		public override string ParseString(string AString)
		{
			this.FStringValue = "";
			return base.ParseString(AString);
		}

		public override void ResetOwner(TGEDCOMTree AOwner)
		{
			base.ResetOwner(AOwner);
			this._FileReferences.ResetOwner(AOwner);
		}

		public override void SaveToStream(StreamWriter AStream)
		{
			base.SaveToStream(AStream);
			this._FileReferences.SaveToStream(AStream);
		}

		public TGEDCOMMultimediaLink(TGEDCOMTree owner, TGEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}
	}
}
