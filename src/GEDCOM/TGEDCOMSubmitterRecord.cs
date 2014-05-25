using System;

namespace GedCom551
{
	public sealed class TGEDCOMSubmitterRecord : TGEDCOMRecord
	{
		private GEDCOMList<TGEDCOMTag> fLanguages;

		public TGEDCOMAddress Address
		{
			get { return base.TagClass("ADDR", TGEDCOMAddress.Create) as TGEDCOMAddress; }
		}

		public GEDCOMList<TGEDCOMTag> Languages
		{
			get { return this.fLanguages; }
		}

		public new TGEDCOMPersonalName Name
		{
			get { return base.TagClass("NAME", TGEDCOMPersonalName.Create) as TGEDCOMPersonalName; }
		}

		public string RegisteredReference
		{
			get { return base.GetTagStringValue("RFN"); }
			set { base.SetTagStringValue("RFN", value); }
		}

		public void SetLanguage(int index, string value)
		{
			if (index >= 0)
			{
				while (index >= this.fLanguages.Count)
				{
					this.fLanguages.Add(new TGEDCOMTag(base.Owner, this, "LANG", ""));
				}
				this.fLanguages[index].StringValue = value;
			}
		}

		protected override void CreateObj(TGEDCOMTree owner, GEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.fRecordType = TGEDCOMRecordType.rtSubmitter;
			this.fName = "SUBM";

			this.fLanguages = new GEDCOMList<TGEDCOMTag>(this);
		}

		protected override void Dispose(bool disposing)
		{
			if (disposing)
			{
				this.fLanguages.Dispose();
			}
			base.Dispose(disposing);
		}

		public TGEDCOMTag AddLanguage(TGEDCOMTag value)
		{
			this.fLanguages.Add(value);
			return value;
		}

		public override TGEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
		{
			TGEDCOMTag result;

			if (tagName == "NAME")
			{
				result = base.AddTag(tagName, tagValue, TGEDCOMPersonalName.Create);
			}
			else if (tagName == "PHON" || tagName == "EMAIL" || tagName == "FAX" || tagName == "WWW")
			{
				result = this.Address.AddTag(tagName, tagValue, tagConstructor);
			}
			else if (tagName == "LANG")
			{
				result = this.AddLanguage(new TGEDCOMTag(base.Owner, this, tagName, tagValue));
			}
			else
			{
				// "ADDR" defines by default
				result = base.AddTag(tagName, tagValue, tagConstructor);
			}

			return result;
		}

		public override void Clear()
		{
			base.Clear();
			this.fLanguages.Clear();
		}

		public override bool IsEmpty()
		{
			return base.IsEmpty() && (this.fLanguages.Count == 0);
		}

        public override void ReplaceXRefs(XRefReplacer map)
		{
            base.ReplaceXRefs(map);
            this.fLanguages.ReplaceXRefs(map);
		}

		public override void ResetOwner(TGEDCOMTree newOwner)
		{
			base.ResetOwner(newOwner);
			this.fLanguages.ResetOwner(newOwner);
		}

		public TGEDCOMSubmitterRecord(TGEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static TGEDCOMTag Create(TGEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
		{
			return new TGEDCOMSubmitterRecord(owner, parent, tagName, tagValue);
		}
	}
}
