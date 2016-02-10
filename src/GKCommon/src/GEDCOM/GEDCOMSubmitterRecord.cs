namespace GKCommon.GEDCOM
{
	public sealed class GEDCOMSubmitterRecord : GEDCOMRecord
	{
		private GEDCOMList<GEDCOMTag> fLanguages;

		public GEDCOMAddress Address
		{
			get { return base.TagClass("ADDR", GEDCOMAddress.Create) as GEDCOMAddress; }
		}

		public GEDCOMList<GEDCOMTag> Languages
		{
			get { return this.fLanguages; }
		}

		public new GEDCOMPersonalName Name
		{
			get { return base.TagClass("NAME", GEDCOMPersonalName.Create) as GEDCOMPersonalName; }
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
					this.fLanguages.Add(new GEDCOMTag(base.Owner, this, "LANG", ""));
				}
				this.fLanguages[index].StringValue = value;
			}
		}

		protected override void CreateObj(GEDCOMTree owner, GEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.fRecordType = GEDCOMRecordType.rtSubmitter;
			this.fName = "SUBM";

			this.fLanguages = new GEDCOMList<GEDCOMTag>(this);
		}

		protected override void Dispose(bool disposing)
		{
			if (disposing)
			{
				this.fLanguages.Dispose();
			}
			base.Dispose(disposing);
		}

		public GEDCOMTag AddLanguage(GEDCOMTag value)
		{
			this.fLanguages.Add(value);
			return value;
		}

		public override GEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
		{
			GEDCOMTag result;

			if (tagName == "NAME")
			{
				result = base.AddTag(tagName, tagValue, GEDCOMPersonalName.Create);
			}
			else if (tagName == "PHON" || tagName == "EMAIL" || tagName == "FAX" || tagName == "WWW")
			{
				result = this.Address.AddTag(tagName, tagValue, tagConstructor);
			}
			else if (tagName == "LANG")
			{
				result = this.AddLanguage(new GEDCOMTag(base.Owner, this, tagName, tagValue));
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

		public override void ResetOwner(GEDCOMTree newOwner)
		{
			base.ResetOwner(newOwner);
			this.fLanguages.ResetOwner(newOwner);
		}

		public GEDCOMSubmitterRecord(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static GEDCOMTag Create(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
		{
			return new GEDCOMSubmitterRecord(owner, parent, tagName, tagValue);
		}
	}
}
