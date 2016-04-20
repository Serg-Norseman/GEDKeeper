using System.IO;

namespace GKCommon.GEDCOM
{
	public sealed class GEDCOMAddress : GEDCOMTag
	{
		private GEDCOMList<GEDCOMTag> fPhoneList;
		private GEDCOMList<GEDCOMTag> fEmailList;
		private GEDCOMList<GEDCOMTag> fFaxList;
		private GEDCOMList<GEDCOMTag> fWWWList;

		public StringList Address
		{
			get { return base.GetTagStrings(this); }
			set { base.SetTagStrings(this, value); }
		}


		public string AddressLine1
		{
			get { return base.GetTagStringValue("ADR1"); }
			set { base.SetTagStringValue("ADR1", value); }
		}

		public string AddressLine2
		{
			get { return base.GetTagStringValue("ADR2"); }
			set { base.SetTagStringValue("ADR2", value); }
		}

		public string AddressLine3
		{
			get { return base.GetTagStringValue("ADR3"); }
			set { base.SetTagStringValue("ADR3", value); }
		}

		public string AddressCity
		{
			get { return base.GetTagStringValue("CITY"); }
			set { base.SetTagStringValue("CITY", value); }
		}

		public string AddressState
		{
			get { return base.GetTagStringValue("STAE"); }
			set { base.SetTagStringValue("STAE", value); }
		}

		public string AddressPostalCode
		{
			get { return base.GetTagStringValue("POST"); }
			set { base.SetTagStringValue("POST", value); }
		}

		public string AddressCountry
		{
			get { return base.GetTagStringValue("CTRY"); }
			set { base.SetTagStringValue("CTRY", value); }
		}

		public GEDCOMList<GEDCOMTag> PhoneNumbers
		{
			get { return this.fPhoneList; }
		}

		public GEDCOMList<GEDCOMTag> EmailAddresses
		{
			get { return this.fEmailList; }
		}

		public GEDCOMList<GEDCOMTag> FaxNumbers
		{
			get { return this.fFaxList; }
		}

		public GEDCOMList<GEDCOMTag> WebPages
		{
			get { return this.fWWWList; }
		}

		public void AddEmailAddress(string value)
		{
			GEDCOMTag tag = this.fEmailList.Add(new GEDCOMTag(base.Owner, this, "EMAIL", value));
			tag.SetLevel(base.Level);
		}

		public void AddFaxNumber(string value)
		{
			GEDCOMTag tag = this.fFaxList.Add(new GEDCOMTag(base.Owner, this, "FAX", value));
			tag.SetLevel(base.Level);
		}

		public void AddPhoneNumber(string value)
		{
			GEDCOMTag tag = this.fPhoneList.Add(new GEDCOMTag(base.Owner, this, "PHON", value));
			tag.SetLevel(base.Level);
		}

		public void AddWebPage(string value)
		{
			GEDCOMTag tag = this.fWWWList.Add(new GEDCOMTag(base.Owner, this, "WWW", value));
			tag.SetLevel(base.Level);
		}

		protected override void CreateObj(GEDCOMTree owner, GEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.SetName("ADDR");

			this.fPhoneList = new GEDCOMList<GEDCOMTag>(this);
			this.fEmailList = new GEDCOMList<GEDCOMTag>(this);
			this.fFaxList = new GEDCOMList<GEDCOMTag>(this);
			this.fWWWList = new GEDCOMList<GEDCOMTag>(this);
		}

		protected override void SaveTagsToStream(StreamWriter stream)
		{
			base.SaveTagsToStream(stream);
			this.fPhoneList.SaveToStream(stream);
			this.fEmailList.SaveToStream(stream);
			this.fFaxList.SaveToStream(stream);
			this.fWWWList.SaveToStream(stream);
		}

        protected override void Dispose(bool disposing)
		{
			if (disposing)
			{
				this.fPhoneList.Dispose();
				this.fEmailList.Dispose();
				this.fFaxList.Dispose();
				this.fWWWList.Dispose();
			}
            base.Dispose(disposing);
		}

        public override void Assign(GEDCOMTag source)
		{
			base.Assign(source);

			if (source is GEDCOMAddress)
			{
				GEDCOMAddress srcaddr = source as GEDCOMAddress;

				base.AssignList(srcaddr.fPhoneList, this.fPhoneList);
				base.AssignList(srcaddr.fEmailList, this.fEmailList);
				base.AssignList(srcaddr.fFaxList, this.fFaxList);
				base.AssignList(srcaddr.fWWWList, this.fWWWList);
			}
		}

		public override GEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
		{
			GEDCOMTag result;

			if (tagName == "PHON")
			{
				result = (this.fPhoneList.Add(new GEDCOMTag(base.Owner, this, tagName, tagValue)));
				result.SetLevel(base.Level);
			}
			else if (tagName == "EMAIL")
			{
				result = (this.fEmailList.Add(new GEDCOMTag(base.Owner, this, tagName, tagValue)));
				result.SetLevel(base.Level);
			}
			else if (tagName == "FAX")
			{
				result = (this.fFaxList.Add(new GEDCOMTag(base.Owner, this, tagName, tagValue)));
				result.SetLevel(base.Level);
			}
			else if (tagName == "WWW")
			{
				result = (this.fWWWList.Add(new GEDCOMTag(base.Owner, this, tagName, tagValue)));
				result.SetLevel(base.Level);
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
			this.fPhoneList.Clear();
			this.fEmailList.Clear();
			this.fFaxList.Clear();
			this.fWWWList.Clear();
		}

		public override bool IsEmpty()
		{
			return base.IsEmpty() && this.fPhoneList.Count == 0 && this.fEmailList.Count == 0 && this.fFaxList.Count == 0 && this.fWWWList.Count == 0;
		}

		public override void ResetOwner(GEDCOMTree newOwner)
		{
			base.ResetOwner(newOwner);
			this.fPhoneList.ResetOwner(newOwner);
			this.fEmailList.ResetOwner(newOwner);
			this.fFaxList.ResetOwner(newOwner);
			this.fWWWList.ResetOwner(newOwner);
		}

		public GEDCOMAddress(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static GEDCOMTag Create(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
		{
			return new GEDCOMAddress(owner, parent, tagName, tagValue);
		}
        
        #region Auxiliary

		public void SetAddressText(string value)
		{
			StringList sl = new StringList(value);
			try
			{
				this.Address = sl;
			}
			finally
			{
                sl.Dispose();
			}
		}

		public void SetAddressArray(string[] value)
		{
			base.SetTagStrings(this, value);
		}

        #endregion
	}
}
