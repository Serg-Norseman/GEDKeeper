using System;
using System.IO;

using ExtUtils;

namespace GedCom551
{
	public sealed class TGEDCOMAddress : TGEDCOMTag
	{
		private GEDCOMList<TGEDCOMTag> fPhoneList;
		private GEDCOMList<TGEDCOMTag> fEmailList;
		private GEDCOMList<TGEDCOMTag> fFaxList;
		private GEDCOMList<TGEDCOMTag> fWWWList;

		public StringList Address
		{
			get { return base.GetTagStrings(this); }
			set { base.SetTagStrings(this, value); }
		}

		public void SetAddressArray(string[] value)
		{
			base.SetTagStrings(this, value);
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

		public GEDCOMList<TGEDCOMTag> PhoneNumbers
		{
			get { return this.fPhoneList; }
		}

		public GEDCOMList<TGEDCOMTag> EmailAddresses
		{
			get { return this.fEmailList; }
		}

		public GEDCOMList<TGEDCOMTag> FaxNumbers
		{
			get { return this.fFaxList; }
		}

		public GEDCOMList<TGEDCOMTag> WebPages
		{
			get { return this.fWWWList; }
		}

		public void AddEmailAddress(string value)
		{
			TGEDCOMTag tag = this.fEmailList.Add(new TGEDCOMTag(base.Owner, this, "EMAIL", value));
			tag.SetLevel(base.Level);
		}

		public void AddFaxNumber(string value)
		{
			TGEDCOMTag tag = this.fFaxList.Add(new TGEDCOMTag(base.Owner, this, "FAX", value));
			tag.SetLevel(base.Level);
		}

		public void AddPhoneNumber(string value)
		{
			TGEDCOMTag tag = this.fPhoneList.Add(new TGEDCOMTag(base.Owner, this, "PHON", value));
			tag.SetLevel(base.Level);
		}

		public void AddWebPage(string value)
		{
			TGEDCOMTag tag = this.fWWWList.Add(new TGEDCOMTag(base.Owner, this, "WWW", value));
			tag.SetLevel(base.Level);
		}

		protected override void CreateObj(TGEDCOMTree owner, GEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.fName = "ADDR";

			this.fPhoneList = new GEDCOMList<TGEDCOMTag>(this);
			this.fEmailList = new GEDCOMList<TGEDCOMTag>(this);
			this.fFaxList = new GEDCOMList<TGEDCOMTag>(this);
			this.fWWWList = new GEDCOMList<TGEDCOMTag>(this);
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

        public override void Assign(TGEDCOMTag source)
		{
			base.Assign(source);

			if (source is TGEDCOMAddress) {
				TGEDCOMAddress srcaddr = source as TGEDCOMAddress;

				base.AssignList(srcaddr.fPhoneList, this.fPhoneList);
				base.AssignList(srcaddr.fEmailList, this.fEmailList);
				base.AssignList(srcaddr.fFaxList, this.fFaxList);
				base.AssignList(srcaddr.fWWWList, this.fWWWList);
			}
		}

		public override TGEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
		{
			TGEDCOMTag result;

			if (tagName == "PHON")
			{
				result = (this.fPhoneList.Add(new TGEDCOMTag(base.Owner, this, tagName, tagValue)));
				result.SetLevel(base.Level);
			}
			else if (tagName == "EMAIL")
			{
				result = (this.fEmailList.Add(new TGEDCOMTag(base.Owner, this, tagName, tagValue)));
				result.SetLevel(base.Level);
			}
			else if (tagName == "FAX")
			{
				result = (this.fFaxList.Add(new TGEDCOMTag(base.Owner, this, tagName, tagValue)));
				result.SetLevel(base.Level);
			}
			else if (tagName == "WWW")
			{
				result = (this.fWWWList.Add(new TGEDCOMTag(base.Owner, this, tagName, tagValue)));
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

		public override void ResetOwner(TGEDCOMTree newOwner)
		{
			base.ResetOwner(newOwner);
			this.fPhoneList.ResetOwner(newOwner);
			this.fEmailList.ResetOwner(newOwner);
			this.fFaxList.ResetOwner(newOwner);
			this.fWWWList.ResetOwner(newOwner);
		}

		public TGEDCOMAddress(TGEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static TGEDCOMTag Create(TGEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
		{
			return new TGEDCOMAddress(owner, parent, tagName, tagValue);
		}
        
        #region Auxiliary

		public void aux_SetAddressValue(string value)
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

        #endregion
	}
}
