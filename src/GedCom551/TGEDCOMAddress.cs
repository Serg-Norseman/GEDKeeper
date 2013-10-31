using System;
using System.IO;

using Ext.Utils;

namespace GedCom551
{
	public sealed class TGEDCOMAddress : TGEDCOMTag
	{
		private StringList FAddress;
		private GEDCOMList<TGEDCOMTag> FPhoneList;
		private GEDCOMList<TGEDCOMTag> FEmailList;
		private GEDCOMList<TGEDCOMTag> FFaxList;
		private GEDCOMList<TGEDCOMTag> FWWWList;

		public StringList Address
		{
			get { return base.GetTagStrings(this, ref this.FAddress); }
			set { base.SetTagStrings(this, value); }
		}

		public void SetAddressArray(string[] Value)
		{
			base.SetTagStrings(this, Value);
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
			get { return this.FPhoneList; }
		}

		public GEDCOMList<TGEDCOMTag> EmailAddresses
		{
			get { return this.FEmailList; }
		}

		public GEDCOMList<TGEDCOMTag> FaxNumbers
		{
			get { return this.FFaxList; }
		}

		public GEDCOMList<TGEDCOMTag> WebPages
		{
			get { return this.FWWWList; }
		}

		public void AddEmailAddress(string value)
		{
			TGEDCOMTag tag = this.FEmailList.Add(new TGEDCOMTag(base.Owner, this, "EMAIL", value));
			tag.SetLevel(base.Level);
		}

		public void AddFaxNumber(string value)
		{
			TGEDCOMTag tag = this.FFaxList.Add(new TGEDCOMTag(base.Owner, this, "FAX", value));
			tag.SetLevel(base.Level);
		}

		public void AddPhoneNumber(string value)
		{
			TGEDCOMTag tag = this.FPhoneList.Add(new TGEDCOMTag(base.Owner, this, "PHON", value));
			tag.SetLevel(base.Level);
		}

		public void AddWebPage(string value)
		{
			TGEDCOMTag tag = this.FWWWList.Add(new TGEDCOMTag(base.Owner, this, "WWW", value));
			tag.SetLevel(base.Level);
		}

		protected override void CreateObj(TGEDCOMTree owner, TGEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.FName = "ADDR";
			this.FAddress = null;

			this.FPhoneList = new GEDCOMList<TGEDCOMTag>(this);
			this.FEmailList = new GEDCOMList<TGEDCOMTag>(this);
			this.FFaxList = new GEDCOMList<TGEDCOMTag>(this);
			this.FWWWList = new GEDCOMList<TGEDCOMTag>(this);
		}

		protected override void SaveTagsToStream(StreamWriter stream)
		{
			base.SaveTagsToStream(stream);
			this.FPhoneList.SaveToStream(stream);
			this.FEmailList.SaveToStream(stream);
			this.FFaxList.SaveToStream(stream);
			this.FWWWList.SaveToStream(stream);
		}

		public override void Dispose()
		{
			if (!this.Disposed_)
			{
				if (this.FAddress != null) this.FAddress.Free();

				this.FPhoneList.Dispose();
				this.FEmailList.Dispose();
				this.FFaxList.Dispose();
				this.FWWWList.Dispose();

				base.Dispose();
				this.Disposed_ = true;
			}
		}

        public override void Assign(TGEDCOMTag Source)
		{
			base.Assign(Source);

			if (Source is TGEDCOMAddress) {
				TGEDCOMAddress srcaddr = Source as TGEDCOMAddress;

				base.AssignList(srcaddr.FPhoneList, this.FPhoneList);
				base.AssignList(srcaddr.FEmailList, this.FEmailList);
				base.AssignList(srcaddr.FFaxList, this.FFaxList);
				base.AssignList(srcaddr.FWWWList, this.FWWWList);
			}
		}

		public override TGEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
		{
			TGEDCOMTag result;

			if (tagName == "PHON")
			{
				result = (this.FPhoneList.Add(new TGEDCOMTag(base.Owner, this, tagName, tagValue)));
				result.SetLevel(base.Level);
			}
			else if (tagName == "EMAIL")
			{
				result = (this.FEmailList.Add(new TGEDCOMTag(base.Owner, this, tagName, tagValue)));
				result.SetLevel(base.Level);
			}
			else if (tagName == "FAX")
			{
				result = (this.FFaxList.Add(new TGEDCOMTag(base.Owner, this, tagName, tagValue)));
				result.SetLevel(base.Level);
			}
			else if (tagName == "WWW")
			{
				result = (this.FWWWList.Add(new TGEDCOMTag(base.Owner, this, tagName, tagValue)));
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
			this.FPhoneList.Clear();
			this.FEmailList.Clear();
			this.FFaxList.Clear();
			this.FWWWList.Clear();
		}

		public override bool IsEmpty()
		{
			return base.IsEmpty() && this.FPhoneList.Count == 0 && this.FEmailList.Count == 0 && this.FFaxList.Count == 0 && this.FWWWList.Count == 0;
		}

		public override void ResetOwner(TGEDCOMTree AOwner)
		{
			base.ResetOwner(AOwner);
			this.FPhoneList.ResetOwner(AOwner);
			this.FEmailList.ResetOwner(AOwner);
			this.FFaxList.ResetOwner(AOwner);
			this.FWWWList.ResetOwner(AOwner);
		}

		public TGEDCOMAddress(TGEDCOMTree owner, TGEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static TGEDCOMTag Create(TGEDCOMTree owner, TGEDCOMObject parent, string tagName, string tagValue)
		{
			return new TGEDCOMAddress(owner, parent, tagName, tagValue);
		}
        
        #region Auxiliary

		public void aux_SetAddressValue(string aValue)
		{
			StringList sl = new StringList(aValue);
			try
			{
				this.Address = sl;
			}
			finally
			{
				sl.Free();
			}
		}

        #endregion
	}
}
