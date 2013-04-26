using System;
using System.IO;
using System.Runtime.InteropServices;

using Ext.Utils;

namespace GedCom551
{
	public sealed class TGEDCOMAddress : TGEDCOMTag
	{
		private StringList FAddress;
		private TGEDCOMList FPhoneList;
		private TGEDCOMList FEmailList;
		private TGEDCOMList FFaxList;
		private TGEDCOMList FWWWList;

		public StringList Address
		{
			get { return this.GetAddress(); }
			set { this.SetAddress(value); }
		}

		private StringList GetAddress()
		{
			return base.GetTagStrings(this, ref this.FAddress);
		}

		public void SetAddress([In] StringList Value)
		{
			base.SetTagStrings(this, Value);
		}

		public void SetAddressArray([In] string[] Value)
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

		/*public string PhoneNumbers
		{
			get
			{
				return this.GetPhoneNumber(Index);
			}
			set
			{
				this.SetPhoneNumbers(Index, Value);
			}
		}

		public int PhoneNumbersCount
		{
			get
			{
				return this.GetPhoneNumbersCount();
			}
		}*/

		/*public string EmailAddresses
		{
			get
			{
				return this.GetEmailAddresses(Index);
			}
			set
			{
				this.SetEmailAddresses(Index, Value);
			}
		}

		public int EmailAddressesCount
		{
			get
			{
				return this.GetEmailAddressesCount();
			}
		}*/

		/*public string FaxNumbers
		{
			get { return this.GetFaxNumbers(Index); }
			set { this.SetFaxNumbers(Index, Value); }
		}
		public int FaxNumbersCount
		{
			get { return this.GetFaxNumbersCount(); }
		}*/

		/*public string WebPages
		{
			get { return this.GetWebPages(Index); }
			set { this.SetWebPages(Index, Value); }
		}
		public int WebPagesCount
		{
			get { return this.GetWebPagesCount(); }
		}*/

		public string GetEmailAddress(int Index)
		{
			string Result;
			if (this.FEmailList == null || Index < 0 || Index >= this.FEmailList.Count)
			{
				Result = "";
			}
			else
			{
				Result = (this.FEmailList[Index] as TGEDCOMTag).StringValue;
			}
			return Result;
		}

		public int GetEmailAddressesCount()
		{
			return ((this.FEmailList == null) ? 0 : this.FEmailList.Count);
		}

		public string GetFaxNumber(int Index)
		{
			string Result;
			if (this.FFaxList == null || Index < 0 || Index >= this.FFaxList.Count)
			{
				Result = "";
			}
			else
			{
				Result = (this.FFaxList[Index] as TGEDCOMTag).StringValue;
			}
			return Result;
		}

		public int GetFaxNumbersCount()
		{
			return ((this.FFaxList == null) ? 0 : this.FFaxList.Count);
		}

		public string GetPhoneNumber(int Index)
		{
			string Result;
			if (this.FPhoneList == null || Index < 0 || Index >= this.FPhoneList.Count)
			{
				Result = "";
			}
			else
			{
				Result = (this.FPhoneList[Index] as TGEDCOMTag).StringValue;
			}
			return Result;
		}

		public int GetPhoneNumbersCount()
		{
			return ((this.FPhoneList == null) ? 0 : this.FPhoneList.Count);
		}

		public string GetWebPage(int Index)
		{
			string Result;
			if (this.FWWWList == null || Index < 0 || Index >= this.FWWWList.Count)
			{
				Result = "";
			}
			else
			{
				Result = (this.FWWWList[Index] as TGEDCOMTag).StringValue;
			}
			return Result;
		}

		public int GetWebPagesCount()
		{
			return ((this.FWWWList == null) ? 0 : this.FWWWList.Count);
		}

		public void SetEmailAddress(int Index, [In] string Value)
		{
			if (Index >= 3)
			{
				throw new EGEDCOMException(string.Format("The maximum number of email addresses is {0}", TGEDCOMObject.GEDCOMMaxEmailAddresses));
			}
			if (Index >= 0)
			{
				if (this.FEmailList == null)
				{
					this.FEmailList = new TGEDCOMList(this);
				}
				while (Index >= this.FEmailList.Count)
				{
					this.FEmailList.Add(new TGEDCOMTag(base.Owner, this, "EMAIL", ""));
				}
				TGEDCOMTag tag = this.FEmailList[Index] as TGEDCOMTag;
				tag.StringValue = Value;
				tag.SetLevel(base.Level);
			}
		}

		public void SetFaxNumber(int Index, [In] string Value)
		{
			if (Index >= 3)
			{
				throw new EGEDCOMException(string.Format("The maximum number of fax numbers is {0}", TGEDCOMObject.GEDCOMMaxFaxNumbers));
			}
			if (Index >= 0)
			{
				if (this.FFaxList == null)
				{
					this.FFaxList = new TGEDCOMList(this);
				}
				while (Index >= this.FFaxList.Count)
				{
					this.FFaxList.Add(new TGEDCOMTag(base.Owner, this, "FAX", ""));
				}
				TGEDCOMTag tag = this.FFaxList[Index] as TGEDCOMTag;
				tag.StringValue = Value;
				tag.SetLevel(base.Level);
			}
		}

		public void SetPhoneNumber(int Index, [In] string Value)
		{
			if (Index >= 3)
			{
				throw new EGEDCOMException(string.Format("The maximum number of phone numbers is {0}", TGEDCOMObject.GEDCOMMaxPhoneNumbers));
			}
			if (Index >= 0)
			{
				if (this.FPhoneList == null)
				{
					this.FPhoneList = new TGEDCOMList(this);
				}
				while (Index >= this.FPhoneList.Count)
				{
					this.FPhoneList.Add(new TGEDCOMTag(base.Owner, this, "PHON", ""));
				}
				TGEDCOMTag tag = this.FPhoneList[Index] as TGEDCOMTag;
				tag.StringValue = Value;
				tag.SetLevel(base.Level);
			}
		}

		public void SetWebPage(int Index, [In] string Value)
		{
			if (Index >= 3)
			{
				throw new EGEDCOMException(string.Format("The maximum number of web page addresses is {0}", TGEDCOMObject.GEDCOMMaxWebPages));
			}
			if (Index >= 0)
			{
				if (this.FWWWList == null)
				{
					this.FWWWList = new TGEDCOMList(this);
				}
				while (Index >= this.FWWWList.Count)
				{
					this.FWWWList.Add(new TGEDCOMTag(base.Owner, this, "WWW", ""));
				}
				TGEDCOMTag tag = this.FWWWList[Index] as TGEDCOMTag;
				tag.StringValue = Value;
				tag.SetLevel(base.Level);
			}
		}

		protected override void CreateObj(TGEDCOMTree AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			this.FName = "ADDR";
			this.FAddress = null;

			this.FPhoneList = null;
			this.FEmailList = null;
			this.FFaxList = null;
			this.FWWWList = null;
		}

		protected override void SaveTagsToStream(StreamWriter AStream, [In] params string[] ATagSorting)
		{
			base.SaveTagsToStream(AStream, ATagSorting);
			if (this.FPhoneList != null) this.FPhoneList.SaveToStream(AStream);
			if (this.FEmailList != null) this.FEmailList.SaveToStream(AStream);
			if (this.FFaxList != null) this.FFaxList.SaveToStream(AStream);
			if (this.FWWWList != null) this.FWWWList.SaveToStream(AStream);
		}

		public override void Dispose()
		{
			if (!this.Disposed_)
			{
				if (this.FAddress != null) this.FAddress.Free();
				if (this.FPhoneList != null) this.FPhoneList.Dispose();
				if (this.FEmailList != null) this.FEmailList.Dispose();
				if (this.FFaxList != null) this.FFaxList.Dispose();
				if (this.FWWWList != null) this.FWWWList.Dispose();

				base.Dispose();
				this.Disposed_ = true;
			}
		}

		public override TGEDCOMTag AddTag([In] string ATag, [In] string AValue, TagConstructor ATagConstructor)
		{
			TGEDCOMTag Result;
			if (ATag == "PHON")
			{
				if (this.FPhoneList == null)
				{
					this.FPhoneList = new TGEDCOMList(this);
				}
				Result = (this.FPhoneList.Add(new TGEDCOMTag(base.Owner, this, ATag, AValue)) as TGEDCOMTag);
				Result.SetLevel(base.Level);
			}
			else
			{
				if (ATag == "EMAIL")
				{
					if (this.FEmailList == null)
					{
						this.FEmailList = new TGEDCOMList(this);
					}
					Result = (this.FEmailList.Add(new TGEDCOMTag(base.Owner, this, ATag, AValue)) as TGEDCOMTag);
					Result.SetLevel(base.Level);
				}
				else
				{
					if (ATag == "FAX")
					{
						if (this.FFaxList == null)
						{
							this.FFaxList = new TGEDCOMList(this);
						}
						Result = (this.FFaxList.Add(new TGEDCOMTag(base.Owner, this, ATag, AValue)) as TGEDCOMTag);
						Result.SetLevel(base.Level);
					}
					else
					{
						if (ATag == "WWW")
						{
							if (this.FWWWList == null)
							{
								this.FWWWList = new TGEDCOMList(this);
							}
							Result = (this.FWWWList.Add(new TGEDCOMTag(base.Owner, this, ATag, AValue)) as TGEDCOMTag);
							Result.SetLevel(base.Level);
						}
						else
						{
							Result = base.AddTag(ATag, AValue, ATagConstructor);
						}
					}
				}
			}
			return Result;
		}

		public override void Clear()
		{
			base.Clear();
			if (this.FPhoneList != null) this.FPhoneList.Clear();
			if (this.FEmailList != null) this.FEmailList.Clear();
			if (this.FFaxList != null) this.FFaxList.Clear();
			if (this.FWWWList != null) this.FWWWList.Clear();
		}

		public override bool IsEmpty()
		{
			return base.IsEmpty() && this.GetPhoneNumbersCount() == 0 && this.GetEmailAddressesCount() == 0 && this.GetFaxNumbersCount() == 0 && this.GetWebPagesCount() == 0;
		}

		public override void ResetOwner(TGEDCOMTree AOwner)
		{
			base.ResetOwner(AOwner);
			if (this.FPhoneList != null) this.FPhoneList.ResetOwner(AOwner);
			if (this.FEmailList != null) this.FEmailList.ResetOwner(AOwner);
			if (this.FFaxList != null) this.FFaxList.ResetOwner(AOwner);
			if (this.FWWWList != null) this.FWWWList.ResetOwner(AOwner);
		}

		public void DeletePhoneNumber(int Index)
		{
			if (this.FPhoneList != null && Index >= 0 && Index < this.FPhoneList.Count)
			{
				this.FPhoneList.Delete(Index);
			}
		}

		public void DeleteEmail(int Index)
		{
			if (this.FEmailList != null && Index >= 0 && Index < this.FEmailList.Count)
			{
				this.FEmailList.Delete(Index);
			}
		}

		public void DeleteWebPage(int Index)
		{
			if (this.FWWWList != null && Index >= 0 && Index < this.FWWWList.Count)
			{
				this.FWWWList.Delete(Index);
			}
		}

		public TGEDCOMAddress(TGEDCOMTree AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}

        public new static TGEDCOMTag Create(TGEDCOMTree AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue)
		{
			return new TGEDCOMAddress(AOwner, AParent, AName, AValue);
		}
	}
}
