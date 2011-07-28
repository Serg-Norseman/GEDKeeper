using GKSys;
using System;
using System.ComponentModel;
using System.IO;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GedCom551
{
	public class TGEDCOMAddress : TGEDCOMTag
	{
		internal TStrings FAddress;
		internal TGEDCOMList FPhoneList;
		internal TGEDCOMList FEmailList;
		internal TGEDCOMList FFaxList;
		internal TGEDCOMList FWWWList;

		public TStrings Address
		{
			get { return this.GetAddress(); }
			set { this.SetAddress(value); }
		}

		public string AddressLine1
		{
			get { return this.GetStringTag(1); }
			set { this.SetStringTag(1, value); }
		}

		public string AddressLine2
		{
			get
			{
				return this.GetStringTag(2);
			}
			set
			{
				this.SetStringTag(2, value);
			}
		}

		public string AddressLine3
		{
			get
			{
				return this.GetStringTag(3);
			}
			set
			{
				this.SetStringTag(3, value);
			}
		}

		public string AddressCity
		{
			get
			{
				return this.GetStringTag(4);
			}
			set
			{
				this.SetStringTag(4, value);
			}
		}

		public string AddressState
		{
			get
			{
				return this.GetStringTag(5);
			}
			set
			{
				this.SetStringTag(5, value);
			}
		}

		public string AddressPostalCode
		{
			get
			{
				return this.GetStringTag(6);
			}
			set
			{
				this.SetStringTag(6, value);
			}
		}

		public string AddressCountry
		{
			get
			{
				return this.GetStringTag(7);
			}
			set
			{
				this.SetStringTag(7, value);
			}
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

		internal TStrings GetAddress()
		{
			return base.GetTagStrings(this, ref this.FAddress);
		}

		internal string GetEmailAddress(int Index)
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

		internal int GetEmailAddressesCount()
		{
			int Result;
			if (this.FEmailList == null)
			{
				Result = 0;
			}
			else
			{
				Result = this.FEmailList.Count;
			}
			return Result;
		}

		internal string GetFaxNumber(int Index)
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

		internal int GetFaxNumbersCount()
		{
			int Result;
			if (this.FFaxList == null)
			{
				Result = 0;
			}
			else
			{
				Result = this.FFaxList.Count;
			}
			return Result;
		}

		internal string GetPhoneNumber(int Index)
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

		internal int GetPhoneNumbersCount()
		{
			int Result;
			if (this.FPhoneList == null)
			{
				Result = 0;
			}
			else
			{
				Result = this.FPhoneList.Count;
			}
			return Result;
		}
		internal string GetStringTag(int Index)
		{
			string Result = "";
			switch (Index)
			{
				case 1:
				{
					Result = base.GetTagStringValue("ADR1");
					break;
				}
				case 2:
				{
					Result = base.GetTagStringValue("ADR2");
					break;
				}
				case 3:
				{
					Result = base.GetTagStringValue("ADR3");
					break;
				}
				case 4:
				{
					Result = base.GetTagStringValue("CITY");
					break;
				}
				case 5:
				{
					Result = base.GetTagStringValue("STAE");
					break;
				}
				case 6:
				{
					Result = base.GetTagStringValue("POST");
					break;
				}
				case 7:
				{
					Result = base.GetTagStringValue("CTRY");
					break;
				}
			}
			return Result;
		}

		internal string GetWebPage(int Index)
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

		internal int GetWebPagesCount()
		{
			int Result;
			if (this.FWWWList == null)
			{
				Result = 0;
			}
			else
			{
				Result = this.FWWWList.Count;
			}
			return Result;
		}

		internal void SetAddress([In] TStrings Value)
		{
			base.SetTagStrings(this, Value);
		}

		internal void SetEmailAddress(int Index, [In] string Value)
		{
			if (Index >= 3)
			{
				throw new EGEDCOMException(string.Format("The maximum number of email addresses is {0}", new object[]
				{
					3
				}));
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

		internal void SetFaxNumber(int Index, [In] string Value)
		{
			if (Index >= 3)
			{
				throw new EGEDCOMException(string.Format("The maximum number of fax numbers is {0}", new object[]
				{
					3
				}));
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

		internal void SetPhoneNumber(int Index, [In] string Value)
		{
			if (Index >= 3)
			{
				throw new EGEDCOMException(string.Format("The maximum number of phone numbers is {0}", new object[]
				{
					3
				}));
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
		internal void SetStringTag(int Index, [In] string Value)
		{
			switch (Index)
			{
				case 1:
				{
					base.SetTagStringValue("ADR1", Value);
					break;
				}
				case 2:
				{
					base.SetTagStringValue("ADR2", Value);
					break;
				}
				case 3:
				{
					base.SetTagStringValue("ADR3", Value);
					break;
				}
				case 4:
				{
					base.SetTagStringValue("CITY", Value);
					break;
				}
				case 5:
				{
					base.SetTagStringValue("STAE", Value);
					break;
				}
				case 6:
				{
					base.SetTagStringValue("POST", Value);
					break;
				}
				case 7:
				{
					base.SetTagStringValue("CTRY", Value);
					break;
				}
			}
		}

		internal void SetWebPage(int Index, [In] string Value)
		{
			if (Index >= 3)
			{
				throw new EGEDCOMException(string.Format("The maximum number of web page addresses is {0}", new object[]
				{
					3
				}));
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
		protected internal override void CreateObj(TGEDCOMObject AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			this.FName = "ADDR";
			this.FAddress = null;
			this.FPhoneList = null;
			this.FEmailList = null;
			this.FFaxList = null;
			this.FWWWList = null;
		}
		protected internal override void SaveTagsToStream(StreamWriter AStream, [In] params string[] ATagSorting)
		{
			base.SaveTagsToStream(AStream, ATagSorting);
			if (this.FPhoneList != null)
			{
				this.FPhoneList.SaveToStream(AStream);
			}
			if (this.FEmailList != null)
			{
				this.FEmailList.SaveToStream(AStream);
			}
			if (this.FFaxList != null)
			{
				this.FFaxList.SaveToStream(AStream);
			}
			if (this.FWWWList != null)
			{
				this.FWWWList.SaveToStream(AStream);
			}
		}

		public override void Dispose()
		{
			if (!this.Disposed_)
			{
				if (this.FAddress != null)
				{
					this.FAddress.Free();
				}
				if (this.FPhoneList != null)
				{
					this.FPhoneList.Free();
				}
				if (this.FEmailList != null)
				{
					this.FEmailList.Free();
				}
				if (this.FFaxList != null)
				{
					this.FFaxList.Free();
				}
				if (this.FWWWList != null)
				{
					this.FWWWList.Free();
				}
				base.Dispose();
				this.Disposed_ = true;
			}
		}

		public override TGEDCOMTag AddTag([In] string ATag, [In] string AValue, Type AClass)
		{
			TGEDCOMTag Result;
			if (ATag.Equals("PHON"))
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
				if (ATag.Equals("EMAIL"))
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
					if (ATag.Equals("FAX"))
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
						if (ATag.Equals("WWW"))
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
							Result = base.AddTag(ATag, AValue, AClass);
						}
					}
				}
			}
			return Result;
		}
		public override void Clear()
		{
			base.Clear();
			if (this.FPhoneList != null)
			{
				this.FPhoneList.Clear();
			}
			if (this.FEmailList != null)
			{
				this.FEmailList.Clear();
			}
			if (this.FFaxList != null)
			{
				this.FFaxList.Clear();
			}
			if (this.FWWWList != null)
			{
				this.FWWWList.Clear();
			}
		}
		public override bool IsEmpty()
		{
			return base.IsEmpty() && this.GetPhoneNumbersCount() == 0 && this.GetEmailAddressesCount() == 0 && this.GetFaxNumbersCount() == 0 && this.GetWebPagesCount() == 0;
		}
		public override void ResetOwner(TGEDCOMObject AOwner)
		{
			base.ResetOwner(AOwner);
			if (this.FPhoneList != null)
			{
				this.FPhoneList.ResetOwner(AOwner);
			}
			if (this.FEmailList != null)
			{
				this.FEmailList.ResetOwner(AOwner);
			}
			if (this.FFaxList != null)
			{
				this.FFaxList.ResetOwner(AOwner);
			}
			if (this.FWWWList != null)
			{
				this.FWWWList.ResetOwner(AOwner);
			}
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

		public TGEDCOMAddress(TGEDCOMObject AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
