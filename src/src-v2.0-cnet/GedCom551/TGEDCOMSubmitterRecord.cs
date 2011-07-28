using GKSys;
using System;
using System.ComponentModel;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GedCom551
{
	public class TGEDCOMSubmitterRecord : TGEDCOMRecord
	{
		internal TGEDCOMList FLanguages;

		public TGEDCOMAddress Address
		{
			get { return this.GetAddress(); }
		}

		public string AutomatedRecordID
		{
			get { return this.GetStringTag(2); }
			set { this.SetStringTag(2, value); }
		}

		/*public string Languages
		{
			get { return this.GetLanguages(Index); }
			set { this.SetLanguages(Index, value); }
		}

		public int LanguagesCount
		{
			get { return this.GetLanguagesCount(); }
		}*/

		public new TGEDCOMPersonalName Name
		{
			get { return this.GetName(); }
		}

		public string RegisteredReference
		{
			get { return this.GetStringTag(1); }
			set { this.SetStringTag(1, value); }
		}

		internal string GetStringTag(int Index)
		{
			string Result = "";
			if (Index != 1)
			{
				if (Index == 2)
				{
					Result = base.GetTagStringValue("RIN");
				}
			}
			else
			{
				Result = base.GetTagStringValue("RFN");
			}
			return Result;
		}

		internal void SetStringTag(int Index, [In] string Value)
		{
			if (Index != 1)
			{
				if (Index == 2)
				{
					base.SetTagStringValue("RIN", Value);
				}
			}
			else
			{
				base.SetTagStringValue("RFN", Value);
			}
		}

		internal TGEDCOMPersonalName GetName()
		{
			return base.TagClass("NAME", typeof(TGEDCOMPersonalName)) as TGEDCOMPersonalName;
		}

		internal TGEDCOMAddress GetAddress()
		{
			return base.TagClass("ADDR", typeof(TGEDCOMAddress)) as TGEDCOMAddress;
		}

		internal string GetLanguage(int Index)
		{
			string Result;
			if (this.FLanguages == null || Index < 0 || Index >= this.FLanguages.Count)
			{
				Result = "";
			}
			else
			{
				Result = (this.FLanguages[Index] as TGEDCOMTag).StringValue;
			}
			return Result;
		}

		internal int GetLanguagesCount()
		{
			int Result;
			if (this.FLanguages == null)
			{
				Result = 0;
			}
			else
			{
				Result = this.FLanguages.Count;
			}
			return Result;
		}

		internal void SetLanguages(int Index, [In] string Value)
		{
			if (Index >= 3)
			{
				throw new EGEDCOMException(string.Format("The maximum number of languages is {0}", new object[]
				{
					3
				}));
			}
			if (Index >= 0)
			{
				if (this.FLanguages == null)
				{
					this.FLanguages = new TGEDCOMList(this);
				}
				while (Index >= this.FLanguages.Count)
				{
					this.FLanguages.Add(new TGEDCOMTag(base.Owner, this, "LANG", ""));
				}
				(this.FLanguages[Index] as TGEDCOMTag).StringValue = Value;
			}
		}

		protected internal override void CreateObj(TGEDCOMObject AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			base.SetLists(TEnumSet.Create(new Enum[]
			{
				TGEDCOMObject.TGEDCOMSubList.stNotes, 
				TGEDCOMObject.TGEDCOMSubList.stMultimedia
			}));
			this.FRecordType = TGEDCOMRecord.TGEDCOMRecordType.rtSubmitter;
			this.FName = "SUBM";
			this.FLanguages = null;
		}

		public override void Dispose()
		{
			if (!this.Disposed_)
			{
				if (this.FLanguages != null)
				{
					this.FLanguages.Free();
				}
				base.Dispose();
				this.Disposed_ = true;
			}
		}

		public TGEDCOMTag AddLanguage(TGEDCOMTag Value)
		{
			if (this.FLanguages == null)
			{
				this.FLanguages = new TGEDCOMList(this);
			}
			if (this.FLanguages != null)
			{
				this.FLanguages.Add(Value);
			}
			return Value;
		}

		public override TGEDCOMTag AddTag([In] string ATag, [In] string AValue, Type AClass)
		{
			TGEDCOMTag Result;
			if (ATag.Equals("NAME"))
			{
				Result = base.AddTag(ATag, AValue, typeof(TGEDCOMPersonalName));
			}
			else
			{
				if (ATag.Equals("ADDR"))
				{
					Result = base.AddTag(ATag, AValue, typeof(TGEDCOMAddress));
				}
				else
				{
					if (ATag.Equals("PHON") || ATag.Equals("EMAIL") || ATag.Equals("FAX") || ATag.Equals("WWW"))
					{
						TGEDCOMTag AddrTag = base.FindTag("ADDR", 0);
						if (AddrTag == null)
						{
							AddrTag = this.AddTag("ADDR", "", null);
						}
						Result = AddrTag.AddTag(ATag, AValue, AClass);
					}
					else
					{
						if (ATag.Equals("LANG"))
						{
							Result = this.AddLanguage(new TGEDCOMTag(base.Owner, this, ATag, AValue));
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
			if (this.FLanguages != null)
			{
				this.FLanguages.Clear();
			}
		}

		public override bool IsEmpty()
		{
			return base.IsEmpty() && this.GetLanguagesCount() == 0;
		}

		public override void ReplaceXRefs(TXRefReplaceMap aMap)
		{
			base.ReplaceXRefs(aMap);
			if (this.FLanguages != null)
			{
				this.FLanguages.ReplaceXRefs(aMap);
			}
		}

		public override void ResetOwner(TGEDCOMObject AOwner)
		{
			base.ResetOwner(AOwner);
			if (this.FLanguages != null)
			{
				this.FLanguages.ResetOwner(AOwner);
			}
		}

		public TGEDCOMSubmitterRecord(TGEDCOMObject AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
