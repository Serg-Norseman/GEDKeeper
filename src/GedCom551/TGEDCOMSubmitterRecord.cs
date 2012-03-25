using System;
using System.Runtime.InteropServices;

using Ext.Utils;

namespace GedCom551
{
	public sealed class TGEDCOMSubmitterRecord : TGEDCOMRecord
	{
		private TGEDCOMList FLanguages;

		public TGEDCOMAddress Address
		{
			get { return base.TagClass("ADDR", typeof(TGEDCOMAddress), TGEDCOMAddress.Create) as TGEDCOMAddress; }
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
			get { return base.TagClass("NAME", typeof(TGEDCOMPersonalName), TGEDCOMPersonalName.Create) as TGEDCOMPersonalName; }
		}

		public string RegisteredReference
		{
			get { return base.GetTagStringValue("RFN"); }
			set { base.SetTagStringValue("RFN", value); }
		}

		public string GetLanguage(int Index)
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

		public int GetLanguagesCount()
		{
			return ((this.FLanguages == null) ? 0 : this.FLanguages.Count);
		}

		public void SetLanguages(int Index, [In] string Value)
		{
			if (Index >= 3)
			{
				throw new EGEDCOMException(string.Format("The maximum number of languages is {0}", TGEDCOMObject.GEDCOMMaxLanguages));
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

		protected override void CreateObj(TGEDCOMTree AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			base.SetLists(EnumSet.Create(new Enum[] { TGEDCOMSubList.stNotes, TGEDCOMSubList.stMultimedia }));
			this.FRecordType = TGEDCOMRecordType.rtSubmitter;
			this.FName = "SUBM";
			this.FLanguages = null;
		}

		public override void Dispose()
		{
			if (!this.Disposed_)
			{
				if (this.FLanguages != null) this.FLanguages.Free();
				base.Dispose();
				this.Disposed_ = true;
			}
		}

		public TGEDCOMTag AddLanguage(TGEDCOMTag Value)
		{
			if (this.FLanguages == null) this.FLanguages = new TGEDCOMList(this);
			if (this.FLanguages != null) this.FLanguages.Add(Value);
			return Value;
		}

		public override TGEDCOMTag AddTag([In] string ATag, [In] string AValue, TagConstructor ATagConstructor)
		{
			TGEDCOMTag Result;
			if (ATag == "NAME")
			{
				Result = base.AddTag(ATag, AValue, TGEDCOMPersonalName.Create);
			}
			else
			{
				if (ATag == "PHON" || ATag == "EMAIL" || ATag == "FAX" || ATag == "WWW")
				{
					Result = this.Address.AddTag(ATag, AValue, ATagConstructor);
				}
				else
				{
					if (ATag == "LANG")
					{
						Result = this.AddLanguage(new TGEDCOMTag(base.Owner, this, ATag, AValue));
					}
					else
					{
						// "ADDR" defines by default
						Result = base.AddTag(ATag, AValue, ATagConstructor);
					}
				}
			}
			return Result;
		}

		public override void Clear()
		{
			base.Clear();
			if (this.FLanguages != null) this.FLanguages.Clear();
		}

		public override bool IsEmpty()
		{
			return base.IsEmpty() && this.GetLanguagesCount() == 0;
		}

		public override void ReplaceXRefs(TXRefReplaceMap aMap)
		{
			base.ReplaceXRefs(aMap);
			if (this.FLanguages != null) this.FLanguages.ReplaceXRefs(aMap);
		}

		public override void ResetOwner(TGEDCOMTree AOwner)
		{
			base.ResetOwner(AOwner);
			if (this.FLanguages != null) this.FLanguages.ResetOwner(AOwner);
		}

		public TGEDCOMSubmitterRecord(TGEDCOMTree AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}

		public new static TGEDCOMCustomTag Create(TGEDCOMTree AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue)
		{
			return new TGEDCOMSubmitterRecord(AOwner, AParent, AName, AValue);
		}
	}
}
