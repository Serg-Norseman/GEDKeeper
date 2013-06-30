using System;

namespace GedCom551
{
	public sealed class TGEDCOMSubmitterRecord : TGEDCOMRecord
	{
		private GEDCOMList<TGEDCOMTag> FLanguages;

		public TGEDCOMAddress Address
		{
			get { return base.TagClass("ADDR", typeof(TGEDCOMAddress), TGEDCOMAddress.Create) as TGEDCOMAddress; }
		}

		public GEDCOMList<TGEDCOMTag> Languages
		{
			get { return this.FLanguages; }
		}

		public new TGEDCOMPersonalName Name
		{
			get { return base.TagClass("NAME", typeof(TGEDCOMPersonalName), TGEDCOMPersonalName.Create) as TGEDCOMPersonalName; }
		}

		public string RegisteredReference
		{
			get { return base.GetTagStringValue("RFN"); }
			set { base.SetTagStringValue("RFN", value); }
		}

		public void SetLanguage(int Index, string Value)
		{
			if (Index >= 0)
			{
				while (Index >= this.FLanguages.Count)
				{
					this.FLanguages.Add(new TGEDCOMTag(base.Owner, this, "LANG", ""));
				}
				(this.FLanguages[Index] as TGEDCOMTag).StringValue = Value;
			}
		}

		protected override void CreateObj(TGEDCOMTree owner, TGEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.FRecordType = TGEDCOMRecordType.rtSubmitter;
			this.FName = "SUBM";

			this.FLanguages = new GEDCOMList<TGEDCOMTag>(this);
		}

		public override void Dispose()
		{
			if (!this.Disposed_)
			{
				this.FLanguages.Dispose();

				base.Dispose();
				this.Disposed_ = true;
			}
		}

		public TGEDCOMTag AddLanguage(TGEDCOMTag Value)
		{
			this.FLanguages.Add(Value);
			return Value;
		}

		public override TGEDCOMTag AddTag(string ATag, string AValue, TagConstructor ATagConstructor)
		{
			TGEDCOMTag Result;

			if (ATag == "NAME")
			{
				Result = base.AddTag(ATag, AValue, TGEDCOMPersonalName.Create);
			}
			else if (ATag == "PHON" || ATag == "EMAIL" || ATag == "FAX" || ATag == "WWW")
			{
				Result = this.Address.AddTag(ATag, AValue, ATagConstructor);
			}
			else if (ATag == "LANG")
			{
				Result = this.AddLanguage(new TGEDCOMTag(base.Owner, this, ATag, AValue));
			}
			else
			{
				// "ADDR" defines by default
				Result = base.AddTag(ATag, AValue, ATagConstructor);
			}

			return Result;
		}

		public override void Clear()
		{
			base.Clear();
			this.FLanguages.Clear();
		}

		public override bool IsEmpty()
		{
			return base.IsEmpty() && (this.FLanguages.Count == 0);
		}

		public override void ReplaceXRefs(TXRefReplaceMap aMap)
		{
			base.ReplaceXRefs(aMap);
			this.FLanguages.ReplaceXRefs(aMap);
		}

		public override void ResetOwner(TGEDCOMTree AOwner)
		{
			base.ResetOwner(AOwner);
			this.FLanguages.ResetOwner(AOwner);
		}

		public TGEDCOMSubmitterRecord(TGEDCOMTree owner, TGEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static TGEDCOMTag Create(TGEDCOMTree owner, TGEDCOMObject parent, string tagName, string tagValue)
		{
			return new TGEDCOMSubmitterRecord(owner, parent, tagName, tagValue);
		}
	}
}
