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

		public void SetLanguage(int Index, string Value)
		{
			if (Index >= 0)
			{
				while (Index >= this.fLanguages.Count)
				{
					this.fLanguages.Add(new TGEDCOMTag(base.Owner, this, "LANG", ""));
				}
				(this.fLanguages[Index] as TGEDCOMTag).StringValue = Value;
			}
		}

		protected override void CreateObj(TGEDCOMTree owner, TGEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.FRecordType = TGEDCOMRecordType.rtSubmitter;
			this.FName = "SUBM";

			this.fLanguages = new GEDCOMList<TGEDCOMTag>(this);
		}

		public override void Dispose()
		{
			if (!this.Disposed_)
			{
				this.fLanguages.Dispose();

				base.Dispose();
				this.Disposed_ = true;
			}
		}

		public TGEDCOMTag AddLanguage(TGEDCOMTag Value)
		{
			this.fLanguages.Add(Value);
			return Value;
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

		public override void ReplaceXRefs(TXRefReplaceMap aMap)
		{
			base.ReplaceXRefs(aMap);
			this.fLanguages.ReplaceXRefs(aMap);
		}

		public override void ResetOwner(TGEDCOMTree AOwner)
		{
			base.ResetOwner(AOwner);
			this.fLanguages.ResetOwner(AOwner);
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
