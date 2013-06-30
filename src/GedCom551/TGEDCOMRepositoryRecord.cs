using System;

namespace GedCom551
{
	public sealed class TGEDCOMRepositoryRecord : TGEDCOMRecord
	{
		public TGEDCOMAddress Address
		{
			get { return base.TagClass("ADDR", typeof(TGEDCOMAddress), TGEDCOMAddress.Create) as TGEDCOMAddress; }
		}

		public string RepositoryName
		{
			get { return base.GetTagStringValue("NAME"); }
			set { base.SetTagStringValue("NAME", value); }
		}

		protected override void CreateObj(TGEDCOMTree owner, TGEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.FRecordType = TGEDCOMRecordType.rtRepository;
			this.FName = "REPO";
		}

		public override TGEDCOMTag AddTag(string ATag, string AValue, TagConstructor ATagConstructor)
		{
			TGEDCOMTag Result;

			if (ATag == "PHON" || ATag == "EMAIL" || ATag == "FAX" || ATag == "WWW")
			{
				Result = this.Address.AddTag(ATag, AValue, ATagConstructor);
			}
			else
			{
				// "ADDR" defines by default
				Result = base.AddTag(ATag, AValue, ATagConstructor);
			}

			return Result;
		}

		public TGEDCOMRepositoryRecord(TGEDCOMTree owner, TGEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static TGEDCOMTag Create(TGEDCOMTree owner, TGEDCOMObject parent, string tagName, string tagValue)
		{
			return new TGEDCOMRepositoryRecord(owner, parent, tagName, tagValue);
		}
	}
}
