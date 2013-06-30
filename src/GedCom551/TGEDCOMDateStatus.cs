using System;

namespace GedCom551
{
	public sealed class TGEDCOMDateStatus : TGEDCOMTag
	{
		public TGEDCOMDateExact ChangeDate
		{
			get {
				return base.TagClass("DATE", typeof(TGEDCOMDateExact), TGEDCOMDateExact.Create) as TGEDCOMDateExact;
			}
		}

		protected override void CreateObj(TGEDCOMTree owner, TGEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.FName = "STAT";
		}

		public override TGEDCOMTag AddTag(string ATag, string AValue, TagConstructor ATagConstructor)
		{
			TGEDCOMTag Result;

			if (ATag == "DATE")
			{
				Result = base.AddTag(ATag, AValue, TGEDCOMDateExact.Create);
			}
			else
			{
				Result = base.AddTag(ATag, AValue, ATagConstructor);
			}

			return Result;
		}

		public TGEDCOMDateStatus(TGEDCOMTree owner, TGEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static TGEDCOMTag Create(TGEDCOMTree owner, TGEDCOMObject parent, string tagName, string tagValue)
		{
			return new TGEDCOMDateStatus(owner, parent, tagName, tagValue);
		}
	}
}
