using System;

namespace GedCom551
{
	public sealed class TGEDCOMDateStatus : TGEDCOMTag
	{
		public TGEDCOMDateExact ChangeDate
		{
			get { return base.TagClass("DATE", TGEDCOMDateExact.Create) as TGEDCOMDateExact; }
		}

		protected override void CreateObj(TGEDCOMTree owner, GEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.fName = "STAT";
		}

		public override TGEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
		{
			TGEDCOMTag result;

			if (tagName == "DATE")
			{
				result = base.AddTag(tagName, tagValue, TGEDCOMDateExact.Create);
			}
			else
			{
				result = base.AddTag(tagName, tagValue, tagConstructor);
			}

			return result;
		}

		public TGEDCOMDateStatus(TGEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static TGEDCOMTag Create(TGEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
		{
			return new TGEDCOMDateStatus(owner, parent, tagName, tagValue);
		}
	}
}
