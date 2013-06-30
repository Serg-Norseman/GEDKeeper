using System;

namespace GedCom551
{
	public sealed class TGEDCOMDateExact : TGEDCOMDate
	{
		public TGEDCOMDateExact(TGEDCOMTree owner, TGEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static TGEDCOMTag Create(TGEDCOMTree owner, TGEDCOMObject parent, string tagName, string tagValue)
		{
			return new TGEDCOMDateExact(owner, parent, tagName, tagValue);
		}
	}
}
