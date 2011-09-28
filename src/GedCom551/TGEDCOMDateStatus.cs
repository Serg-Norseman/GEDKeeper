using System;
using System.Runtime.InteropServices;

namespace GedCom551
{
	public sealed class TGEDCOMDateStatus : TGEDCOMTag
	{
		public TGEDCOMDateExact ChangeDate
		{
			get { return this.GetChangeDate(); }
		}

		private TGEDCOMDateExact GetChangeDate()
		{
			return base.TagClass("DATE", typeof(TGEDCOMDateExact)) as TGEDCOMDateExact;
		}

		protected override void CreateObj(TGEDCOMObject AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			this.FName = "STAT";
		}

		public override TGEDCOMTag AddTag([In] string ATag, [In] string AValue, Type AClass)
		{
			TGEDCOMTag Result;

			if (ATag == "DATE")
			{
				Result = base.AddTag(ATag, AValue, typeof(TGEDCOMDateExact));
			}
			else
			{
				Result = base.AddTag(ATag, AValue, AClass);
			}

			return Result;
		}

		public TGEDCOMDateStatus(TGEDCOMObject AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
