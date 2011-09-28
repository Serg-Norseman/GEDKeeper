using System;
using System.Runtime.InteropServices;

using GKCore.Sys;

namespace GedCom551
{
	public sealed class TGEDCOMRepositoryRecord : TGEDCOMRecord
	{
		public TGEDCOMAddress Address
		{
			get { return this.GetAddress(); }
		}

		private TGEDCOMAddress GetAddress()
		{
			return base.TagClass("ADDR", typeof(TGEDCOMAddress)) as TGEDCOMAddress;
		}

		public string RepositoryName
		{
			get { return base.GetTagStringValue("NAME"); }
			set { base.SetTagStringValue("NAME", value); }
		}

		protected override void CreateObj(TGEDCOMObject AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			base.SetLists(TEnumSet.Create(new Enum[]
			{
				TGEDCOMSubList.stNotes
			}));
			this.FRecordType = TGEDCOMRecordType.rtRepository;
			this.FName = "REPO";
		}

		public override TGEDCOMTag AddTag([In] string ATag, [In] string AValue, Type AClass)
		{
			TGEDCOMTag Result;

			if (ATag == "ADDR")
			{
				Result = base.AddTag(ATag, AValue, typeof(TGEDCOMAddress));
			}
			else
			{
				if (ATag == "PHON" || ATag == "EMAIL" || ATag == "FAX" || ATag == "WWW")
				{
					Result = this.GetAddress().AddTag(ATag, AValue, AClass);
				}
				else
				{
					Result = base.AddTag(ATag, AValue, AClass);
				}
			}

			return Result;
		}

		public TGEDCOMRepositoryRecord(TGEDCOMObject AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
