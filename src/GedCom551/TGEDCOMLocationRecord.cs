using System;
using System.Runtime.InteropServices;

using GKCore.Sys;

namespace GedCom551
{
	public sealed class TGEDCOMLocationRecord : TGEDCOMRecord
	{
		public TGEDCOMMap Map
		{
			get { return base.TagClass("MAP", typeof(TGEDCOMMap)) as TGEDCOMMap; }
		}

		public string LocationName
		{
			get { return base.GetTagStringValue("NAME"); }
			set { base.SetTagStringValue("NAME", value); }
		}

		protected override void CreateObj(TGEDCOMObject AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			base.SetLists(TEnumSet.Create(new Enum[]
			{
				TGEDCOMSubList.stNotes, 
				TGEDCOMSubList.stMultimedia
			}));
			this.FRecordType = TGEDCOMRecordType.rtLocation;
			this.FName = "_LOC";
		}

		public override TGEDCOMTag AddTag([In] string ATag, [In] string AValue, Type AClass)
		{
			TGEDCOMTag Result;

			if (ATag == "MAP")
			{
				Result = base.AddTag(ATag, AValue, typeof(TGEDCOMMap));
			} else {
				Result = base.AddTag(ATag, AValue, AClass);
			}

			return Result;
		}

		public TGEDCOMLocationRecord(TGEDCOMObject AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
