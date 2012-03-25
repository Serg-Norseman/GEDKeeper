using System;
using System.Runtime.InteropServices;

using Ext.Utils;

namespace GedCom551
{
	public sealed class TGEDCOMLocationRecord : TGEDCOMRecord
	{
		public TGEDCOMMap Map
		{
			get { return base.TagClass("MAP", typeof(TGEDCOMMap), TGEDCOMMap.Create) as TGEDCOMMap; }
		}

		public string LocationName
		{
			get { return base.GetTagStringValue("NAME"); }
			set { base.SetTagStringValue("NAME", value); }
		}

		protected override void CreateObj(TGEDCOMTree AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			base.SetLists(EnumSet.Create(new Enum[] { TGEDCOMSubList.stNotes, TGEDCOMSubList.stMultimedia }));
			this.FRecordType = TGEDCOMRecordType.rtLocation;
			this.FName = "_LOC";
		}

		public override TGEDCOMTag AddTag([In] string ATag, [In] string AValue, TagConstructor ATagConstructor)
		{
			// "MAP" defines by default
			return base.AddTag(ATag, AValue, ATagConstructor);
		}

		public TGEDCOMLocationRecord(TGEDCOMTree AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}

		public new static TGEDCOMCustomTag Create(TGEDCOMTree AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue)
		{
			return new TGEDCOMLocationRecord(AOwner, AParent, AName, AValue);
		}
	}
}
