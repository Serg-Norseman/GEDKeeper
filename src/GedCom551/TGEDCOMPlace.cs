using System;
using System.Runtime.InteropServices;

using Ext.Utils;

namespace GedCom551
{
	public sealed class TGEDCOMPlace : TGEDCOMTagWithLists
	{
		public string Form
		{
			get { return base.GetTagStringValue("FORM"); }
			set { base.SetTagStringValue("FORM", value); }
		}

		public TGEDCOMPointer Location
		{
			get { return base.TagClass("_LOC", typeof(TGEDCOMPointer), TGEDCOMPointer.Create) as TGEDCOMPointer; }
		}

		public TGEDCOMMap Map
		{
			get { return base.TagClass("MAP", typeof(TGEDCOMMap), TGEDCOMMap.Create) as TGEDCOMMap; }
		}

		protected override void CreateObj(TGEDCOMTree AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			base.SetLists(EnumSet.Create(new Enum[] { TGEDCOMSubList.stNotes }));
			this.FName = "PLAC";
		}

		public override TGEDCOMTag AddTag([In] string ATag, [In] string AValue, TagConstructor ATagConstructor)
		{
			// "MAP", "_LOC" defines by default
			return base.AddTag(ATag, AValue, ATagConstructor);
		}

		public TGEDCOMPlace(TGEDCOMTree AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}

		public new static TGEDCOMCustomTag Create(TGEDCOMTree AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue)
		{
			return new TGEDCOMPlace(AOwner, AParent, AName, AValue);
		}
	}
}
