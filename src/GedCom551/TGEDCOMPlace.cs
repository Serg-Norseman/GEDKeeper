using System;
using System.IO;
using System.Runtime.InteropServices;

using GKCore.Sys;

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
			get { return base.TagClass("_LOC", typeof(TGEDCOMPointer)) as TGEDCOMPointer; }
		}

		public TGEDCOMMap Map
		{
			get { return base.TagClass("MAP", typeof(TGEDCOMMap)) as TGEDCOMMap; }
		}

		protected override void CreateObj(TGEDCOMObject AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			base.SetLists(TEnumSet.Create(new Enum[]
			{
				TGEDCOMSubList.stNotes
			}));
			this.FName = "PLAC";
		}

		public override TGEDCOMTag AddTag([In] string ATag, [In] string AValue, Type AClass)
		{
			TGEDCOMTag Result;
			if (ATag == "_LOC")
			{
				Result = base.AddTag(ATag, AValue, typeof(TGEDCOMPointer));
			}
			else
			{
				if (ATag == "MAP")
				{
					Result = base.AddTag(ATag, AValue, typeof(TGEDCOMMap));
				}
				else
				{
					Result = base.AddTag(ATag, AValue, AClass);
				}
			}
			return Result;
		}

		public TGEDCOMPlace(TGEDCOMObject AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
