using GKSys;
using System;
using System.ComponentModel;
using System.IO;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GedCom551
{
	public class TGEDCOMPlace : TGEDCOMTagWithLists
	{
		[Browsable(false)]
		public string Form
		{
			get { return this.GetStringTag(); }
			set { this.SetStringTag(value); }
		}

		[Browsable(false)]
		public TGEDCOMPointer Location
		{
			get { return this.GetLocation(); }
		}

		[Browsable(false)]
		public TGEDCOMMap Map
		{
			get { return this.GetMap(); }
		}

		internal TGEDCOMMap GetMap()
		{
			return base.TagClass("MAP", typeof(TGEDCOMMap)) as TGEDCOMMap;
		}

		internal TGEDCOMPointer GetLocation()
		{
			return base.TagClass("_LOC", typeof(TGEDCOMPointer)) as TGEDCOMPointer;
		}
		internal string GetStringTag()
		{
			return base.GetTagStringValue("FORM");
		}
		internal void SetStringTag([In] string Value)
		{
			base.SetTagStringValue("FORM", Value);
		}

		protected internal override void CreateObj(TGEDCOMObject AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			base.SetLists(TEnumSet.Create(new Enum[]
			{
				TGEDCOMObject.TGEDCOMSubList.stNotes
			}));
			this.FName = "PLAC";
		}

		public override TGEDCOMTag AddTag([In] string ATag, [In] string AValue, Type AClass)
		{
			TGEDCOMTag Result;
			if (BDSSystem.WStrCmp(ATag, "_LOC") == 0)
			{
				Result = base.AddTag(ATag, AValue, typeof(TGEDCOMPointer));
			}
			else
			{
				if (BDSSystem.WStrCmp(ATag, "MAP") == 0)
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

		public override void SaveToStream(StreamWriter AStream)
		{
			base.SaveToStream(AStream);
		}

		public TGEDCOMPlace(TGEDCOMObject AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
