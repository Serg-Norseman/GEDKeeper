using GKSys;
using System;
using System.ComponentModel;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GedCom551
{
	public class TGEDCOMLocationRecord : TGEDCOMRecord
	{
		public TGEDCOMMap Map
		{
			get { return this.GetMap(); }
		}

		public new string Name
		{
			get { return this.GetName(); }
			set { this.SetName(value); }
		}

		internal string GetName()
		{
			return base.GetTagStringValue("NAME");
		}

		internal void SetName([In] string Value)
		{
			base.SetTagStringValue("NAME", Value);
		}

		internal TGEDCOMMap GetMap()
		{
			return base.TagClass("MAP", typeof(TGEDCOMMap)) as TGEDCOMMap;
		}

		protected internal override void CreateObj(TGEDCOMObject AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			base.SetLists(TEnumSet.Create(new Enum[]
			{
				TGEDCOMObject.TGEDCOMSubList.stNotes, 
				TGEDCOMObject.TGEDCOMSubList.stMultimedia
			}));
			this.FRecordType = TGEDCOMRecord.TGEDCOMRecordType.rtLocation;
			this.FName = "_LOC";
		}

		public override TGEDCOMTag AddTag([In] string ATag, [In] string AValue, Type AClass)
		{
			TGEDCOMTag Result;
			if (BDSSystem.WStrCmp(ATag, "MAP") == 0)
			{
				Result = base.AddTag(ATag, AValue, typeof(TGEDCOMMap));
			}
			else
			{
				Result = base.AddTag(ATag, AValue, AClass);
			}
			return Result;
		}

		public TGEDCOMLocationRecord(TGEDCOMObject AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
