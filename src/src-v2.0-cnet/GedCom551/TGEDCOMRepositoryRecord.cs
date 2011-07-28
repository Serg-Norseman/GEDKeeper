using GKSys;
using System;
using System.ComponentModel;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GedCom551
{
	public class TGEDCOMRepositoryRecord : TGEDCOMRecord
	{
		public TGEDCOMAddress Address
		{
			get { return this.GetAddress(); }
		}

		public string AutomatedRecordID
		{
			get { return this.GetStringTag(2); }
			set { this.SetStringTag(2, value); }
		}

		public string RepositoryName
		{
			get { return this.GetStringTag(1); }
			set { this.SetStringTag(1, value); }
		}

		internal string GetStringTag(int Index)
		{
			string Result = "";
			if (Index != 1)
			{
				if (Index == 2)
				{
					Result = base.GetTagStringValue("RIN");
				}
			}
			else
			{
				Result = base.GetTagStringValue("NAME");
			}
			return Result;
		}
		internal void SetStringTag(int Index, [In] string Value)
		{
			if (Index != 1)
			{
				if (Index == 2)
				{
					base.SetTagStringValue("RIN", Value);
				}
			}
			else
			{
				base.SetTagStringValue("NAME", Value);
			}
		}

		internal TGEDCOMAddress GetAddress()
		{
			return base.TagClass("ADDR", typeof(TGEDCOMAddress)) as TGEDCOMAddress;
		}

		protected internal override void CreateObj(TGEDCOMObject AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			base.SetLists(TEnumSet.Create(new Enum[]
			{
				TGEDCOMObject.TGEDCOMSubList.stNotes
			}));
			this.FRecordType = TGEDCOMRecord.TGEDCOMRecordType.rtRepository;
			this.FName = "REPO";
		}

		public override TGEDCOMTag AddTag([In] string ATag, [In] string AValue, Type AClass)
		{
			TGEDCOMTag Result;
			if (BDSSystem.WStrCmp(ATag, "ADDR") == 0)
			{
				Result = base.AddTag(ATag, AValue, typeof(TGEDCOMAddress));
			}
			else
			{
				if (BDSSystem.WStrCmp(ATag, "PHON") == 0 || BDSSystem.WStrCmp(ATag, "EMAIL") == 0 || BDSSystem.WStrCmp(ATag, "FAX") == 0 || BDSSystem.WStrCmp(ATag, "WWW") == 0)
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
		public override bool IsEmpty()
		{
			return base.IsEmpty();
		}
		public override void ReplaceXRefs(TXRefReplaceMap aMap)
		{
			base.ReplaceXRefs(aMap);
		}
		public override void ResetOwner(TGEDCOMObject AOwner)
		{
			base.ResetOwner(AOwner);
		}

		public TGEDCOMRepositoryRecord(TGEDCOMObject AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
