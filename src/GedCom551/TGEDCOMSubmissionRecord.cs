using System;
using System.Runtime.InteropServices;

using Ext.Utils;

namespace GedCom551
{
	public sealed class TGEDCOMSubmissionRecord : TGEDCOMRecord
	{
		public string FamilyFileName
		{
			get { return base.GetTagStringValue("FAMF"); }
			set { base.SetTagStringValue("FAMF", value); }
		}

		public string TempleCode
		{
			get { return base.GetTagStringValue("TEMP"); }
			set { base.SetTagStringValue("TEMP", value); }
		}

		public int GenerationsOfAncestors
		{
			get { return base.GetTagIntegerValue("ANCE", 0); }
			set { base.SetTagIntegerValue("ANCE", value); }
		}

		public int GenerationsOfDescendants
		{
			get { return base.GetTagIntegerValue("DESC", 0); }
			set { base.SetTagIntegerValue("DESC", value); }
		}

		public TGEDCOMOrdinanceProcessFlag OrdinanceProcessFlag
		{
			get { return GetOrdinanceProcessFlagVal(base.GetTagStringValue("ORDI").Trim().ToUpper()); }
			set { base.SetTagStringValue("ORDI", GetOrdinanceProcessFlagStr(value)); }
		}

		public TGEDCOMPointer Submitter
		{
			get { return base.TagClass("SUBM", typeof(TGEDCOMPointer), TGEDCOMPointer.Create) as TGEDCOMPointer; }
		}

		protected override void CreateObj(TGEDCOMTree AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			base.SetLists(EnumSet.Create(new Enum[] { TGEDCOMSubList.stNotes }));
			this.FRecordType = TGEDCOMRecordType.rtSubmission;
			this.FName = "SUBN";
		}

		public override TGEDCOMTag AddTag([In] string ATag, [In] string AValue, TagConstructor ATagConstructor)
		{
			TGEDCOMTag Result;

			if (ATag == "SUBM")
			{
				Result = base.AddTag(ATag, AValue, TGEDCOMPointer.Create);
			}
			else
			{
				Result = base.AddTag(ATag, AValue, ATagConstructor);
			}

			return Result;
		}

		public TGEDCOMSubmissionRecord(TGEDCOMTree AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}

        public new static TGEDCOMTag Create(TGEDCOMTree AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue)
		{
			return new TGEDCOMSubmissionRecord(AOwner, AParent, AName, AValue);
		}
	}
}
