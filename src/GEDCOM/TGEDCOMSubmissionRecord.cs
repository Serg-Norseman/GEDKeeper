using System;

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
			get { return GEDCOMUtils.GetOrdinanceProcessFlagVal(base.GetTagStringValue("ORDI")); }
			set { base.SetTagStringValue("ORDI", GEDCOMUtils.GetOrdinanceProcessFlagStr(value)); }
		}

		public TGEDCOMPointer Submitter
		{
			get { return base.TagClass("SUBM", TGEDCOMPointer.Create) as TGEDCOMPointer; }
		}

		protected override void CreateObj(TGEDCOMTree owner, GEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.fRecordType = TGEDCOMRecordType.rtSubmission;
			this.fName = "SUBN";
		}

		public override TGEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
		{
			TGEDCOMTag result;

			if (tagName == "SUBM")
			{
				result = base.AddTag(tagName, tagValue, TGEDCOMPointer.Create);
			}
			else
			{
				result = base.AddTag(tagName, tagValue, tagConstructor);
			}

			return result;
		}

		public TGEDCOMSubmissionRecord(TGEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static TGEDCOMTag Create(TGEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
		{
			return new TGEDCOMSubmissionRecord(owner, parent, tagName, tagValue);
		}
	}
}
