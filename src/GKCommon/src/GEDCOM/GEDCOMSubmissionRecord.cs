namespace GKCommon.GEDCOM
{
	public sealed class GEDCOMSubmissionRecord : GEDCOMRecord
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

		public GEDCOMOrdinanceProcessFlag OrdinanceProcessFlag
		{
			get { return GEDCOMUtils.GetOrdinanceProcessFlagVal(base.GetTagStringValue("ORDI")); }
			set { base.SetTagStringValue("ORDI", GEDCOMUtils.GetOrdinanceProcessFlagStr(value)); }
		}

		public GEDCOMPointer Submitter
		{
			get { return base.TagClass("SUBM", GEDCOMPointer.Create) as GEDCOMPointer; }
		}

		protected override void CreateObj(GEDCOMTree owner, GEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			base.SetRecordType(GEDCOMRecordType.rtSubmission);
			base.SetName("SUBN");
		}

		public override GEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
		{
			GEDCOMTag result;

			if (tagName == "SUBM")
			{
				result = base.AddTag(tagName, tagValue, GEDCOMPointer.Create);
			}
			else
			{
				result = base.AddTag(tagName, tagValue, tagConstructor);
			}

			return result;
		}

		public GEDCOMSubmissionRecord(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static GEDCOMTag Create(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
		{
			return new GEDCOMSubmissionRecord(owner, parent, tagName, tagValue);
		}
	}
}
