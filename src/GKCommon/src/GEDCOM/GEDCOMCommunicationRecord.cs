using GKCommon.GEDCOM.Enums;

namespace GKCommon.GEDCOM
{
	public sealed class GEDCOMCommunicationRecord : GEDCOMRecord
	{
		public static readonly string[] CommunicationTags = new string[] { "FROM", "TO" };

		public GEDCOMDateExact Date
		{
			get { return base.TagClass("DATE", GEDCOMDateExact.Create) as GEDCOMDateExact; }
		}

		public string CommName
		{
			get { return base.GetTagStringValue("NAME"); }
			set { base.SetTagStringValue("NAME", value); }
		}

		public GKCommunicationType CommunicationType
		{
			get { return GEDCOMUtils.GetCommunicationTypeVal(base.GetTagStringValue("TYPE")); }
			set { base.SetTagStringValue("TYPE", GEDCOMUtils.GetCommunicationTypeStr(value)); }
		}

		protected override void CreateObj(GEDCOMTree owner, GEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.fRecordType = GEDCOMRecordType.rtCommunication;
			this.fName = "_COMM";
		}

		public override GEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
		{
			GEDCOMTag result;

			if (tagName == "NAME")
			{
				result = base.AddTag(tagName, tagValue, null);
			}
			else if (tagName == "DATE")
			{
				result = base.AddTag(tagName, tagValue, GEDCOMDateExact.Create);
			}
			else
			{
				result = base.AddTag(tagName, tagValue, tagConstructor);
			}

			return result;
		}

		public void GetCorresponder(ref GKCommunicationDir aDir, ref GEDCOMIndividualRecord aCorresponder)
		{
			aCorresponder = null;

            GEDCOMTag cr_tag = base.FindTag("FROM", 0);
			if (cr_tag == null)
			{
				cr_tag = base.FindTag("TO", 0);
			}

            if (cr_tag != null)
			{
				aCorresponder = (this.fOwner.XRefIndex_Find(GEDCOMUtils.CleanXRef(cr_tag.StringValue)) as GEDCOMIndividualRecord);
				if (cr_tag.Name == "FROM")
				{
					aDir = GKCommunicationDir.cdFrom;
				}
				else if (cr_tag.Name == "TO")
				{
					aDir = GKCommunicationDir.cdTo;
				}
			}
		}

		public void SetCorresponder(GKCommunicationDir aDir, GEDCOMIndividualRecord aCorresponder)
		{
			base.DeleteTag("FROM");
			base.DeleteTag("TO");

            if (aCorresponder != null)
			{
				this.AddTag(GEDCOMCommunicationRecord.CommunicationTags[(int)aDir], GEDCOMUtils.EncloseXRef(aCorresponder.XRef), null);
			}
		}

		public GEDCOMCommunicationRecord(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static GEDCOMTag Create(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
		{
			return new GEDCOMCommunicationRecord(owner, parent, tagName, tagValue);
		}
	}
}
