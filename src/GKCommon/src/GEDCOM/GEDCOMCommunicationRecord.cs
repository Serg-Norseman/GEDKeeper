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

		public GEDCOMCommunicationRecord(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static GEDCOMTag Create(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
		{
			return new GEDCOMCommunicationRecord(owner, parent, tagName, tagValue);
		}
        
        #region Auxiliary

		public void GetCorresponder(out GKCommunicationDir commDir, out GEDCOMIndividualRecord corresponder)
		{
			commDir = GKCommunicationDir.cdFrom;
			corresponder = null;

            GEDCOMTag corrTag = base.FindTag("FROM", 0);
			if (corrTag == null) {
				corrTag = base.FindTag("TO", 0);
			}

            if (corrTag != null) {
				corresponder = (this.fOwner.XRefIndex_Find(GEDCOMUtils.CleanXRef(corrTag.StringValue)) as GEDCOMIndividualRecord);

				if (corrTag.Name == "FROM") {
					commDir = GKCommunicationDir.cdFrom;
				} else if (corrTag.Name == "TO") {
					commDir = GKCommunicationDir.cdTo;
				}
			}
		}

		public void SetCorresponder(GKCommunicationDir commDir, GEDCOMIndividualRecord corresponder)
		{
			base.DeleteTag("FROM");
			base.DeleteTag("TO");

            if (corresponder != null) {
				this.AddTag(GEDCOMCommunicationRecord.CommunicationTags[(int)commDir], GEDCOMUtils.EncloseXRef(corresponder.XRef), null);
			}
		}

		#endregion
	}
}
