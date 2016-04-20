namespace GKCommon.GEDCOM
{
	public sealed class GEDCOMTaskRecord : GEDCOMRecord
	{
		public string Goal
		{
			get { return base.GetTagStringValue("_GOAL"); }
			set { base.SetTagStringValue("_GOAL", value); }
		}

		public GKResearchPriority Priority
		{
			get { return GEDCOMUtils.GetPriorityVal(base.GetTagStringValue("_PRIORITY")); }
			set { base.SetTagStringValue("_PRIORITY", GEDCOMUtils.GetPriorityStr(value)); }
		}

		public GEDCOMDateExact StartDate
		{
			get { return base.TagClass("_STARTDATE", GEDCOMDateExact.Create) as GEDCOMDateExact; }
		}

		public GEDCOMDateExact StopDate
		{
			get { return base.TagClass("_STOPDATE", GEDCOMDateExact.Create) as GEDCOMDateExact; }
		}

		protected override void CreateObj(GEDCOMTree owner, GEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			base.SetRecordType(GEDCOMRecordType.rtTask);
            base.SetName("_TASK");
		}

		public override GEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
		{
			GEDCOMTag result;

			if (tagName == "_STARTDATE" || tagName == "_STOPDATE")
			{
				result = base.AddTag(tagName, tagValue, GEDCOMDateExact.Create);
			}
			else
			{
				result = base.AddTag(tagName, tagValue, tagConstructor);
			}

			return result;
		}

		public GEDCOMTaskRecord(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static GEDCOMTag Create(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
		{
			return new GEDCOMTaskRecord(owner, parent, tagName, tagValue);
		}

		#region Auxiliary

        public void GetTaskGoal(out GKGoalType goalType, out GEDCOMRecord goalRec)
        {
            GEDCOMTree tree = this.Owner;
            goalRec = tree.XRefIndex_Find(GEDCOMUtils.CleanXRef(this.Goal));

            if (goalRec is GEDCOMIndividualRecord)
            {
            	goalType = GKGoalType.gtIndividual;
            }
            else if (goalRec is GEDCOMFamilyRecord)
            {
            	goalType = GKGoalType.gtFamily;
            }
            else if (goalRec is GEDCOMSourceRecord)
            {
            	goalType = GKGoalType.gtSource;
            }
            else
            {
            	goalType = GKGoalType.gtOther;
            }
        }

		#endregion
	}
}
