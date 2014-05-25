using System;

namespace GedCom551
{
	public sealed class TGEDCOMTaskRecord : TGEDCOMRecord
	{
		public string Goal
		{
			get { return base.GetTagStringValue("_GOAL"); }
			set { base.SetTagStringValue("_GOAL", value); }
		}

		public TResearchPriority Priority
		{
			get { return GEDCOMUtils.GetPriorityVal(base.GetTagStringValue("_PRIORITY")); }
			set { base.SetTagStringValue("_PRIORITY", GEDCOMUtils.GetPriorityStr(value)); }
		}

		public TGEDCOMDateExact StartDate
		{
			get { return base.TagClass("_STARTDATE", TGEDCOMDateExact.Create) as TGEDCOMDateExact; }
		}

		public TGEDCOMDateExact StopDate
		{
			get { return base.TagClass("_STOPDATE", TGEDCOMDateExact.Create) as TGEDCOMDateExact; }
		}

		protected override void CreateObj(TGEDCOMTree owner, GEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.fRecordType = TGEDCOMRecordType.rtTask;
			this.fName = "_TASK";
		}

		public override TGEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
		{
			TGEDCOMTag result;

			if (tagName == "_STARTDATE" || tagName == "_STOPDATE")
			{
				result = base.AddTag(tagName, tagValue, TGEDCOMDateExact.Create);
			}
			else
			{
				result = base.AddTag(tagName, tagValue, tagConstructor);
			}

			return result;
		}

		public TGEDCOMTaskRecord(TGEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static TGEDCOMTag Create(TGEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
		{
			return new TGEDCOMTaskRecord(owner, parent, tagName, tagValue);
		}

		#region Auxiliary

        public void aux_GetTaskGoal(out TGoalType aType, out TGEDCOMRecord aGoalRec)
        {
            TGEDCOMTree tree = this.Owner;
            aGoalRec = tree.XRefIndex_Find(GEDCOMUtils.CleanXRef(this.Goal));

            if (aGoalRec is TGEDCOMIndividualRecord)
            {
            	aType = TGoalType.gtIndividual;
            }
            else if (aGoalRec is TGEDCOMFamilyRecord)
            {
            	aType = TGoalType.gtFamily;
            }
            else if (aGoalRec is TGEDCOMSourceRecord)
            {
            	aType = TGoalType.gtSource;
            }
            else
            {
            	aType = TGoalType.gtOther;
            }
        }

		#endregion
	}
}
