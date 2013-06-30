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
			get { return GetPriorityVal(base.GetTagStringValue("_PRIORITY").Trim().ToLower()); }
			set { base.SetTagStringValue("_PRIORITY", GetPriorityStr(value)); }
		}

		public TGEDCOMDateExact StartDate
		{
			get { return base.TagClass("_STARTDATE", typeof(TGEDCOMDateExact), TGEDCOMDateExact.Create) as TGEDCOMDateExact; }
		}

		public TGEDCOMDateExact StopDate
		{
			get { return base.TagClass("_STOPDATE", typeof(TGEDCOMDateExact), TGEDCOMDateExact.Create) as TGEDCOMDateExact; }
		}

		protected override void CreateObj(TGEDCOMTree owner, TGEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.FRecordType = TGEDCOMRecordType.rtTask;
			this.FName = "_TASK";
		}

		public override TGEDCOMTag AddTag(string ATag, string AValue, TagConstructor ATagConstructor)
		{
			TGEDCOMTag Result;
			if (ATag == "_STARTDATE" || ATag == "_STOPDATE")
			{
				Result = base.AddTag(ATag, AValue, TGEDCOMDateExact.Create);
			}
			else
			{
				Result = base.AddTag(ATag, AValue, ATagConstructor);
			}
			return Result;
		}

        public void aux_GetTaskGoal(ref TGoalType aType, ref TGEDCOMRecord aGoalRec)
        {
            TGEDCOMTree tree = this.Owner;
            aGoalRec = tree.XRefIndex_Find(TGEDCOMObject.CleanXRef(this.Goal));

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

		public TGEDCOMTaskRecord(TGEDCOMTree owner, TGEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static TGEDCOMTag Create(TGEDCOMTree owner, TGEDCOMObject parent, string tagName, string tagValue)
		{
			return new TGEDCOMTaskRecord(owner, parent, tagName, tagValue);
		}
	}
}
