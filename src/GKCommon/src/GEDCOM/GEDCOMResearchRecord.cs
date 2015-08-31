using System.IO;
using GKCommon.GEDCOM.Enums;

namespace GKCommon.GEDCOM
{
	public sealed class GEDCOMResearchRecord : GEDCOMRecord
	{
		private GEDCOMList<GEDCOMPointer> fTasks;
		private GEDCOMList<GEDCOMPointer> fCommunications;
		private GEDCOMList<GEDCOMPointer> fGroups;

		public string ResearchName
		{
			get { return base.GetTagStringValue("NAME"); }
			set { base.SetTagStringValue("NAME", value); }
		}

		public GKResearchPriority Priority
		{
			get { return GEDCOMUtils.GetPriorityVal(base.GetTagStringValue("_PRIORITY")); }
			set { base.SetTagStringValue("_PRIORITY", GEDCOMUtils.GetPriorityStr(value)); }
		}

		public GKResearchStatus Status
		{
			get { return GEDCOMUtils.GetStatusVal(base.GetTagStringValue("_STATUS")); }
			set { base.SetTagStringValue("_STATUS", GEDCOMUtils.GetStatusStr(value)); }
		}

		public GEDCOMDateExact StartDate
		{
			get { return base.TagClass("_STARTDATE", GEDCOMDateExact.Create) as GEDCOMDateExact; }
		}

		public GEDCOMDateExact StopDate
		{
			get { return base.TagClass("_STOPDATE", GEDCOMDateExact.Create) as GEDCOMDateExact; }
		}

		public int Percent
		{
			get { return base.GetTagIntegerValue("_PERCENT", 0); }
			set { base.SetTagIntegerValue("_PERCENT", value); }
		}

		public GEDCOMList<GEDCOMPointer> Tasks
		{
			get { return this.fTasks; }
		}

		public GEDCOMList<GEDCOMPointer> Communications
		{
			get { return this.fCommunications; }
		}

		public GEDCOMList<GEDCOMPointer> Groups
		{
			get { return this.fGroups; }
		}

		protected override void CreateObj(GEDCOMTree owner, GEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.fRecordType = GEDCOMRecordType.rtResearch;
			this.fName = "_RESEARCH";

			this.fTasks = new GEDCOMList<GEDCOMPointer>(this);
			this.fCommunications = new GEDCOMList<GEDCOMPointer>(this);
			this.fGroups = new GEDCOMList<GEDCOMPointer>(this);
		}

		protected override void Dispose(bool disposing)
		{
			if (disposing)
			{
				this.fTasks.Dispose();
				this.fCommunications.Dispose();
				this.fGroups.Dispose();
			}
			base.Dispose(disposing);
		}

		public override GEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
		{
			GEDCOMTag result;

			if (tagName == "NAME")
			{
				result = base.AddTag(tagName, tagValue, null);
			}
			else if (tagName == "_STARTDATE" || tagName == "_STOPDATE")
			{
				result = base.AddTag(tagName, tagValue, GEDCOMDateExact.Create);
			}
			else if (tagName == "_TASK")
			{
				result = this.fTasks.Add(new GEDCOMPointer(base.Owner, this, tagName, tagValue));
			}
			else if (tagName == "_COMM")
			{
				result = this.fCommunications.Add(new GEDCOMPointer(base.Owner, this, tagName, tagValue));
			}
			else if (tagName == "_GROUP")
			{
				result = this.fGroups.Add(new GEDCOMPointer(base.Owner, this, tagName, tagValue));
			}
			else
			{
				result = base.AddTag(tagName, tagValue, tagConstructor);
			}

			return result;
		}

		public override void Clear()
		{
			base.Clear();

			this.fTasks.Clear();
			this.fCommunications.Clear();
			this.fGroups.Clear();
		}

		public override bool IsEmpty()
		{
			return base.IsEmpty() && this.fTasks.Count == 0 && this.fCommunications.Count == 0 && this.fGroups.Count == 0;
		}

		public override void ReplaceXRefs(XRefReplacer map)
		{
            base.ReplaceXRefs(map);

            this.fTasks.ReplaceXRefs(map);
            this.fCommunications.ReplaceXRefs(map);
            this.fGroups.ReplaceXRefs(map);
		}

		public override void ResetOwner(GEDCOMTree newOwner)
		{
			base.ResetOwner(newOwner);

			this.fTasks.ResetOwner(newOwner);
			this.fCommunications.ResetOwner(newOwner);
			this.fGroups.ResetOwner(newOwner);
		}

		public override void SaveToStream(StreamWriter stream)
		{
			base.SaveToStream(stream);

			this.fTasks.SaveToStream(stream);
			this.fCommunications.SaveToStream(stream);
			this.fGroups.SaveToStream(stream);
		}

		public int IndexOfTask(GEDCOMTaskRecord taskRec)
		{
			if (taskRec != null) {
				int num = this.fTasks.Count;
				for (int i = 0; i < num; i++)
				{
					if (this.fTasks[i].XRef == taskRec.XRef) {
						return i;
					}
				}
			}

			return -1;
		}

		public int IndexOfCommunication(GEDCOMCommunicationRecord commRec)
		{
			if (commRec != null) {
				int num = this.fCommunications.Count;
				for (int i = 0; i < num; i++)
				{
					if (this.fCommunications[i].XRef == commRec.XRef) {
						return i;
					}
				}
			}

			return -1;
		}

		public int IndexOfGroup(GEDCOMGroupRecord groupRec)
		{
			if (groupRec != null) {
				int num = this.fGroups.Count;
				for (int i = 0; i < num; i++)
				{
					if (this.fGroups[i].XRef == groupRec.XRef) {
						return i;
					}
				}
			}

			return -1;
		}

		public GEDCOMResearchRecord(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static GEDCOMTag Create(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
		{
			return new GEDCOMResearchRecord(owner, parent, tagName, tagValue);
		}

        #region Auxiliary

        public bool AddTask(GEDCOMTaskRecord taskRecord)
		{
			bool result = false;

			if (taskRecord != null)
			{
				GEDCOMPointer ptr = new GEDCOMPointer(this.Owner, this, "", "");
				ptr.SetNamedValue("_TASK", taskRecord);
				this.fTasks.Add(ptr);
				result = true;
			}

			return result;
		}

		public void RemoveTask(GEDCOMTaskRecord taskRecord)
		{
            if (taskRecord == null) return;

			this.fTasks.Delete(this.IndexOfTask(taskRecord));
		}

		public bool AddGroup(GEDCOMGroupRecord groupRecord)
		{
			bool result = false;

			if (groupRecord != null)
			{
				GEDCOMPointer ptr = new GEDCOMPointer(this.Owner, this, "", "");
				ptr.SetNamedValue("_GROUP", groupRecord);
				this.fGroups.Add(ptr);
				result = true;
			}

			return result;
		}

		public void RemoveGroup(GEDCOMGroupRecord groupRecord)
		{
		    if (groupRecord == null) return;

			this.fGroups.Delete(this.IndexOfGroup(groupRecord));
		}

		public bool AddCommunication(GEDCOMCommunicationRecord commRecord)
		{
            bool result = false;

			if (commRecord != null)
			{
				GEDCOMPointer ptr = new GEDCOMPointer(this.Owner, this, "", "");
				ptr.SetNamedValue("_COMM", commRecord);
				this.fCommunications.Add(ptr);
				result = true;
			}

			return result;
		}

		public void RemoveCommunication(GEDCOMCommunicationRecord commRecord)
		{
		    if (commRecord == null) return;

			this.fCommunications.Delete(this.IndexOfCommunication(commRecord));
		}

		#endregion
	}
}
