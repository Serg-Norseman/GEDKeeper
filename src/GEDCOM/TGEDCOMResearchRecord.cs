using System;
using System.IO;

namespace GedCom551
{
	public sealed class TGEDCOMResearchRecord : TGEDCOMRecord
	{
		private GEDCOMList<TGEDCOMPointer> fTasks;
		private GEDCOMList<TGEDCOMPointer> fCommunications;
		private GEDCOMList<TGEDCOMPointer> fGroups;

		public string ResearchName
		{
			get { return base.GetTagStringValue("NAME"); }
			set { base.SetTagStringValue("NAME", value); }
		}

		public TResearchPriority Priority
		{
			get { return GEDCOMUtils.GetPriorityVal(base.GetTagStringValue("_PRIORITY")); }
			set { base.SetTagStringValue("_PRIORITY", GEDCOMUtils.GetPriorityStr(value)); }
		}

		public TResearchStatus Status
		{
			get { return GEDCOMUtils.GetStatusVal(base.GetTagStringValue("_STATUS")); }
			set { base.SetTagStringValue("_STATUS", GEDCOMUtils.GetStatusStr(value)); }
		}

		public TGEDCOMDateExact StartDate
		{
			get { return base.TagClass("_STARTDATE", TGEDCOMDateExact.Create) as TGEDCOMDateExact; }
		}

		public TGEDCOMDateExact StopDate
		{
			get { return base.TagClass("_STOPDATE", TGEDCOMDateExact.Create) as TGEDCOMDateExact; }
		}

		public int Percent
		{
			get { return base.GetTagIntegerValue("_PERCENT", 0); }
			set { base.SetTagIntegerValue("_PERCENT", value); }
		}

		public GEDCOMList<TGEDCOMPointer> Tasks
		{
			get { return this.fTasks; }
		}

		public GEDCOMList<TGEDCOMPointer> Communications
		{
			get { return this.fCommunications; }
		}

		public GEDCOMList<TGEDCOMPointer> Groups
		{
			get { return this.fGroups; }
		}

		protected override void CreateObj(TGEDCOMTree owner, GEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.fRecordType = TGEDCOMRecordType.rtResearch;
			this.fName = "_RESEARCH";

			this.fTasks = new GEDCOMList<TGEDCOMPointer>(this);
			this.fCommunications = new GEDCOMList<TGEDCOMPointer>(this);
			this.fGroups = new GEDCOMList<TGEDCOMPointer>(this);
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

		public override TGEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
		{
			TGEDCOMTag result;

			if (tagName == "NAME")
			{
				result = base.AddTag(tagName, tagValue, null);
			}
			else if (tagName == "_STARTDATE" || tagName == "_STOPDATE")
			{
				result = base.AddTag(tagName, tagValue, TGEDCOMDateExact.Create);
			}
			else if (tagName == "_TASK")
			{
				result = this.fTasks.Add(new TGEDCOMPointer(base.Owner, this, tagName, tagValue));
			}
			else if (tagName == "_COMM")
			{
				result = this.fCommunications.Add(new TGEDCOMPointer(base.Owner, this, tagName, tagValue));
			}
			else if (tagName == "_GROUP")
			{
				result = this.fGroups.Add(new TGEDCOMPointer(base.Owner, this, tagName, tagValue));
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

		public override void ResetOwner(TGEDCOMTree newOwner)
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

		public int IndexOfTask(TGEDCOMTaskRecord aTask)
		{
			int result = -1;
            if (aTask == null) return result;

			int num = this.fTasks.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				if (this.fTasks[i].XRef == aTask.XRef)
				{
					result = i;
					break;
				}
			}
			return result;
		}

		public int IndexOfCommunication(TGEDCOMCommunicationRecord aCommunication)
		{
			int result = -1;
            if (aCommunication == null) return result;

			int num = this.fCommunications.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				if (this.fCommunications[i].XRef == aCommunication.XRef)
				{
					result = i;
					break;
				}
			}
			return result;
		}

		public int IndexOfGroup(TGEDCOMGroupRecord aGroup)
		{
			int result = -1;
            if (aGroup == null) return result;

			int num = this.fGroups.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				if (this.fGroups[i].XRef == aGroup.XRef)
				{
					result = i;
					break;
				}
			}
			return result;
		}

		public TGEDCOMResearchRecord(TGEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static TGEDCOMTag Create(TGEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
		{
			return new TGEDCOMResearchRecord(owner, parent, tagName, tagValue);
		}

        #region Auxiliary

        public bool aux_AddTask(TGEDCOMTaskRecord taskRecord)
		{
			bool result = false;

			if (taskRecord != null)
			{
				TGEDCOMPointer ptr = new TGEDCOMPointer(this.Owner, this, "", "");
				ptr.SetNamedValue("_TASK", taskRecord);
				this.Tasks.Add(ptr);
				result = true;
			}

			return result;
		}

		public void aux_RemoveTask(TGEDCOMTaskRecord taskRecord)
		{
            if (taskRecord == null) return;

			this.Tasks.Delete(this.IndexOfTask(taskRecord));
		}

		public bool aux_AddGroup(TGEDCOMGroupRecord groupRecord)
		{
			bool result = false;

			if (groupRecord != null)
			{
				TGEDCOMPointer ptr = new TGEDCOMPointer(this.Owner, this, "", "");
				ptr.SetNamedValue("_GROUP", groupRecord);
				this.Groups.Add(ptr);
				result = true;
			}

			return result;
		}

		public void aux_RemoveGroup(TGEDCOMGroupRecord groupRecord)
		{
		    if (groupRecord == null) return;

			this.Groups.Delete(this.IndexOfGroup(groupRecord));
		}

		public bool aux_AddCommunication(TGEDCOMCommunicationRecord commRecord)
		{
            bool result = false;

			if (commRecord != null)
			{
				TGEDCOMPointer ptr = new TGEDCOMPointer(this.Owner, this, "", "");
				ptr.SetNamedValue("_COMM", commRecord);
				this.Communications.Add(ptr);
				result = true;
			}

			return result;
		}

		public void aux_RemoveCommunication(TGEDCOMCommunicationRecord commRecord)
		{
		    if (commRecord == null) return;

			this.Communications.Delete(this.IndexOfCommunication(commRecord));
		}

		#endregion
	}
}
