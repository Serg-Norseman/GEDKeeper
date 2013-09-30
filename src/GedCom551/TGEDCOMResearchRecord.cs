using System;
using System.IO;

namespace GedCom551
{
	public sealed class TGEDCOMResearchRecord : TGEDCOMRecord
	{
		private GEDCOMList<TGEDCOMPointer> _Tasks;
		private GEDCOMList<TGEDCOMPointer> _Communications;
		private GEDCOMList<TGEDCOMPointer> _Groups;

		public string ResearchName
		{
			get { return base.GetTagStringValue("NAME"); }
			set { base.SetTagStringValue("NAME", value); }
		}

		public TResearchPriority Priority
		{
			get { return GEDCOMUtils.GetPriorityVal(base.GetTagStringValue("_PRIORITY").Trim().ToLower()); }
			set { base.SetTagStringValue("_PRIORITY", GEDCOMUtils.GetPriorityStr(value)); }
		}

		public TResearchStatus Status
		{
			get { return GEDCOMUtils.GetStatusVal(base.GetTagStringValue("_STATUS").Trim().ToLower()); }
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
			get { return this._Tasks; }
		}

		public GEDCOMList<TGEDCOMPointer> Communications
		{
			get { return this._Communications; }
		}

		public GEDCOMList<TGEDCOMPointer> Groups
		{
			get { return this._Groups; }
		}

		protected override void CreateObj(TGEDCOMTree owner, TGEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.FRecordType = TGEDCOMRecordType.rtResearch;
			this.FName = "_RESEARCH";

			this._Tasks = new GEDCOMList<TGEDCOMPointer>(this);
			this._Communications = new GEDCOMList<TGEDCOMPointer>(this);
			this._Groups = new GEDCOMList<TGEDCOMPointer>(this);
		}

		public override void Dispose()
		{
			if (!this.Disposed_)
			{
				this._Tasks.Dispose();
				this._Communications.Dispose();
				this._Groups.Dispose();

				base.Dispose();
				this.Disposed_ = true;
			}
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
				result = this.Tasks.Add(new TGEDCOMPointer(base.Owner, this, tagName, tagValue));
			}
			else if (tagName == "_COMM")
			{
				result = this.Communications.Add(new TGEDCOMPointer(base.Owner, this, tagName, tagValue));
			}
			else if (tagName == "_GROUP")
			{
				result = this.Groups.Add(new TGEDCOMPointer(base.Owner, this, tagName, tagValue));
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

			this._Tasks.Clear();
			this._Communications.Clear();
			this._Groups.Clear();
		}

		public override bool IsEmpty()
		{
			return base.IsEmpty() && this._Tasks.Count == 0 && this._Communications.Count == 0 && this._Groups.Count == 0;
		}

		public override void ReplaceXRefs(TXRefReplaceMap aMap)
		{
			base.ReplaceXRefs(aMap);

			this._Tasks.ReplaceXRefs(aMap);
			this._Communications.ReplaceXRefs(aMap);
			this._Groups.ReplaceXRefs(aMap);
		}

		public override void ResetOwner(TGEDCOMTree AOwner)
		{
			base.ResetOwner(AOwner);

			this._Tasks.ResetOwner(AOwner);
			this._Communications.ResetOwner(AOwner);
			this._Groups.ResetOwner(AOwner);
		}

		public override void SaveToStream(StreamWriter AStream)
		{
			base.SaveToStream(AStream);

			this._Tasks.SaveToStream(AStream);
			this._Communications.SaveToStream(AStream);
			this._Groups.SaveToStream(AStream);
		}

		public int IndexOfTask(TGEDCOMTaskRecord aTask)
		{
			int Result = -1;
			int num = this._Tasks.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				if (this._Tasks[i].XRef == aTask.XRef)
				{
					Result = i;
					break;
				}
			}
			return Result;
		}

		public int IndexOfCommunication(TGEDCOMCommunicationRecord aCommunication)
		{
			int Result = -1;
			int num = this._Communications.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				if (this._Communications[i].XRef == aCommunication.XRef)
				{
					Result = i;
					break;
				}
			}
			return Result;
		}

		public int IndexOfGroup(TGEDCOMGroupRecord aGroup)
		{
			int Result = -1;
			int num = this._Groups.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				if (this._Groups[i].XRef == aGroup.XRef)
				{
					Result = i;
					break;
				}
			}
			return Result;
		}

		public TGEDCOMResearchRecord(TGEDCOMTree owner, TGEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static TGEDCOMTag Create(TGEDCOMTree owner, TGEDCOMObject parent, string tagName, string tagValue)
		{
			return new TGEDCOMResearchRecord(owner, parent, tagName, tagValue);
		}

        #region Auxiliary

        public bool aux_AddTask(TGEDCOMTaskRecord aTask)
		{
			bool Result = false;
			if (aTask != null)
			{
				TGEDCOMPointer ptr = new TGEDCOMPointer(this.Owner, this, "", "");
				ptr.SetNamedValue("_TASK", aTask);
				this.Tasks.Add(ptr);
				Result = true;
			}
			return Result;
		}

		public void aux_RemoveTask(TGEDCOMTaskRecord aTask)
		{
			this.Tasks.Delete(this.IndexOfTask(aTask));
		}

		public bool aux_AddGroup(TGEDCOMGroupRecord aGroup)
		{
			bool Result = false;
			if (aGroup != null)
			{
				TGEDCOMPointer ptr = new TGEDCOMPointer(this.Owner, this, "", "");
				ptr.SetNamedValue("_GROUP", aGroup);
				this.Groups.Add(ptr);
				Result = true;
			}
			return Result;
		}

		public void aux_RemoveGroup(TGEDCOMGroupRecord aGroup)
		{
			this.Groups.Delete(this.IndexOfGroup(aGroup));
		}

		public bool aux_AddCommunication(TGEDCOMCommunicationRecord aComm)
		{
			bool Result = false;
			if (aComm != null)
			{
				TGEDCOMPointer ptr = new TGEDCOMPointer(this.Owner, this, "", "");
				ptr.SetNamedValue("_COMM", aComm);
				this.Communications.Add(ptr);
				Result = true;
			}
			return Result;
		}

		public void aux_RemoveCommunication(TGEDCOMCommunicationRecord aComm)
		{
			this.Communications.Delete(this.IndexOfCommunication(aComm));
		}

		#endregion
	}
}
