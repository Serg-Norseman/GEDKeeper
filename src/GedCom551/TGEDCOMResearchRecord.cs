using System;
using System.IO;
using System.Runtime.InteropServices;

using GKCore.Sys;

namespace GedCom551
{
	public sealed class TGEDCOMResearchRecord : TGEDCOMRecord
	{
		private TGEDCOMListEx<TGEDCOMPointer> _Tasks;
		private TGEDCOMListEx<TGEDCOMPointer> _Communications;
		private TGEDCOMListEx<TGEDCOMPointer> _Groups;

		public string ResearchName
		{
			get { return base.GetTagStringValue("NAME"); }
			set { base.SetTagStringValue("NAME", value); }
		}

		public TResearchPriority Priority
		{
			get { return this.GetPriority(); }
			set { this.SetPriority(value); }
		}

		public TResearchStatus Status
		{
			get { return this.GetStatus(); }
			set { this.SetStatus(value); }
		}

		public TGEDCOMDateExact StartDate
		{
			get { return this.GetStartDate(); }
		}

		public TGEDCOMDateExact StopDate
		{
			get { return this.GetStopDate(); }
		}

		public int Percent
		{
			get { return base.GetTagIntegerValue("_PERCENT", 0); }
			set { base.SetTagIntegerValue("_PERCENT", value); }
		}

		public TGEDCOMListEx<TGEDCOMPointer> Tasks
		{
			get { return this._Tasks; }
		}

		public TGEDCOMListEx<TGEDCOMPointer> Communications
		{
			get { return this._Communications; }
		}

		public TGEDCOMListEx<TGEDCOMPointer> Groups
		{
			get { return this._Groups; }
		}

		private TResearchPriority GetPriority()
		{
			string S = base.GetTagStringValue("_PRIORITY").Trim().ToLower();
			return StrToPriority(S);
		}

		private void SetPriority([In] TResearchPriority Value)
		{
			base.SetTagStringValue("_PRIORITY", PriorityToStr(Value));
		}

		private TResearchStatus GetStatus()
		{
			string S = base.GetTagStringValue("_STATUS").Trim().ToLower();
			TResearchStatus Result;
			if (S == "inprogress")
			{
				Result = TResearchStatus.rsInProgress;
			}
			else
			{
				if (S == "onhold")
				{
					Result = TResearchStatus.rsOnHold;
				}
				else
				{
					if (S == "problems")
					{
						Result = TResearchStatus.rsProblems;
					}
					else
					{
						if (S == "completed")
						{
							Result = TResearchStatus.rsCompleted;
						}
						else
						{
							if (S == "withdrawn")
							{
								Result = TResearchStatus.rsWithdrawn;
							}
							else
							{
								Result = TResearchStatus.rsDefined;
							}
						}
					}
				}
			}
			return Result;
		}

		private void SetStatus([In] TResearchStatus Value)
		{
			string S = "";
			switch (Value)
			{
				case TResearchStatus.rsDefined:
				{
					S = "defined";
					break;
				}
				case TResearchStatus.rsInProgress:
				{
					S = "inprogress";
					break;
				}
				case TResearchStatus.rsOnHold:
				{
					S = "onhold";
					break;
				}
				case TResearchStatus.rsProblems:
				{
					S = "problems";
					break;
				}
				case TResearchStatus.rsCompleted:
				{
					S = "completed";
					break;
				}
				case TResearchStatus.rsWithdrawn:
				{
					S = "withdrawn";
					break;
				}
			}
			base.SetTagStringValue("_STATUS", S);
		}

		private TGEDCOMDateExact GetStartDate()
		{
			return base.TagClass("_STARTDATE", typeof(TGEDCOMDateExact)) as TGEDCOMDateExact;
		}

		private TGEDCOMDateExact GetStopDate()
		{
			return base.TagClass("_STOPDATE", typeof(TGEDCOMDateExact)) as TGEDCOMDateExact;
		}

		protected override void CreateObj(TGEDCOMObject AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			base.SetLists(TEnumSet.Create(new Enum[]
			{
				TGEDCOMSubList.stNotes
			}));
			this.FRecordType = TGEDCOMRecordType.rtResearch;
			this.FName = "_RESEARCH";

			this._Tasks = new TGEDCOMListEx<TGEDCOMPointer>(this);
			this._Communications = new TGEDCOMListEx<TGEDCOMPointer>(this);
			this._Groups = new TGEDCOMListEx<TGEDCOMPointer>(this);
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

		public override TGEDCOMTag AddTag([In] string ATag, [In] string AValue, Type AClass)
		{
			TGEDCOMTag Result;
			if (ATag == "NAME")
			{
				Result = base.AddTag(ATag, AValue, null);
			}
			else
			{
				if (ATag == "_STARTDATE" || ATag == "_STOPDATE")
				{
					Result = base.AddTag(ATag, AValue, typeof(TGEDCOMDateExact));
				}
				else
				{
					if (ATag == "_TASK")
					{
						Result = this.Tasks.Add(new TGEDCOMPointer(base.Owner, this, ATag, AValue));
					}
					else
					{
						if (ATag == "_COMM")
						{
							Result = this.Communications.Add(new TGEDCOMPointer(base.Owner, this, ATag, AValue));
						}
						else
						{
							if (ATag == "_GROUP")
							{
								Result = this.Groups.Add(new TGEDCOMPointer(base.Owner, this, ATag, AValue));
							}
							else
							{
								Result = base.AddTag(ATag, AValue, AClass);
							}
						}
					}
				}
			}
			return Result;
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

		public override void ResetOwner(TGEDCOMObject AOwner)
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

		public TGEDCOMResearchRecord(TGEDCOMObject AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
