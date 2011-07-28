using GKSys;
using System;
using System.ComponentModel;
using System.IO;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GedCom551
{
	public class TGEDCOMResearchRecord : TGEDCOMRecord
	{
		internal TGEDCOMList FTasks;
		internal TGEDCOMList FCommunications;
		internal TGEDCOMList FGroups;

		public new string Name
		{
			get { return this.GetName(); }
			set { this.SetName(value); }
		}

		[Browsable(false)]
		public TResearchPriority Priority
		{
			get
			{
				return this.GetPriority();
			}
			set
			{
				this.SetPriority(value);
			}
		}

		[Browsable(false)]
		public TResearchStatus Status
		{
			get
			{
				return this.GetStatus();
			}
			set
			{
				this.SetStatus(value);
			}
		}

		[Browsable(false)]
		public TGEDCOMDateExact StartDate
		{
			get
			{
				return this.GetStartDate();
			}
		}

		[Browsable(false)]
		public TGEDCOMDateExact StopDate
		{
			get
			{
				return this.GetStopDate();
			}
		}

		[Browsable(false)]
		public int Percent
		{
			get
			{
				return this.GetPercent();
			}
			set
			{
				this.SetPercent(value);
			}
		}

		/*[Browsable(false)]
		public TGEDCOMPointer Tasks
		{
			get
			{
				return this.GetTask(Index);
			}
		}*/

		[Browsable(false)]
		public int TasksCount
		{
			get
			{
				return this.GetTasksCount();
			}
		}

		/*[Browsable(false)]
		public TGEDCOMPointer Communications
		{
			get
			{
				return this.GetCommunication(Index);
			}
		}*/

		[Browsable(false)]
		public int CommunicationsCount
		{
			get
			{
				return this.GetCommunicationsCount();
			}
		}

		/*[Browsable(false)]
		public TGEDCOMPointer Groups
		{
			get
			{
				return this.GetGroup(Index);
			}
		}*/

		[Browsable(false)]
		public int GroupsCount
		{
			get
			{
				return this.GetGroupsCount();
			}
		}

		internal string GetName()
		{
			return base.GetTagStringValue("NAME");
		}
		internal void SetName([In] string Value)
		{
			base.SetTagStringValue("NAME", Value);
		}
		internal TResearchPriority GetPriority()
		{
			string S = base.GetTagStringValue("_PRIORITY").Trim().ToLower();
			TResearchPriority Result;
			if (BDSSystem.WStrCmp(S, "low") == 0)
			{
				Result = TResearchPriority.rpLow;
			}
			else
			{
				if (BDSSystem.WStrCmp(S, "normal") == 0)
				{
					Result = TResearchPriority.rpNormal;
				}
				else
				{
					if (BDSSystem.WStrCmp(S, "high") == 0)
					{
						Result = TResearchPriority.rpHigh;
					}
					else
					{
						if (BDSSystem.WStrCmp(S, "top") == 0)
						{
							Result = TResearchPriority.rpTop;
						}
						else
						{
							Result = TResearchPriority.rpNone;
						}
					}
				}
			}
			return Result;
		}
		internal void SetPriority([In] TResearchPriority Value)
		{
			string S = "";
			switch (Value)
			{
				case TResearchPriority.rpNone:
				{
					S = "";
					break;
				}
				case TResearchPriority.rpLow:
				{
					S = "low";
					break;
				}
				case TResearchPriority.rpNormal:
				{
					S = "normal";
					break;
				}
				case TResearchPriority.rpHigh:
				{
					S = "high";
					break;
				}
				case TResearchPriority.rpTop:
				{
					S = "top";
					break;
				}
			}
			base.SetTagStringValue("_PRIORITY", S);
		}
		internal TGEDCOMPointer GetTask(int Index)
		{
			TGEDCOMPointer Result;
			if (this.FTasks == null || Index < 0 || Index >= this.FTasks.Count)
			{
				Result = null;
			}
			else
			{
				Result = (this.FTasks[Index] as TGEDCOMPointer);
			}
			return Result;
		}
		internal int GetTasksCount()
		{
			int Result;
			if (this.FTasks == null)
			{
				Result = 0;
			}
			else
			{
				Result = this.FTasks.Count;
			}
			return Result;
		}
		internal TGEDCOMPointer GetCommunication(int Index)
		{
			TGEDCOMPointer Result;
			if (this.FCommunications == null || Index < 0 || Index >= this.FCommunications.Count)
			{
				Result = null;
			}
			else
			{
				Result = (this.FCommunications[Index] as TGEDCOMPointer);
			}
			return Result;
		}
		internal int GetCommunicationsCount()
		{
			int Result;
			if (this.FCommunications == null)
			{
				Result = 0;
			}
			else
			{
				Result = this.FCommunications.Count;
			}
			return Result;
		}
		internal TResearchStatus GetStatus()
		{
			string S = base.GetTagStringValue("_STATUS").Trim().ToLower();
			TResearchStatus Result;
			if (BDSSystem.WStrCmp(S, "inprogress") == 0)
			{
				Result = TResearchStatus.rsInProgress;
			}
			else
			{
				if (BDSSystem.WStrCmp(S, "onhold") == 0)
				{
					Result = TResearchStatus.rsOnHold;
				}
				else
				{
					if (BDSSystem.WStrCmp(S, "problems") == 0)
					{
						Result = TResearchStatus.rsProblems;
					}
					else
					{
						if (BDSSystem.WStrCmp(S, "completed") == 0)
						{
							Result = TResearchStatus.rsCompleted;
						}
						else
						{
							if (BDSSystem.WStrCmp(S, "withdrawn") == 0)
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
		internal void SetStatus([In] TResearchStatus Value)
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

		internal TGEDCOMDateExact GetStartDate()
		{
			return base.TagClass("_STARTDATE", typeof(TGEDCOMDateExact)) as TGEDCOMDateExact;
		}

		internal TGEDCOMDateExact GetStopDate()
		{
			return base.TagClass("_STOPDATE", typeof(TGEDCOMDateExact)) as TGEDCOMDateExact;
		}

		internal int GetPercent()
		{
			return base.GetTagIntegerValue("_PERCENT", 0);
		}
		internal void SetPercent([In] int Value)
		{
			base.SetTagIntegerValue("_PERCENT", Value);
		}

		public TGEDCOMPointer GetGroup(int Index)
		{
			TGEDCOMPointer Result;
			if (this.FGroups == null)
			{
				Result = null;
			}
			else
			{
				Result = (this.FGroups[Index] as TGEDCOMPointer);
			}
			return Result;
		}

		internal int GetGroupsCount()
		{
			int Result;
			if (this.FGroups == null)
			{
				Result = 0;
			}
			else
			{
				Result = this.FGroups.Count;
			}
			return Result;
		}
		protected internal override void CreateObj(TGEDCOMObject AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			base.SetLists(TEnumSet.Create(new Enum[]
			{
				TGEDCOMObject.TGEDCOMSubList.stNotes
			}));
			this.FRecordType = TGEDCOMRecord.TGEDCOMRecordType.rtResearch;
			this.FName = "_RESEARCH";
			this.FTasks = null;
			this.FCommunications = null;
			this.FGroups = null;
		}

		public override void Dispose()
		{
			if (!this.Disposed_)
			{
				if (this.FTasks != null)
				{
					this.FTasks.Free();
				}
				if (this.FCommunications != null)
				{
					this.FCommunications.Free();
				}
				if (this.FGroups != null)
				{
					this.FGroups.Free();
				}
				base.Dispose();
				this.Disposed_ = true;
			}
		}

		public override TGEDCOMTag AddTag([In] string ATag, [In] string AValue, Type AClass)
		{
			TGEDCOMTag Result;
			if (BDSSystem.WStrCmp(ATag, "NAME") == 0)
			{
				Result = base.AddTag(ATag, AValue, null);
			}
			else
			{
				if (BDSSystem.WStrCmp(ATag, "_STARTDATE") == 0 || BDSSystem.WStrCmp(ATag, "_STOPDATE") == 0)
				{
					Result = base.AddTag(ATag, AValue, typeof(TGEDCOMDateExact));
				}
				else
				{
					if (BDSSystem.WStrCmp(ATag, "_TASK") == 0)
					{
						Result = this.AddTask(new TGEDCOMPointer(base.Owner, this, ATag, AValue));
					}
					else
					{
						if (BDSSystem.WStrCmp(ATag, "_COMM") == 0)
						{
							Result = this.AddCommunication(new TGEDCOMPointer(base.Owner, this, ATag, AValue));
						}
						else
						{
							if (BDSSystem.WStrCmp(ATag, "_GROUP") == 0)
							{
								Result = this.AddGroup(new TGEDCOMPointer(base.Owner, this, ATag, AValue));
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
			if (this.FTasks != null)
			{
				this.FTasks.Clear();
			}
			if (this.FCommunications != null)
			{
				this.FCommunications.Clear();
			}
			if (this.FGroups != null)
			{
				this.FGroups.Clear();
			}
		}
		public override bool IsEmpty()
		{
			return base.IsEmpty() && this.GetTasksCount() == 0 && this.GetCommunicationsCount() == 0 && this.GetGroupsCount() == 0;
		}
		public override void ReplaceXRefs(TXRefReplaceMap aMap)
		{
			base.ReplaceXRefs(aMap);
			if (this.FTasks != null)
			{
				this.FTasks.ReplaceXRefs(aMap);
			}
			if (this.FCommunications != null)
			{
				this.FCommunications.ReplaceXRefs(aMap);
			}
			if (this.FGroups != null)
			{
				this.FGroups.ReplaceXRefs(aMap);
			}
		}
		public override void ResetOwner(TGEDCOMObject AOwner)
		{
			base.ResetOwner(AOwner);
			if (this.FTasks != null)
			{
				this.FTasks.ResetOwner(AOwner);
			}
			if (this.FCommunications != null)
			{
				this.FCommunications.ResetOwner(AOwner);
			}
			if (this.FGroups != null)
			{
				this.FGroups.ResetOwner(AOwner);
			}
		}
		public override void SaveToStream(StreamWriter AStream)
		{
			base.SaveToStream(AStream);
			if (this.FTasks != null)
			{
				this.FTasks.SaveToStream(AStream);
			}
			if (this.FCommunications != null)
			{
				this.FCommunications.SaveToStream(AStream);
			}
			if (this.FGroups != null)
			{
				this.FGroups.SaveToStream(AStream);
			}
		}
		public TGEDCOMPointer AddTask(TGEDCOMPointer Value)
		{
			if (this.FTasks == null)
			{
				this.FTasks = new TGEDCOMList(this);
			}
			if (this.FTasks != null)
			{
				this.FTasks.Add(Value);
			}
			return Value;
		}
		public int IndexOfTask(TGEDCOMTaskRecord aTask)
		{
			int Result = -1;
			int arg_11_0 = 0;
			int num = this.FTasks.Count - 1;
			int i = arg_11_0;
			if (num >= i)
			{
				num++;
				while (BDSSystem.WStrCmp((this.FTasks[i] as TGEDCOMPointer).XRef, aTask.XRef) != 0)
				{
					i++;
					if (i == num)
					{
						return Result;
					}
				}
				Result = i;
			}
			return Result;
		}
		public void DeleteTask(int aIndex)
		{
			if (this.FTasks != null)
			{
				this.FTasks.Delete(aIndex);
			}
		}
		public TGEDCOMPointer AddCommunication(TGEDCOMPointer Value)
		{
			if (this.FCommunications == null)
			{
				this.FCommunications = new TGEDCOMList(this);
			}
			if (this.FCommunications != null)
			{
				this.FCommunications.Add(Value);
			}
			return Value;
		}
		public int IndexOfCommunication(TGEDCOMCommunicationRecord aCommunication)
		{
			int Result = -1;
			int arg_11_0 = 0;
			int num = this.FCommunications.Count - 1;
			int i = arg_11_0;
			if (num >= i)
			{
				num++;
				while (BDSSystem.WStrCmp((this.FCommunications[i] as TGEDCOMPointer).XRef, aCommunication.XRef) != 0)
				{
					i++;
					if (i == num)
					{
						return Result;
					}
				}
				Result = i;
			}
			return Result;
		}
		public void DeleteCommunication(int aIndex)
		{
			if (this.FCommunications != null)
			{
				this.FCommunications.Delete(aIndex);
			}
		}
		public TGEDCOMPointer AddGroup(TGEDCOMPointer Value)
		{
			if (this.FGroups == null)
			{
				this.FGroups = new TGEDCOMList(this);
			}
			if (Value != null)
			{
				this.FGroups.Add(Value);
			}
			return Value;
		}
		public int IndexOfGroup(TGEDCOMGroupRecord aGroup)
		{
			int Result = -1;
			if (this.FGroups != null)
			{
				int arg_19_0 = 0;
				int num = this.FGroups.Count - 1;
				int i = arg_19_0;
				if (num >= i)
				{
					num++;
					while (BDSSystem.WStrCmp((this.FGroups[i] as TGEDCOMPointer).XRef, aGroup.XRef) != 0)
					{
						i++;
						if (i == num)
						{
							return Result;
						}
					}
					Result = i;
				}
			}
			return Result;
		}
		public void DeleteGroup(int aIndex)
		{
			if (this.FGroups != null)
			{
				this.FGroups.Delete(aIndex);
			}
		}

		public TGEDCOMResearchRecord(TGEDCOMObject AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
