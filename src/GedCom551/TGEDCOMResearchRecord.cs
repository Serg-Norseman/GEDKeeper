using System;
using System.IO;
using System.Runtime.InteropServices;

using Ext.Utils;

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
			get { return GetPriorityVal(base.GetTagStringValue("_PRIORITY").Trim().ToLower()); }
			set { base.SetTagStringValue("_PRIORITY", GetPriorityStr(value)); }
		}

		public TResearchStatus Status
		{
			get { return GetStatusVal(base.GetTagStringValue("_STATUS").Trim().ToLower()); }
			set { base.SetTagStringValue("_STATUS", GetStatusStr(value)); }
		}

		public TGEDCOMDateExact StartDate
		{
			get { return base.TagClass("_STARTDATE", typeof(TGEDCOMDateExact), TGEDCOMDateExact.Create) as TGEDCOMDateExact; }
		}

		public TGEDCOMDateExact StopDate
		{
			get { return base.TagClass("_STOPDATE", typeof(TGEDCOMDateExact), TGEDCOMDateExact.Create) as TGEDCOMDateExact; }
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

		protected override void CreateObj(TGEDCOMTree AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			base.SetLists(EnumSet.Create(new Enum[] { TGEDCOMSubList.stNotes }));
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

		public override TGEDCOMTag AddTag([In] string ATag, [In] string AValue, TagConstructor ATagConstructor)
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
					Result = base.AddTag(ATag, AValue, TGEDCOMDateExact.Create);
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
								Result = base.AddTag(ATag, AValue, ATagConstructor);
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

		public TGEDCOMResearchRecord(TGEDCOMTree AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}

		public new static TGEDCOMCustomTag Create(TGEDCOMTree AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue)
		{
			return new TGEDCOMResearchRecord(AOwner, AParent, AName, AValue);
		}
	}
}
