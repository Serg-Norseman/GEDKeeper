using GKSys;
using System;
using System.ComponentModel;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GedCom551
{
	public class TGEDCOMTaskRecord : TGEDCOMRecord
	{
		[Browsable(false)]
		public string Goal
		{
			get
			{
				return this.GetGoal();
			}
			set
			{
				this.SetGoal(value);
			}
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

		internal TGEDCOMDateExact GetStartDate()
		{
			return base.TagClass("_STARTDATE", typeof(TGEDCOMDateExact)) as TGEDCOMDateExact;
		}

		internal TGEDCOMDateExact GetStopDate()
		{
			return base.TagClass("_STOPDATE", typeof(TGEDCOMDateExact)) as TGEDCOMDateExact;
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

		internal string GetGoal()
		{
			return base.GetTagStringValue("_GOAL");
		}

		internal void SetGoal([In] string Value)
		{
			base.SetTagStringValue("_GOAL", Value);
		}

		protected internal override void CreateObj(TGEDCOMObject AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			base.SetLists(TEnumSet.Create(new Enum[]
			{
				TGEDCOMObject.TGEDCOMSubList.stNotes
			}));
			this.FRecordType = TGEDCOMRecord.TGEDCOMRecordType.rtTask;
			this.FName = "_TASK";
		}

		public override TGEDCOMTag AddTag([In] string ATag, [In] string AValue, Type AClass)
		{
			TGEDCOMTag Result;
			if (BDSSystem.WStrCmp(ATag, "_STARTDATE") == 0 || BDSSystem.WStrCmp(ATag, "_STOPDATE") == 0)
			{
				Result = base.AddTag(ATag, AValue, typeof(TGEDCOMDateExact));
			}
			else
			{
				Result = base.AddTag(ATag, AValue, AClass);
			}
			return Result;
		}

		public TGEDCOMTaskRecord(TGEDCOMObject AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
