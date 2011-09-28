using System;
using System.Runtime.InteropServices;

using GKCore.Sys;

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
			get { return this.GetPriority(); }
			set { this.SetPriority(value); }
		}

		public TGEDCOMDateExact StartDate
		{
			get { return this.GetStartDate(); }
		}

		private TGEDCOMDateExact GetStartDate()
		{
			return base.TagClass("_STARTDATE", typeof(TGEDCOMDateExact)) as TGEDCOMDateExact;
		}

		public TGEDCOMDateExact StopDate
		{
			get { return this.GetStopDate(); }
		}

		private TGEDCOMDateExact GetStopDate()
		{
			return base.TagClass("_STOPDATE", typeof(TGEDCOMDateExact)) as TGEDCOMDateExact;
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

		protected override void CreateObj(TGEDCOMObject AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			base.SetLists(TEnumSet.Create(new Enum[]
			{
				TGEDCOMSubList.stNotes
			}));
			this.FRecordType = TGEDCOMRecordType.rtTask;
			this.FName = "_TASK";
		}

		public override TGEDCOMTag AddTag([In] string ATag, [In] string AValue, Type AClass)
		{
			TGEDCOMTag Result;
			if (ATag == "_STARTDATE" || ATag == "_STOPDATE")
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
