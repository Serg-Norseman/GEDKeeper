using System;
using System.Runtime.InteropServices;

using Ext.Utils;

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

		protected override void CreateObj(TGEDCOMTree AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			base.SetLists(EnumSet.Create(new Enum[] { TGEDCOMSubList.stNotes }));
			this.FRecordType = TGEDCOMRecordType.rtTask;
			this.FName = "_TASK";
		}

		public override TGEDCOMTag AddTag([In] string ATag, [In] string AValue, TagConstructor ATagConstructor)
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

		public TGEDCOMTaskRecord(TGEDCOMTree AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}

		public new static TGEDCOMCustomTag Create(TGEDCOMTree AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue)
		{
			return new TGEDCOMTaskRecord(AOwner, AParent, AName, AValue);
		}
	}
}
