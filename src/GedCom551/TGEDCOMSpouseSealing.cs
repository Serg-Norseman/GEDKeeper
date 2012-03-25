using System;
using System.Runtime.InteropServices;

using Ext.Utils;

namespace GedCom551
{
	public sealed class TGEDCOMSpouseSealing : TGEDCOMTagWithLists
	{
		public TGEDCOMDateValue Date
		{
			get { return base.TagClass("DATE", typeof(TGEDCOMDateValue), TGEDCOMDateValue.Create) as TGEDCOMDateValue; }
		}

		public string TempleCode
		{
			get { return base.GetTagStringValue("TEMP"); }
			set { base.SetTagStringValue("TEMP", value); }
		}

		public string Place
		{
			get { return base.GetTagStringValue("PLAC"); }
			set { base.SetTagStringValue("PLAC", value); }
		}

		public TGEDCOMSpouseSealingDateStatus SpouseSealingDateStatus
		{
			get { return GetSpouseSealingDateStatusVal(base.GetTagStringValue("STAT").Trim().ToUpper()); }
			set { base.SetTagStringValue("STAT", GetSpouseSealingDateStatusStr(value)); }
		}

		public TGEDCOMDateExact SpouseSealingChangeDate
		{
			get { return this.GetChangeDate(); }
		}

		private TGEDCOMDateExact GetChangeDate()
		{
			TGEDCOMTag StatTag = base.FindTag("STAT", 0);
			if (StatTag == null)
			{
				this.AddTag("STAT", "", null);
			}
			return StatTag.TagClass("CHAN", typeof(TGEDCOMDateExact), TGEDCOMDateExact.Create) as TGEDCOMDateExact;
		}

		protected override void CreateObj(TGEDCOMTree AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			base.SetLists(EnumSet.Create(new Enum[] { TGEDCOMSubList.stNotes, TGEDCOMSubList.stSource }));
		}

		public override TGEDCOMTag AddTag([In] string ATag, [In] string AValue, TagConstructor ATagConstructor)
		{
			TGEDCOMTag Result;

			if (ATag == "DATE")
			{
				Result = base.AddTag(ATag, AValue, TGEDCOMDateValue.Create);
			}
			else
			{
				if (ATag == "STAT")
				{
					Result = base.AddTag(ATag, AValue, TGEDCOMDateStatus.Create);
				}
				else
				{
					Result = base.AddTag(ATag, AValue, ATagConstructor);
				}
			}

			return Result;
		}

		public TGEDCOMSpouseSealing(TGEDCOMTree AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
