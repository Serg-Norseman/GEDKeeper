using GKSys;
using System;
using System.ComponentModel;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GedCom551
{
	public class TGEDCOMSpouseSealing : TGEDCOMTagWithLists
	{
		public enum TGEDCOMSpouseSealingDateStatus : byte
		{
			sdsNone,
			sdsCanceled,
			sdsCompleted,
			sdsExcluded,
			sdsDNS,
			sdsDNSCAN,
			sdsPre1970,
			sdsSubmitted,
			sdsUncleared
		}

		[Browsable(false)]
		public TGEDCOMDateValue Date
		{
			get { return this.GetDate(); }
		}

		[Browsable(false)]
		public string TempleCode
		{
			get
			{
				return this.GetStringTag(1);
			}
			set
			{
				this.SetStringTag(1, value);
			}
		}
		[Browsable(false)]
		public string Place
		{
			get { return this.GetStringTag(2); }
			set { this.SetStringTag(2, value); }
		}
		[Browsable(false)]
		public TGEDCOMSpouseSealing.TGEDCOMSpouseSealingDateStatus SpouseSealingDateStatus
		{
			get
			{
				return this.GetSpouseSealingDateStatus();
			}
			set
			{
				this.SetSpouseSealingDateStatus(value);
			}
		}

		[Browsable(false)]
		public TGEDCOMDateExact SpouseSealingChangeDate
		{
			get { return this.GetChangeDate(); }
		}

		internal TGEDCOMDateValue GetDate()
		{
			return base.TagClass("DATE", typeof(TGEDCOMDateValue)) as TGEDCOMDateValue;
		}

		internal string GetStringTag(int Index)
		{
			string Result = "";
			if (Index != 1)
			{
				if (Index == 2)
				{
					Result = base.GetTagStringValue("PLAC");
				}
			}
			else
			{
				Result = base.GetTagStringValue("TEMP");
			}
			return Result;
		}
		internal void SetStringTag(int Index, [In] string Value)
		{
			if (Index != 1)
			{
				if (Index == 2)
				{
					base.SetTagStringValue("PLAC", Value);
				}
			}
			else
			{
				base.SetTagStringValue("TEMP", Value);
			}
		}
		internal TGEDCOMSpouseSealing.TGEDCOMSpouseSealingDateStatus GetSpouseSealingDateStatus()
		{
			string S = base.GetTagStringValue("STAT").Trim().ToUpper();
			TGEDCOMSpouseSealing.TGEDCOMSpouseSealingDateStatus Result;
			if (BDSSystem.WStrCmp(S, "CANCELED") == 0)
			{
				Result = TGEDCOMSpouseSealing.TGEDCOMSpouseSealingDateStatus.sdsCanceled;
			}
			else
			{
				if (BDSSystem.WStrCmp(S, "COMPLETED") == 0)
				{
					Result = TGEDCOMSpouseSealing.TGEDCOMSpouseSealingDateStatus.sdsCompleted;
				}
				else
				{
					if (BDSSystem.WStrCmp(S, "EXCLUDED") == 0)
					{
						Result = TGEDCOMSpouseSealing.TGEDCOMSpouseSealingDateStatus.sdsExcluded;
					}
					else
					{
						if (BDSSystem.WStrCmp(S, "DNS") == 0)
						{
							Result = TGEDCOMSpouseSealing.TGEDCOMSpouseSealingDateStatus.sdsDNS;
						}
						else
						{
							if (BDSSystem.WStrCmp(S, "DNS/CAN") == 0)
							{
								Result = TGEDCOMSpouseSealing.TGEDCOMSpouseSealingDateStatus.sdsDNSCAN;
							}
							else
							{
								if (BDSSystem.WStrCmp(S, "PRE-1970") == 0)
								{
									Result = TGEDCOMSpouseSealing.TGEDCOMSpouseSealingDateStatus.sdsPre1970;
								}
								else
								{
									if (BDSSystem.WStrCmp(S, "SUBMITTED") == 0)
									{
										Result = TGEDCOMSpouseSealing.TGEDCOMSpouseSealingDateStatus.sdsSubmitted;
									}
									else
									{
										if (BDSSystem.WStrCmp(S, "UNCLEARED") == 0)
										{
											Result = TGEDCOMSpouseSealing.TGEDCOMSpouseSealingDateStatus.sdsUncleared;
										}
										else
										{
											Result = TGEDCOMSpouseSealing.TGEDCOMSpouseSealingDateStatus.sdsNone;
										}
									}
								}
							}
						}
					}
				}
			}
			return Result;
		}
		internal void SetSpouseSealingDateStatus(TGEDCOMSpouseSealing.TGEDCOMSpouseSealingDateStatus Value)
		{
			string S;
			switch (Value)
			{
				case TGEDCOMSpouseSealing.TGEDCOMSpouseSealingDateStatus.sdsCanceled:
				{
					S = "CANCELED";
					goto IL_72;
				}
				case TGEDCOMSpouseSealing.TGEDCOMSpouseSealingDateStatus.sdsCompleted:
				{
					S = "COMPLETED";
					goto IL_72;
				}
				case TGEDCOMSpouseSealing.TGEDCOMSpouseSealingDateStatus.sdsExcluded:
				{
					S = "EXCLUDED";
					goto IL_72;
				}
				case TGEDCOMSpouseSealing.TGEDCOMSpouseSealingDateStatus.sdsDNS:
				{
					S = "DNS";
					goto IL_72;
				}
				case TGEDCOMSpouseSealing.TGEDCOMSpouseSealingDateStatus.sdsDNSCAN:
				{
					S = "DNS/CAN";
					goto IL_72;
				}
				case TGEDCOMSpouseSealing.TGEDCOMSpouseSealingDateStatus.sdsPre1970:
				{
					S = "PRE-1970";
					goto IL_72;
				}
				case TGEDCOMSpouseSealing.TGEDCOMSpouseSealingDateStatus.sdsSubmitted:
				{
					S = "SUBMITTED";
					goto IL_72;
				}
				case TGEDCOMSpouseSealing.TGEDCOMSpouseSealingDateStatus.sdsUncleared:
				{
					S = "UNCLEARED";
					goto IL_72;
				}
			}
			S = "";
			IL_72:
			base.SetTagStringValue("STAT", S);
		}
		internal TGEDCOMDateExact GetChangeDate()
		{
			TGEDCOMTag StatTag = base.FindTag("STAT", 0);
			if (StatTag == null)
			{
				this.AddTag("STAT", "", null);
			}
			return StatTag.TagClass("CHAN", typeof(TGEDCOMDateExact)) as TGEDCOMDateExact;
		}
		protected internal override void CreateObj(TGEDCOMObject AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			base.SetLists(TEnumSet.Create(new Enum[]
			{
				TGEDCOMObject.TGEDCOMSubList.stNotes, 
				TGEDCOMObject.TGEDCOMSubList.stSource
			}));
		}

		public override TGEDCOMTag AddTag([In] string ATag, [In] string AValue, Type AClass)
		{
			TGEDCOMTag Result;
			if (BDSSystem.WStrCmp(ATag, "DATE") == 0)
			{
				Result = base.AddTag(ATag, AValue, typeof(TGEDCOMDateValue));
			}
			else
			{
				if (BDSSystem.WStrCmp(ATag, "STAT") == 0)
				{
					Result = base.AddTag(ATag, AValue, typeof(TGEDCOMDateStatus));
				}
				else
				{
					Result = base.AddTag(ATag, AValue, AClass);
				}
			}
			return Result;
		}

		public TGEDCOMSpouseSealing(TGEDCOMObject AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
