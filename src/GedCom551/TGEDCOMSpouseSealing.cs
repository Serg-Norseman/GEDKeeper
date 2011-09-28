using System;
using System.Runtime.InteropServices;

using GKCore.Sys;

namespace GedCom551
{
	public sealed class TGEDCOMSpouseSealing : TGEDCOMTagWithLists
	{
		public TGEDCOMDateValue Date
		{
			get { return base.TagClass("DATE", typeof(TGEDCOMDateValue)) as TGEDCOMDateValue; }
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
			get { return this.GetSpouseSealingDateStatus(); }
			set { this.SetSpouseSealingDateStatus(value); }
		}

		public TGEDCOMDateExact SpouseSealingChangeDate
		{
			get { return this.GetChangeDate(); }
		}

		private TGEDCOMSpouseSealingDateStatus GetSpouseSealingDateStatus()
		{
			string S = base.GetTagStringValue("STAT").Trim().ToUpper();
			TGEDCOMSpouseSealingDateStatus Result;
			if (S == "CANCELED")
			{
				Result = TGEDCOMSpouseSealingDateStatus.sdsCanceled;
			}
			else
			{
				if (S == "COMPLETED")
				{
					Result = TGEDCOMSpouseSealingDateStatus.sdsCompleted;
				}
				else
				{
					if (S == "EXCLUDED")
					{
						Result = TGEDCOMSpouseSealingDateStatus.sdsExcluded;
					}
					else
					{
						if (S == "DNS")
						{
							Result = TGEDCOMSpouseSealingDateStatus.sdsDNS;
						}
						else
						{
							if (S == "DNS/CAN")
							{
								Result = TGEDCOMSpouseSealingDateStatus.sdsDNSCAN;
							}
							else
							{
								if (S == "PRE-1970")
								{
									Result = TGEDCOMSpouseSealingDateStatus.sdsPre1970;
								}
								else
								{
									if (S == "SUBMITTED")
									{
										Result = TGEDCOMSpouseSealingDateStatus.sdsSubmitted;
									}
									else
									{
										if (S == "UNCLEARED")
										{
											Result = TGEDCOMSpouseSealingDateStatus.sdsUncleared;
										}
										else
										{
											Result = TGEDCOMSpouseSealingDateStatus.sdsNone;
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

		private void SetSpouseSealingDateStatus(TGEDCOMSpouseSealingDateStatus Value)
		{
			string S;
			switch (Value)
			{
				case TGEDCOMSpouseSealingDateStatus.sdsCanceled:
				{
					S = "CANCELED";
					goto IL_72;
				}
				case TGEDCOMSpouseSealingDateStatus.sdsCompleted:
				{
					S = "COMPLETED";
					goto IL_72;
				}
				case TGEDCOMSpouseSealingDateStatus.sdsExcluded:
				{
					S = "EXCLUDED";
					goto IL_72;
				}
				case TGEDCOMSpouseSealingDateStatus.sdsDNS:
				{
					S = "DNS";
					goto IL_72;
				}
				case TGEDCOMSpouseSealingDateStatus.sdsDNSCAN:
				{
					S = "DNS/CAN";
					goto IL_72;
				}
				case TGEDCOMSpouseSealingDateStatus.sdsPre1970:
				{
					S = "PRE-1970";
					goto IL_72;
				}
				case TGEDCOMSpouseSealingDateStatus.sdsSubmitted:
				{
					S = "SUBMITTED";
					goto IL_72;
				}
				case TGEDCOMSpouseSealingDateStatus.sdsUncleared:
				{
					S = "UNCLEARED";
					goto IL_72;
				}
			}
			S = "";
			IL_72:
			base.SetTagStringValue("STAT", S);
		}

		private TGEDCOMDateExact GetChangeDate()
		{
			TGEDCOMTag StatTag = base.FindTag("STAT", 0);
			if (StatTag == null)
			{
				this.AddTag("STAT", "", null);
			}
			return StatTag.TagClass("CHAN", typeof(TGEDCOMDateExact)) as TGEDCOMDateExact;
		}

		protected override void CreateObj(TGEDCOMObject AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			base.SetLists(TEnumSet.Create(new Enum[]
			{
				TGEDCOMSubList.stNotes, 
				TGEDCOMSubList.stSource
			}));
		}

		public override TGEDCOMTag AddTag([In] string ATag, [In] string AValue, Type AClass)
		{
			TGEDCOMTag Result;

			if (ATag == "DATE")
			{
				Result = base.AddTag(ATag, AValue, typeof(TGEDCOMDateValue));
			}
			else
			{
				if (ATag == "STAT")
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
