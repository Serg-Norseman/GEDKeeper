using System;
using System.Runtime.InteropServices;

using GKSys;

namespace GedCom551
{
	public sealed class TGEDCOMIndividualOrdinance : TGEDCOMTagWithLists
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

		public TGEDCOMBaptismDateStatus BaptismDateStatus
		{
			get { return this.GetBaptismDateStatus(); }
			set { this.SetBaptismDateStatus(value); }
		}

		public TGEDCOMDateExact BaptismChangeDate
		{
			get { return this.GetChangeDate(); }
		}

		public TGEDCOMEndowmentDateStatus EndowmentDateStatus
		{
			get { return this.GetEndowmentDateStatus(); }
			set { this.SetEndowmentDateStatus(value); }
		}

		public TGEDCOMDateExact EndowmentChangeDate
		{
			get { return this.GetChangeDate(); }
		}

		public TGEDCOMPointer Family
		{
			get { return base.TagClass("FAMC", typeof(TGEDCOMPointer), TGEDCOMPointer.Create) as TGEDCOMPointer; }
		}

		public TGEDCOMChildSealingDateStatus ChildSealingDateStatus
		{
			get { return this.GetChildSealingDateStatus(); }
			set { this.SetChildSealingDateStatus(value); }
		}

		public TGEDCOMDateExact ChildSealingChangeDate
		{
			get { return this.GetChangeDate(); }
		}


		private TGEDCOMBaptismDateStatus GetBaptismDateStatus()
		{
			string S = base.GetTagStringValue("STAT").Trim().ToUpper();
			TGEDCOMBaptismDateStatus Result;
			if (S == "CHILD")
			{
				Result = TGEDCOMBaptismDateStatus.bdsChild;
			}
			else if (S == "COMPLETED")
			{
				Result = TGEDCOMBaptismDateStatus.bdsCompleted;
			}
			else if (S == "EXCLUDED")
			{
				Result = TGEDCOMBaptismDateStatus.bdsExcluded;
			}
			else if (S == "PRE-1970")
			{
				Result = TGEDCOMBaptismDateStatus.bdsPre1970;
			}
			else if (S == "STILLBORN")
			{
				Result = TGEDCOMBaptismDateStatus.bdsStillborn;
			}
			else if (S == "SUBMITTED")
			{
				Result = TGEDCOMBaptismDateStatus.bdsSubmitted;
			}
			else if (S == "UNCLEARED")
			{
				Result = TGEDCOMBaptismDateStatus.bdsUncleared;
			}
			else
			{
				Result = TGEDCOMBaptismDateStatus.bdsNone;
			}
			return Result;
		}

		private void SetBaptismDateStatus(TGEDCOMBaptismDateStatus Value)
		{
			string S;
			switch (Value)
			{
				case TGEDCOMBaptismDateStatus.bdsChild:
				{
					S = "CHILD";
					goto IL_66;
				}
				case TGEDCOMBaptismDateStatus.bdsCompleted:
				{
					S = "COMPLETED";
					goto IL_66;
				}
				case TGEDCOMBaptismDateStatus.bdsExcluded:
				{
					S = "EXCLUDED";
					goto IL_66;
				}
				case TGEDCOMBaptismDateStatus.bdsPre1970:
				{
					S = "PRE-1970";
					goto IL_66;
				}
				case TGEDCOMBaptismDateStatus.bdsStillborn:
				{
					S = "STILLBORN";
					goto IL_66;
				}
				case TGEDCOMBaptismDateStatus.bdsSubmitted:
				{
					S = "SUBMITTED";
					goto IL_66;
				}
				case TGEDCOMBaptismDateStatus.bdsUncleared:
				{
					S = "UNCLEARED";
					goto IL_66;
				}
			}
			S = "";
			IL_66:
			base.SetTagStringValue("STAT", S);
		}

		private TGEDCOMEndowmentDateStatus GetEndowmentDateStatus()
		{
			string S = base.GetTagStringValue("STAT").Trim().ToUpper();
			TGEDCOMEndowmentDateStatus Result;
			if (S == "CHILD")
			{
				Result = TGEDCOMEndowmentDateStatus.edsChild;
			}
			else if (S == "COMPLETED")
			{
				Result = TGEDCOMEndowmentDateStatus.edsCompleted;
			}
			else if (S == "EXCLUDED")
			{
				Result = TGEDCOMEndowmentDateStatus.edsExcluded;
			}
			else if (S == "INFANT")
			{
				Result = TGEDCOMEndowmentDateStatus.edsInfant;
			}
			else if (S == "PRE-1970")
			{
				Result = TGEDCOMEndowmentDateStatus.edsPre1970;
			}
			else if (S == "STILLBORN")
			{
				Result = TGEDCOMEndowmentDateStatus.edsStillborn;
			}
			else if (S == "SUBMITTED")
			{
				Result = TGEDCOMEndowmentDateStatus.edsSubmitted;
			}
			else if (S == "UNCLEARED")
			{
				Result = TGEDCOMEndowmentDateStatus.edsUncleared;
			}
			else
			{
				Result = TGEDCOMEndowmentDateStatus.edsNone;
			}
			return Result;
		}

		private void SetEndowmentDateStatus(TGEDCOMEndowmentDateStatus Value)
		{
			string S;
			switch (Value)
			{
				case TGEDCOMEndowmentDateStatus.edsChild:
				{
					S = "CHILD";
					goto IL_72;
				}
				case TGEDCOMEndowmentDateStatus.edsCompleted:
				{
					S = "COMPLETED";
					goto IL_72;
				}
				case TGEDCOMEndowmentDateStatus.edsExcluded:
				{
					S = "EXCLUDED";
					goto IL_72;
				}
				case TGEDCOMEndowmentDateStatus.edsInfant:
				{
					S = "INFANT";
					goto IL_72;
				}
				case TGEDCOMEndowmentDateStatus.edsPre1970:
				{
					S = "PRE-1970";
					goto IL_72;
				}
				case TGEDCOMEndowmentDateStatus.edsStillborn:
				{
					S = "STILLBORN";
					goto IL_72;
				}
				case TGEDCOMEndowmentDateStatus.edsSubmitted:
				{
					S = "SUBMITTED";
					goto IL_72;
				}
				case TGEDCOMEndowmentDateStatus.edsUncleared:
				{
					S = "UNCLEARED";
					goto IL_72;
				}
			}
			S = "";
			IL_72:
			base.SetTagStringValue("STAT", S);
		}

		private TGEDCOMChildSealingDateStatus GetChildSealingDateStatus()
		{
			string S = base.GetTagStringValue("STAT").Trim().ToUpper();
			TGEDCOMChildSealingDateStatus Result;
			if (S == "BIC")
			{
				Result = TGEDCOMChildSealingDateStatus.cdsBIC;
			}
			else
			{
				if (S == "EXCLUDED")
				{
					Result = TGEDCOMChildSealingDateStatus.cdsExcluded;
				}
				else
				{
					if (S == "PRE-1970")
					{
						Result = TGEDCOMChildSealingDateStatus.cdsPre1970;
					}
					else
					{
						if (S == "STILLBORN")
						{
							Result = TGEDCOMChildSealingDateStatus.cdsStillborn;
						}
						else
						{
							if (S == "SUBMITTED")
							{
								Result = TGEDCOMChildSealingDateStatus.cdsSubmitted;
							}
							else
							{
								if (S == "UNCLEARED")
								{
									Result = TGEDCOMChildSealingDateStatus.cdsUncleared;
								}
								else
								{
									Result = TGEDCOMChildSealingDateStatus.cdsNone;
								}
							}
						}
					}
				}
			}
			return Result;
		}

		private void SetChildSealingDateStatus(TGEDCOMChildSealingDateStatus Value)
		{
			string S;
			switch (Value)
			{
				case TGEDCOMChildSealingDateStatus.cdsBIC:
				{
					S = "BIC";
					goto IL_5A;
				}
				case TGEDCOMChildSealingDateStatus.cdsExcluded:
				{
					S = "EXCLUDED";
					goto IL_5A;
				}
				case TGEDCOMChildSealingDateStatus.cdsPre1970:
				{
					S = "PRE-1970";
					goto IL_5A;
				}
				case TGEDCOMChildSealingDateStatus.cdsStillborn:
				{
					S = "STILLBORN";
					goto IL_5A;
				}
				case TGEDCOMChildSealingDateStatus.cdsSubmitted:
				{
					S = "SUBMITTED";
					goto IL_5A;
				}
				case TGEDCOMChildSealingDateStatus.cdsUncleared:
				{
					S = "UNCLEARED";
					goto IL_5A;
				}
			}
			S = "";
			IL_5A:
			base.SetTagStringValue("STAT", S);
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
			base.SetLists(EnumSet.Create(new Enum[]
			{
				TGEDCOMSubList.stNotes, 
				TGEDCOMSubList.stSource
			}));
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
					if (ATag == "FAMC")
					{
						Result = base.AddTag(ATag, AValue, TGEDCOMPointer.Create);
					}
					else
					{
						Result = base.AddTag(ATag, AValue, ATagConstructor);
					}
				}
			}
			return Result;
		}

		public TGEDCOMIndividualOrdinance(TGEDCOMTree AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
