using System;

namespace GedCom551
{
	// TODO: возможны многочисленные нарушения стандарта, перепроверить вложенность тэгов
	public sealed class TGEDCOMIndividualOrdinance : TGEDCOMTagWithLists
	{
		public TGEDCOMDateValue Date
		{
			get { return base.TagClass("DATE", TGEDCOMDateValue.Create) as TGEDCOMDateValue; }
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
			get { return base.TagClass("FAMC", TGEDCOMPointer.Create) as TGEDCOMPointer; }
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


		private TGEDCOMDateExact GetChangeDate()
		{
			return this.DateStatus.TagClass("CHAN", TGEDCOMDateExact.Create) as TGEDCOMDateExact;
		}

		private TGEDCOMBaptismDateStatus GetBaptismDateStatus()
		{
			string S = base.GetTagStringValue("STAT").Trim().ToUpper();

            TGEDCOMBaptismDateStatus result;
			if (S == "CHILD")
			{
				result = TGEDCOMBaptismDateStatus.bdsChild;
			}
			else if (S == "COMPLETED")
			{
				result = TGEDCOMBaptismDateStatus.bdsCompleted;
			}
			else if (S == "EXCLUDED")
			{
				result = TGEDCOMBaptismDateStatus.bdsExcluded;
			}
			else if (S == "PRE-1970")
			{
				result = TGEDCOMBaptismDateStatus.bdsPre1970;
			}
			else if (S == "STILLBORN")
			{
				result = TGEDCOMBaptismDateStatus.bdsStillborn;
			}
			else if (S == "SUBMITTED")
			{
				result = TGEDCOMBaptismDateStatus.bdsSubmitted;
			}
			else if (S == "UNCLEARED")
			{
				result = TGEDCOMBaptismDateStatus.bdsUncleared;
			}
			else
			{
				result = TGEDCOMBaptismDateStatus.bdsNone;
			}
			return result;
		}

		private void SetBaptismDateStatus(TGEDCOMBaptismDateStatus value)
		{
			string S;
			switch (value)
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
			TGEDCOMEndowmentDateStatus result;
			if (S == "CHILD")
			{
				result = TGEDCOMEndowmentDateStatus.edsChild;
			}
			else if (S == "COMPLETED")
			{
				result = TGEDCOMEndowmentDateStatus.edsCompleted;
			}
			else if (S == "EXCLUDED")
			{
				result = TGEDCOMEndowmentDateStatus.edsExcluded;
			}
			else if (S == "INFANT")
			{
				result = TGEDCOMEndowmentDateStatus.edsInfant;
			}
			else if (S == "PRE-1970")
			{
				result = TGEDCOMEndowmentDateStatus.edsPre1970;
			}
			else if (S == "STILLBORN")
			{
				result = TGEDCOMEndowmentDateStatus.edsStillborn;
			}
			else if (S == "SUBMITTED")
			{
				result = TGEDCOMEndowmentDateStatus.edsSubmitted;
			}
			else if (S == "UNCLEARED")
			{
				result = TGEDCOMEndowmentDateStatus.edsUncleared;
			}
			else
			{
				result = TGEDCOMEndowmentDateStatus.edsNone;
			}
			return result;
		}

		private void SetEndowmentDateStatus(TGEDCOMEndowmentDateStatus value)
		{
			string S;
			switch (value)
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
			TGEDCOMChildSealingDateStatus result;

			if (S == "BIC")
			{
				result = TGEDCOMChildSealingDateStatus.cdsBIC;
			}
			else if (S == "EXCLUDED")
			{
				result = TGEDCOMChildSealingDateStatus.cdsExcluded;
			}
			else if (S == "PRE-1970")
			{
				result = TGEDCOMChildSealingDateStatus.cdsPre1970;
			}
			else if (S == "STILLBORN")
			{
				result = TGEDCOMChildSealingDateStatus.cdsStillborn;
			}
			else if (S == "SUBMITTED")
			{
				result = TGEDCOMChildSealingDateStatus.cdsSubmitted;
			}
			else if (S == "UNCLEARED")
			{
				result = TGEDCOMChildSealingDateStatus.cdsUncleared;
			}
			else
			{
				result = TGEDCOMChildSealingDateStatus.cdsNone;
			}

			return result;
		}

		private void SetChildSealingDateStatus(TGEDCOMChildSealingDateStatus value)
		{
			string S;
			switch (value)
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

		public TGEDCOMDateStatus DateStatus
		{
			get { return base.TagClass("STAT", TGEDCOMDateStatus.Create) as TGEDCOMDateStatus; }
		}

		public override TGEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
		{
			TGEDCOMTag result;

			if (tagName == "STAT")
			{
				result = base.AddTag(tagName, tagValue, TGEDCOMDateStatus.Create);
			}
			else if (tagName == "FAMC")
			{
				result = base.AddTag(tagName, tagValue, TGEDCOMPointer.Create);
			}
			else
			{
				// define "DATE" by default
				result = base.AddTag(tagName, tagValue, tagConstructor);
			}

			return result;
		}

		public TGEDCOMIndividualOrdinance(TGEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static TGEDCOMTag Create(TGEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
		{
			return new TGEDCOMIndividualOrdinance(owner, parent, tagName, tagValue);
		}
	}
}
