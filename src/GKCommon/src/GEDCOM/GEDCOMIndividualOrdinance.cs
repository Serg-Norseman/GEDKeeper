using GKCommon.GEDCOM.Enums;

namespace GKCommon.GEDCOM
{
	// TODO: возможны многочисленные нарушения стандарта, перепроверить вложенность тэгов
	public sealed class GEDCOMIndividualOrdinance : GEDCOMTagWithLists
	{
		public GEDCOMDateValue Date
		{
			get { return base.TagClass("DATE", GEDCOMDateValue.Create) as GEDCOMDateValue; }
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

		public GEDCOMBaptismDateStatus BaptismDateStatus
		{
			get { return this.GetBaptismDateStatus(); }
			set { this.SetBaptismDateStatus(value); }
		}

		public GEDCOMDateExact BaptismChangeDate
		{
			get { return this.GetChangeDate(); }
		}

		public GEDCOMEndowmentDateStatus EndowmentDateStatus
		{
			get { return this.GetEndowmentDateStatus(); }
			set { this.SetEndowmentDateStatus(value); }
		}

		public GEDCOMDateExact EndowmentChangeDate
		{
			get { return this.GetChangeDate(); }
		}

		public GEDCOMPointer Family
		{
			get { return base.TagClass("FAMC", GEDCOMPointer.Create) as GEDCOMPointer; }
		}

		public GEDCOMChildSealingDateStatus ChildSealingDateStatus
		{
			get { return this.GetChildSealingDateStatus(); }
			set { this.SetChildSealingDateStatus(value); }
		}

		public GEDCOMDateExact ChildSealingChangeDate
		{
			get { return this.GetChangeDate(); }
		}


		private GEDCOMDateExact GetChangeDate()
		{
			return this.DateStatus.TagClass("CHAN", GEDCOMDateExact.Create) as GEDCOMDateExact;
		}

		private GEDCOMBaptismDateStatus GetBaptismDateStatus()
		{
			string S = base.GetTagStringValue("STAT").Trim().ToUpper();

            GEDCOMBaptismDateStatus result;
			if (S == "CHILD")
			{
				result = GEDCOMBaptismDateStatus.bdsChild;
			}
			else if (S == "COMPLETED")
			{
				result = GEDCOMBaptismDateStatus.bdsCompleted;
			}
			else if (S == "EXCLUDED")
			{
				result = GEDCOMBaptismDateStatus.bdsExcluded;
			}
			else if (S == "PRE-1970")
			{
				result = GEDCOMBaptismDateStatus.bdsPre1970;
			}
			else if (S == "STILLBORN")
			{
				result = GEDCOMBaptismDateStatus.bdsStillborn;
			}
			else if (S == "SUBMITTED")
			{
				result = GEDCOMBaptismDateStatus.bdsSubmitted;
			}
			else if (S == "UNCLEARED")
			{
				result = GEDCOMBaptismDateStatus.bdsUncleared;
			}
			else
			{
				result = GEDCOMBaptismDateStatus.bdsNone;
			}
			return result;
		}

		private void SetBaptismDateStatus(GEDCOMBaptismDateStatus value)
		{
			string S;
			switch (value)
			{
				case GEDCOMBaptismDateStatus.bdsChild:
				{
					S = "CHILD";
					goto IL_66;
				}
				case GEDCOMBaptismDateStatus.bdsCompleted:
				{
					S = "COMPLETED";
					goto IL_66;
				}
				case GEDCOMBaptismDateStatus.bdsExcluded:
				{
					S = "EXCLUDED";
					goto IL_66;
				}
				case GEDCOMBaptismDateStatus.bdsPre1970:
				{
					S = "PRE-1970";
					goto IL_66;
				}
				case GEDCOMBaptismDateStatus.bdsStillborn:
				{
					S = "STILLBORN";
					goto IL_66;
				}
				case GEDCOMBaptismDateStatus.bdsSubmitted:
				{
					S = "SUBMITTED";
					goto IL_66;
				}
				case GEDCOMBaptismDateStatus.bdsUncleared:
				{
					S = "UNCLEARED";
					goto IL_66;
				}
			}
			S = "";
			IL_66:
			base.SetTagStringValue("STAT", S);
		}

		private GEDCOMEndowmentDateStatus GetEndowmentDateStatus()
		{
			string S = base.GetTagStringValue("STAT").Trim().ToUpper();
			GEDCOMEndowmentDateStatus result;
			if (S == "CHILD")
			{
				result = GEDCOMEndowmentDateStatus.edsChild;
			}
			else if (S == "COMPLETED")
			{
				result = GEDCOMEndowmentDateStatus.edsCompleted;
			}
			else if (S == "EXCLUDED")
			{
				result = GEDCOMEndowmentDateStatus.edsExcluded;
			}
			else if (S == "INFANT")
			{
				result = GEDCOMEndowmentDateStatus.edsInfant;
			}
			else if (S == "PRE-1970")
			{
				result = GEDCOMEndowmentDateStatus.edsPre1970;
			}
			else if (S == "STILLBORN")
			{
				result = GEDCOMEndowmentDateStatus.edsStillborn;
			}
			else if (S == "SUBMITTED")
			{
				result = GEDCOMEndowmentDateStatus.edsSubmitted;
			}
			else if (S == "UNCLEARED")
			{
				result = GEDCOMEndowmentDateStatus.edsUncleared;
			}
			else
			{
				result = GEDCOMEndowmentDateStatus.edsNone;
			}
			return result;
		}

		private void SetEndowmentDateStatus(GEDCOMEndowmentDateStatus value)
		{
			string S;
			switch (value)
			{
				case GEDCOMEndowmentDateStatus.edsChild:
				{
					S = "CHILD";
					goto IL_72;
				}
				case GEDCOMEndowmentDateStatus.edsCompleted:
				{
					S = "COMPLETED";
					goto IL_72;
				}
				case GEDCOMEndowmentDateStatus.edsExcluded:
				{
					S = "EXCLUDED";
					goto IL_72;
				}
				case GEDCOMEndowmentDateStatus.edsInfant:
				{
					S = "INFANT";
					goto IL_72;
				}
				case GEDCOMEndowmentDateStatus.edsPre1970:
				{
					S = "PRE-1970";
					goto IL_72;
				}
				case GEDCOMEndowmentDateStatus.edsStillborn:
				{
					S = "STILLBORN";
					goto IL_72;
				}
				case GEDCOMEndowmentDateStatus.edsSubmitted:
				{
					S = "SUBMITTED";
					goto IL_72;
				}
				case GEDCOMEndowmentDateStatus.edsUncleared:
				{
					S = "UNCLEARED";
					goto IL_72;
				}
			}
			S = "";
			IL_72:
			base.SetTagStringValue("STAT", S);
		}

		private GEDCOMChildSealingDateStatus GetChildSealingDateStatus()
		{
			string S = base.GetTagStringValue("STAT").Trim().ToUpper();
			GEDCOMChildSealingDateStatus result;

			if (S == "BIC")
			{
				result = GEDCOMChildSealingDateStatus.cdsBIC;
			}
			else if (S == "EXCLUDED")
			{
				result = GEDCOMChildSealingDateStatus.cdsExcluded;
			}
			else if (S == "PRE-1970")
			{
				result = GEDCOMChildSealingDateStatus.cdsPre1970;
			}
			else if (S == "STILLBORN")
			{
				result = GEDCOMChildSealingDateStatus.cdsStillborn;
			}
			else if (S == "SUBMITTED")
			{
				result = GEDCOMChildSealingDateStatus.cdsSubmitted;
			}
			else if (S == "UNCLEARED")
			{
				result = GEDCOMChildSealingDateStatus.cdsUncleared;
			}
			else
			{
				result = GEDCOMChildSealingDateStatus.cdsNone;
			}

			return result;
		}

		private void SetChildSealingDateStatus(GEDCOMChildSealingDateStatus value)
		{
			string S;
			switch (value)
			{
				case GEDCOMChildSealingDateStatus.cdsBIC:
				{
					S = "BIC";
					goto IL_5A;
				}
				case GEDCOMChildSealingDateStatus.cdsExcluded:
				{
					S = "EXCLUDED";
					goto IL_5A;
				}
				case GEDCOMChildSealingDateStatus.cdsPre1970:
				{
					S = "PRE-1970";
					goto IL_5A;
				}
				case GEDCOMChildSealingDateStatus.cdsStillborn:
				{
					S = "STILLBORN";
					goto IL_5A;
				}
				case GEDCOMChildSealingDateStatus.cdsSubmitted:
				{
					S = "SUBMITTED";
					goto IL_5A;
				}
				case GEDCOMChildSealingDateStatus.cdsUncleared:
				{
					S = "UNCLEARED";
					goto IL_5A;
				}
			}
			S = "";
			IL_5A:
			base.SetTagStringValue("STAT", S);
		}

		public GEDCOMDateStatus DateStatus
		{
			get { return base.TagClass("STAT", GEDCOMDateStatus.Create) as GEDCOMDateStatus; }
		}

		public override GEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
		{
			GEDCOMTag result;

			if (tagName == "STAT")
			{
				result = base.AddTag(tagName, tagValue, GEDCOMDateStatus.Create);
			}
			else if (tagName == "FAMC")
			{
				result = base.AddTag(tagName, tagValue, GEDCOMPointer.Create);
			}
			else
			{
				// define "DATE" by default
				result = base.AddTag(tagName, tagValue, tagConstructor);
			}

			return result;
		}

		public GEDCOMIndividualOrdinance(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static GEDCOMTag Create(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
		{
			return new GEDCOMIndividualOrdinance(owner, parent, tagName, tagValue);
		}
	}
}
