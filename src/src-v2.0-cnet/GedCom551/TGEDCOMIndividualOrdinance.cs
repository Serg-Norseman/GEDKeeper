using GKSys;
using System;
using System.ComponentModel;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GedCom551
{
	public class TGEDCOMIndividualOrdinance : TGEDCOMTagWithLists
	{
		public enum TGEDCOMChildSealingDateStatus : byte
		{
			cdsNone,
			cdsBIC,
			cdsExcluded,
			cdsPre1970,
			cdsStillborn,
			cdsSubmitted,
			cdsUncleared
		}

		public enum TGEDCOMBaptismDateStatus : byte
		{
			bdsNone,
			bdsChild,
			bdsCompleted,
			bdsExcluded,
			bdsPre1970,
			bdsStillborn,
			bdsSubmitted,
			bdsUncleared
		}

		public enum TGEDCOMEndowmentDateStatus : byte
		{
			edsNone,
			edsChild,
			edsCompleted,
			edsExcluded,
			edsInfant,
			edsPre1970,
			edsStillborn,
			edsSubmitted,
			edsUncleared
		}

		[Browsable(false)]
		public TGEDCOMDateValue Date
		{
			get
			{
				return this.GetDate();
			}
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
			get
			{
				return this.GetStringTag(2);
			}
			set
			{
				this.SetStringTag(2, value);
			}
		}

		[Browsable(false)]
		public TGEDCOMIndividualOrdinance.TGEDCOMBaptismDateStatus BaptismDateStatus
		{
			get
			{
				return this.GetBaptismDateStatus();
			}
			set
			{
				this.SetBaptismDateStatus(value);
			}
		}

		[Browsable(false)]
		public TGEDCOMDateExact BaptismChangeDate
		{
			get
			{
				return this.GetChangeDate();
			}
		}

		[Browsable(false)]
		public TGEDCOMIndividualOrdinance.TGEDCOMEndowmentDateStatus EndowmentDateStatus
		{
			get
			{
				return this.GetEndowmentDateStatus();
			}
			set
			{
				this.SetEndowmentDateStatus(value);
			}
		}

		[Browsable(false)]
		public TGEDCOMDateExact EndowmentChangeDate
		{
			get { return this.GetChangeDate(); }
		}

		public TGEDCOMPointer Family
		{
			get { return this.GetFamily(); }
		}

		[Browsable(false)]
		public TGEDCOMIndividualOrdinance.TGEDCOMChildSealingDateStatus ChildSealingDateStatus
		{
			get
			{
				return this.GetChildSealingDateStatus();
			}
			set
			{
				this.SetChildSealingDateStatus(value);
			}
		}

		[Browsable(false)]
		public TGEDCOMDateExact ChildSealingChangeDate
		{
			get
			{
				return this.GetChangeDate();
			}
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

		internal TGEDCOMIndividualOrdinance.TGEDCOMBaptismDateStatus GetBaptismDateStatus()
		{
			string S = base.GetTagStringValue("STAT").Trim().ToUpper();
			TGEDCOMIndividualOrdinance.TGEDCOMBaptismDateStatus Result;
			if (BDSSystem.WStrCmp(S, "CHILD") == 0)
			{
				Result = TGEDCOMIndividualOrdinance.TGEDCOMBaptismDateStatus.bdsChild;
			}
			else
			{
				if (BDSSystem.WStrCmp(S, "COMPLETED") == 0)
				{
					Result = TGEDCOMIndividualOrdinance.TGEDCOMBaptismDateStatus.bdsCompleted;
				}
				else
				{
					if (BDSSystem.WStrCmp(S, "EXCLUDED") == 0)
					{
						Result = TGEDCOMIndividualOrdinance.TGEDCOMBaptismDateStatus.bdsExcluded;
					}
					else
					{
						if (BDSSystem.WStrCmp(S, "PRE-1970") == 0)
						{
							Result = TGEDCOMIndividualOrdinance.TGEDCOMBaptismDateStatus.bdsPre1970;
						}
						else
						{
							if (BDSSystem.WStrCmp(S, "STILLBORN") == 0)
							{
								Result = TGEDCOMIndividualOrdinance.TGEDCOMBaptismDateStatus.bdsStillborn;
							}
							else
							{
								if (BDSSystem.WStrCmp(S, "SUBMITTED") == 0)
								{
									Result = TGEDCOMIndividualOrdinance.TGEDCOMBaptismDateStatus.bdsSubmitted;
								}
								else
								{
									if (BDSSystem.WStrCmp(S, "UNCLEARED") == 0)
									{
										Result = TGEDCOMIndividualOrdinance.TGEDCOMBaptismDateStatus.bdsUncleared;
									}
									else
									{
										Result = TGEDCOMIndividualOrdinance.TGEDCOMBaptismDateStatus.bdsNone;
									}
								}
							}
						}
					}
				}
			}
			return Result;
		}

		internal void SetBaptismDateStatus(TGEDCOMIndividualOrdinance.TGEDCOMBaptismDateStatus Value)
		{
			string S;
			switch (Value)
			{
				case TGEDCOMIndividualOrdinance.TGEDCOMBaptismDateStatus.bdsChild:
				{
					S = "CHILD";
					goto IL_66;
				}
				case TGEDCOMIndividualOrdinance.TGEDCOMBaptismDateStatus.bdsCompleted:
				{
					S = "COMPLETED";
					goto IL_66;
				}
				case TGEDCOMIndividualOrdinance.TGEDCOMBaptismDateStatus.bdsExcluded:
				{
					S = "EXCLUDED";
					goto IL_66;
				}
				case TGEDCOMIndividualOrdinance.TGEDCOMBaptismDateStatus.bdsPre1970:
				{
					S = "PRE-1970";
					goto IL_66;
				}
				case TGEDCOMIndividualOrdinance.TGEDCOMBaptismDateStatus.bdsStillborn:
				{
					S = "STILLBORN";
					goto IL_66;
				}
				case TGEDCOMIndividualOrdinance.TGEDCOMBaptismDateStatus.bdsSubmitted:
				{
					S = "SUBMITTED";
					goto IL_66;
				}
				case TGEDCOMIndividualOrdinance.TGEDCOMBaptismDateStatus.bdsUncleared:
				{
					S = "UNCLEARED";
					goto IL_66;
				}
			}
			S = "";
			IL_66:
			base.SetTagStringValue("STAT", S);
		}

		internal TGEDCOMIndividualOrdinance.TGEDCOMEndowmentDateStatus GetEndowmentDateStatus()
		{
			string S = base.GetTagStringValue("STAT").Trim().ToUpper();
			TGEDCOMIndividualOrdinance.TGEDCOMEndowmentDateStatus Result;
			if (BDSSystem.WStrCmp(S, "CHILD") == 0)
			{
				Result = TGEDCOMIndividualOrdinance.TGEDCOMEndowmentDateStatus.edsChild;
			}
			else
			{
				if (BDSSystem.WStrCmp(S, "COMPLETED") == 0)
				{
					Result = TGEDCOMIndividualOrdinance.TGEDCOMEndowmentDateStatus.edsCompleted;
				}
				else
				{
					if (BDSSystem.WStrCmp(S, "EXCLUDED") == 0)
					{
						Result = TGEDCOMIndividualOrdinance.TGEDCOMEndowmentDateStatus.edsExcluded;
					}
					else
					{
						if (BDSSystem.WStrCmp(S, "INFANT") == 0)
						{
							Result = TGEDCOMIndividualOrdinance.TGEDCOMEndowmentDateStatus.edsInfant;
						}
						else
						{
							if (BDSSystem.WStrCmp(S, "PRE-1970") == 0)
							{
								Result = TGEDCOMIndividualOrdinance.TGEDCOMEndowmentDateStatus.edsPre1970;
							}
							else
							{
								if (BDSSystem.WStrCmp(S, "STILLBORN") == 0)
								{
									Result = TGEDCOMIndividualOrdinance.TGEDCOMEndowmentDateStatus.edsStillborn;
								}
								else
								{
									if (BDSSystem.WStrCmp(S, "SUBMITTED") == 0)
									{
										Result = TGEDCOMIndividualOrdinance.TGEDCOMEndowmentDateStatus.edsSubmitted;
									}
									else
									{
										if (BDSSystem.WStrCmp(S, "UNCLEARED") == 0)
										{
											Result = TGEDCOMIndividualOrdinance.TGEDCOMEndowmentDateStatus.edsUncleared;
										}
										else
										{
											Result = TGEDCOMIndividualOrdinance.TGEDCOMEndowmentDateStatus.edsNone;
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

		internal void SetEndowmentDateStatus(TGEDCOMIndividualOrdinance.TGEDCOMEndowmentDateStatus Value)
		{
			string S;
			switch (Value)
			{
				case TGEDCOMIndividualOrdinance.TGEDCOMEndowmentDateStatus.edsChild:
				{
					S = "CHILD";
					goto IL_72;
				}
				case TGEDCOMIndividualOrdinance.TGEDCOMEndowmentDateStatus.edsCompleted:
				{
					S = "COMPLETED";
					goto IL_72;
				}
				case TGEDCOMIndividualOrdinance.TGEDCOMEndowmentDateStatus.edsExcluded:
				{
					S = "EXCLUDED";
					goto IL_72;
				}
				case TGEDCOMIndividualOrdinance.TGEDCOMEndowmentDateStatus.edsInfant:
				{
					S = "INFANT";
					goto IL_72;
				}
				case TGEDCOMIndividualOrdinance.TGEDCOMEndowmentDateStatus.edsPre1970:
				{
					S = "PRE-1970";
					goto IL_72;
				}
				case TGEDCOMIndividualOrdinance.TGEDCOMEndowmentDateStatus.edsStillborn:
				{
					S = "STILLBORN";
					goto IL_72;
				}
				case TGEDCOMIndividualOrdinance.TGEDCOMEndowmentDateStatus.edsSubmitted:
				{
					S = "SUBMITTED";
					goto IL_72;
				}
				case TGEDCOMIndividualOrdinance.TGEDCOMEndowmentDateStatus.edsUncleared:
				{
					S = "UNCLEARED";
					goto IL_72;
				}
			}
			S = "";
			IL_72:
			base.SetTagStringValue("STAT", S);
		}

		internal TGEDCOMIndividualOrdinance.TGEDCOMChildSealingDateStatus GetChildSealingDateStatus()
		{
			string S = base.GetTagStringValue("STAT").Trim().ToUpper();
			TGEDCOMIndividualOrdinance.TGEDCOMChildSealingDateStatus Result;
			if (BDSSystem.WStrCmp(S, "BIC") == 0)
			{
				Result = TGEDCOMIndividualOrdinance.TGEDCOMChildSealingDateStatus.cdsBIC;
			}
			else
			{
				if (BDSSystem.WStrCmp(S, "EXCLUDED") == 0)
				{
					Result = TGEDCOMIndividualOrdinance.TGEDCOMChildSealingDateStatus.cdsExcluded;
				}
				else
				{
					if (BDSSystem.WStrCmp(S, "PRE-1970") == 0)
					{
						Result = TGEDCOMIndividualOrdinance.TGEDCOMChildSealingDateStatus.cdsPre1970;
					}
					else
					{
						if (BDSSystem.WStrCmp(S, "STILLBORN") == 0)
						{
							Result = TGEDCOMIndividualOrdinance.TGEDCOMChildSealingDateStatus.cdsStillborn;
						}
						else
						{
							if (BDSSystem.WStrCmp(S, "SUBMITTED") == 0)
							{
								Result = TGEDCOMIndividualOrdinance.TGEDCOMChildSealingDateStatus.cdsSubmitted;
							}
							else
							{
								if (BDSSystem.WStrCmp(S, "UNCLEARED") == 0)
								{
									Result = TGEDCOMIndividualOrdinance.TGEDCOMChildSealingDateStatus.cdsUncleared;
								}
								else
								{
									Result = TGEDCOMIndividualOrdinance.TGEDCOMChildSealingDateStatus.cdsNone;
								}
							}
						}
					}
				}
			}
			return Result;
		}

		internal void SetChildSealingDateStatus(TGEDCOMIndividualOrdinance.TGEDCOMChildSealingDateStatus Value)
		{
			string S;
			switch (Value)
			{
				case TGEDCOMIndividualOrdinance.TGEDCOMChildSealingDateStatus.cdsBIC:
				{
					S = "BIC";
					goto IL_5A;
				}
				case TGEDCOMIndividualOrdinance.TGEDCOMChildSealingDateStatus.cdsExcluded:
				{
					S = "EXCLUDED";
					goto IL_5A;
				}
				case TGEDCOMIndividualOrdinance.TGEDCOMChildSealingDateStatus.cdsPre1970:
				{
					S = "PRE-1970";
					goto IL_5A;
				}
				case TGEDCOMIndividualOrdinance.TGEDCOMChildSealingDateStatus.cdsStillborn:
				{
					S = "STILLBORN";
					goto IL_5A;
				}
				case TGEDCOMIndividualOrdinance.TGEDCOMChildSealingDateStatus.cdsSubmitted:
				{
					S = "SUBMITTED";
					goto IL_5A;
				}
				case TGEDCOMIndividualOrdinance.TGEDCOMChildSealingDateStatus.cdsUncleared:
				{
					S = "UNCLEARED";
					goto IL_5A;
				}
			}
			S = "";
			IL_5A:
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

		internal TGEDCOMPointer GetFamily()
		{
			return base.TagClass("FAMC", typeof(TGEDCOMPointer)) as TGEDCOMPointer;
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
					if (BDSSystem.WStrCmp(ATag, "FAMC") == 0)
					{
						Result = base.AddTag(ATag, AValue, typeof(TGEDCOMPointer));
					}
					else
					{
						Result = base.AddTag(ATag, AValue, AClass);
					}
				}
			}
			return Result;
		}

		public TGEDCOMIndividualOrdinance(TGEDCOMObject AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
