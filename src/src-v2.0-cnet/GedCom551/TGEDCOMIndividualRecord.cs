using GKSys;
using System;
using System.ComponentModel;
using System.IO;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GedCom551
{
	public class TGEDCOMIndividualRecord : TGEDCOMRecord
	{
		internal TGEDCOMList FPersonalNames;
		internal TGEDCOMList FIndividualEvents;
		internal TGEDCOMList FIndividualOrdinances;
		internal TGEDCOMList FChildToFamilyLinks;
		internal TGEDCOMList FSpouseToFamilyLinks;
		internal TGEDCOMList FSubmittors;
		internal TGEDCOMList FAssociations;
		internal TGEDCOMList FAliasses;
		internal TGEDCOMList FAncestorsInterest;
		internal TGEDCOMList FDescendantsInterest;
		internal TGEDCOMList FGroups;

		[Browsable(false)]
		public string AncestralFileNumber
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
		public string AutomatedRecordID
		{
			get
			{
				return this.GetStringTag(3);
			}
			set
			{
				this.SetStringTag(3, value);
			}
		}

		/*[Browsable(false)]
		public TGEDCOMCustomEvent IndividualEvents
		{
			get
			{
				return this.GetIndividualEvent(Index);
			}
		}*/
		[Browsable(false)]
		public int IndividualEventsCount
		{
			get
			{
				return this.GetIndividualEventsCount();
			}
		}

		/*[Browsable(false)]
		public TGEDCOMIndividualOrdinance IndividualOrdinances
		{
			get
			{
				return this.GetIndividualOrdinance(Index);
			}
		}*/
		[Browsable(false)]
		public int IndividualOrdinancesCount
		{
			get
			{
				return this.GetIndividualOrdinancesCount();
			}
		}

		[Browsable(false)]
		public string PermanentRecordFileNumber
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

		/*public TGEDCOMPersonalName PersonalNames
		{
			get { return this.GetPersonalName(Index); }
		}*/
		public int PersonalNamesCount
		{
			get { return this.GetPersonalNamesCount(); }
		}

		public TGEDCOMObject.TGEDCOMRestriction Restriction
		{
			get { return this.GetRestriction(); }
			set { this.SetRestriction(value); }
		}

		public TGEDCOMObject.TGEDCOMSex Sex
		{
			get { return this.GetSex(); }
			set { this.SetSex(value); }
		}

		public bool Bookmark
		{
			get { return this.GetBookmark(); }
			set { this.SetBookmark(value); }
		}

		public bool Patriarch
		{
			get { return this.GetPatriarch(); }
			set { this.SetPatriarch(value); }
		}

		/*[Browsable(false)]
		public TGEDCOMChildToFamilyLink ChildToFamilyLinks
		{
			get
			{
				return this.GetChildToFamilyLink(Index);
			}
		}*/
		[Browsable(false)]
		public int ChildToFamilyLinksCount
		{
			get
			{
				return this.GetChildToFamilyLinksCount();
			}
		}

		/*[Browsable(false)]
		public TGEDCOMSpouseToFamilyLink SpouseToFamilyLinks
		{
			get
			{
				return this.GetSpouseToFamilyLink(Index);
			}
		}*/
		[Browsable(false)]
		public int SpouseToFamilyLinksCount
		{
			get
			{
				return this.GetSpouseToFamilyLinksCount();
			}
		}

		/*[Browsable(false)]
		public TGEDCOMPointer Submittors
		{
			get
			{
				return this.GetSubmittor(Index);
			}
		}*/

		[Browsable(false)]
		public int SubmittorsCount
		{
			get
			{
				return this.GetSubmittorsCount();
			}
		}

		/*[Browsable(false)]
		public TGEDCOMAssociation Associations
		{
			get
			{
				return this.GetAssociation(Index);
			}
		}*/
		[Browsable(false)]
		public int AssociationsCount
		{
			get
			{
				return this.GetAssociationsCount();
			}
		}

		/*[Browsable(false)]
		public TGEDCOMPointer Aliasses
		{
			get
			{
				return this.GetAlias(Index);
			}
		}*/
		[Browsable(false)]
		public int AliassesCount
		{
			get
			{
				return this.GetAliasesCount();
			}
		}

		/*[Browsable(false)]
		public TGEDCOMPointer AncestorsInterest
		{
			get
			{
				return this.GetAncestorsInterest(Index);
			}
		}*/
		[Browsable(false)]
		public int AncestorsInterestCount
		{
			get
			{
				return this.GetAncestorsInterestCount();
			}
		}

		/*[Browsable(false)]
		public TGEDCOMPointer DescendantsInterest
		{
			get
			{
				return this.GetDescendantsInterest(Index);
			}
		}*/
		[Browsable(false)]
		public int DescendantsInterestCount
		{
			get
			{
				return this.GetDescendantsInterestCount();
			}
		}

		/*[Browsable(false)]
		public TGEDCOMPointer Groups
		{
			get
			{
				return this.GetGroup(Index);
			}
		}*/
		[Browsable(false)]
		public int GroupsCount
		{
			get
			{
				return this.GetGroupsCount();
			}
		}

		/*[Browsable(false)]
		public new TGEDCOMNotes Notes
		{
			get
			{
				return base.GetNote(Index);
			}
		}*/

		/*[Browsable(false)]
		public new int NotesCount
		{
			get
			{
				return base.NotesCount;
			}
		}*/

		/*[Browsable(false)]
		public new TGEDCOMSourceCitation SourceCitations
		{
			get
			{
				return base.GetSourceCitation(Index);
			}
		}*/

		/*[Browsable(false)]
		public new int SourceCitationsCount
		{
			get
			{
				return base.SourceCitationsCount;
			}
		}*/

		/*[Browsable(false)]
		public new TGEDCOMMultimediaLink MultimediaLinks
		{
			get
			{
				return base.GetMultimediaLink(Index);
			}
		}*/

		/*[Browsable(false)]
		public new int MultimediaLinksCount
		{
			get
			{
				return base.MultimediaLinksCount;
			}
		}*/

		internal TGEDCOMObject.TGEDCOMRestriction GetRestriction()
		{
			string S = base.GetTagStringValue("RESN").Trim().ToUpper();
			TGEDCOMObject.TGEDCOMRestriction Result;
			if (BDSSystem.WStrCmp(S, "CONFIDENTIAL") == 0)
			{
				Result = TGEDCOMObject.TGEDCOMRestriction.rnConfidential;
			}
			else
			{
				if (BDSSystem.WStrCmp(S, "LOCKED") == 0)
				{
					Result = TGEDCOMObject.TGEDCOMRestriction.rnLocked;
				}
				else
				{
					if (BDSSystem.WStrCmp(S, "PRIVACY") == 0)
					{
						Result = TGEDCOMObject.TGEDCOMRestriction.rnPrivacy;
					}
					else
					{
						Result = TGEDCOMObject.TGEDCOMRestriction.rnNone;
					}
				}
			}
			return Result;
		}

		internal void SetRestriction([In] TGEDCOMObject.TGEDCOMRestriction Value)
		{
			string S = "";
			if (Value != TGEDCOMObject.TGEDCOMRestriction.rnNone)
			{
				if (Value != TGEDCOMObject.TGEDCOMRestriction.rnConfidential)
				{
					if (Value != TGEDCOMObject.TGEDCOMRestriction.rnLocked)
					{
						if (Value == TGEDCOMObject.TGEDCOMRestriction.rnPrivacy)
						{
							S = "privacy";
						}
					}
					else
					{
						S = "locked";
					}
				}
				else
				{
					S = "confidential";
				}
			}
			else
			{
				S = "";
			}
			base.SetTagStringValue("RESN", S);
		}

		public TGEDCOMPersonalName GetPersonalName(int Index)
		{
			TGEDCOMPersonalName Result;
			if (this.FPersonalNames == null)
			{
				Result = null;
			}
			else
			{
				Result = (this.FPersonalNames[Index] as TGEDCOMPersonalName);
			}
			return Result;
		}

		internal int GetPersonalNamesCount()
		{
			int Result;
			if (this.FPersonalNames == null)
			{
				Result = 0;
			}
			else
			{
				Result = this.FPersonalNames.Count;
			}
			return Result;
		}

		internal TGEDCOMObject.TGEDCOMSex GetSex()
		{
			string S = base.GetTagStringValue("SEX").Trim().ToUpper();
			TGEDCOMObject.TGEDCOMSex Result;
			if (BDSSystem.WStrCmp(S, "") == 0)
			{
				Result = TGEDCOMObject.TGEDCOMSex.svNone;
			}
			else
			{
				if (BDSSystem.WStrCmp(S, "M") == 0)
				{
					Result = TGEDCOMObject.TGEDCOMSex.svMale;
				}
				else
				{
					if (BDSSystem.WStrCmp(S, "F") == 0)
					{
						Result = TGEDCOMObject.TGEDCOMSex.svFemale;
					}
					else
					{
						if (BDSSystem.WStrCmp(S, "U") == 0)
						{
							Result = TGEDCOMObject.TGEDCOMSex.svUndetermined;
						}
						else
						{
							Result = TGEDCOMObject.TGEDCOMSex.svNone;
						}
					}
				}
			}
			return Result;
		}

		internal void SetSex([In] TGEDCOMObject.TGEDCOMSex Value)
		{
			string S = "";
			if (Value != TGEDCOMObject.TGEDCOMSex.svNone)
			{
				if (Value != TGEDCOMObject.TGEDCOMSex.svMale)
				{
					if (Value != TGEDCOMObject.TGEDCOMSex.svFemale)
					{
						if (Value == TGEDCOMObject.TGEDCOMSex.svUndetermined)
						{
							S = "U";
						}
					}
					else
					{
						S = "F";
					}
				}
				else
				{
					S = "M";
				}
			}
			else
			{
				S = "";
			}
			base.SetTagStringValue("SEX", S);
		}

		public TGEDCOMCustomEvent GetIndividualEvent(int Index)
		{
			TGEDCOMCustomEvent Result;
			if (this.FIndividualEvents == null)
			{
				Result = null;
			}
			else
			{
				Result = (this.FIndividualEvents[Index] as TGEDCOMCustomEvent);
			}
			return Result;
		}

		internal int GetIndividualEventsCount()
		{
			int Result;
			if (this.FIndividualEvents == null)
			{
				Result = 0;
			}
			else
			{
				Result = this.FIndividualEvents.Count;
			}
			return Result;
		}
		
		public TGEDCOMIndividualOrdinance GetIndividualOrdinance(int Index)
		{
			TGEDCOMIndividualOrdinance Result;
			if (this.FIndividualOrdinances == null)
			{
				Result = null;
			}
			else
			{
				Result = (this.FIndividualOrdinances[Index] as TGEDCOMIndividualOrdinance);
			}
			return Result;
		}

		internal int GetIndividualOrdinancesCount()
		{
			int Result;
			if (this.FIndividualOrdinances == null)
			{
				Result = 0;
			}
			else
			{
				Result = this.FIndividualOrdinances.Count;
			}
			return Result;
		}

		public TGEDCOMChildToFamilyLink GetChildToFamilyLink(int Index)
		{
			TGEDCOMChildToFamilyLink Result;
			if (this.FChildToFamilyLinks == null)
			{
				Result = null;
			}
			else
			{
				Result = (this.FChildToFamilyLinks[Index] as TGEDCOMChildToFamilyLink);
			}
			return Result;
		}

		internal int GetChildToFamilyLinksCount()
		{
			int Result;
			if (this.FChildToFamilyLinks == null)
			{
				Result = 0;
			}
			else
			{
				Result = this.FChildToFamilyLinks.Count;
			}
			return Result;
		}

		public TGEDCOMSpouseToFamilyLink GetSpouseToFamilyLink(int Index)
		{
			TGEDCOMSpouseToFamilyLink Result;
			if (this.FSpouseToFamilyLinks == null)
			{
				Result = null;
			}
			else
			{
				Result = (this.FSpouseToFamilyLinks[Index] as TGEDCOMSpouseToFamilyLink);
			}
			return Result;
		}

		internal int GetSpouseToFamilyLinksCount()
		{
			int Result;
			if (this.FSpouseToFamilyLinks == null)
			{
				Result = 0;
			}
			else
			{
				Result = this.FSpouseToFamilyLinks.Count;
			}
			return Result;
		}

		public TGEDCOMPointer GetSubmittor(int Index)
		{
			TGEDCOMPointer Result;
			if (this.FSubmittors == null)
			{
				Result = null;
			}
			else
			{
				Result = (this.FSubmittors[Index] as TGEDCOMPointer);
			}
			return Result;
		}

		internal int GetSubmittorsCount()
		{
			int Result;
			if (this.FSubmittors == null)
			{
				Result = 0;
			}
			else
			{
				Result = this.FSubmittors.Count;
			}
			return Result;
		}

		public TGEDCOMAssociation GetAssociation(int Index)
		{
			TGEDCOMAssociation Result;
			if (this.FAssociations == null)
			{
				Result = null;
			}
			else
			{
				Result = (this.FAssociations[Index] as TGEDCOMAssociation);
			}
			return Result;
		}

		internal int GetAssociationsCount()
		{
			int Result;
			if (this.FAssociations == null)
			{
				Result = 0;
			}
			else
			{
				Result = this.FAssociations.Count;
			}
			return Result;
		}

		public TGEDCOMPointer GetAlias(int Index)
		{
			TGEDCOMPointer Result;
			if (this.FAliasses == null)
			{
				Result = null;
			}
			else
			{
				Result = (this.FAliasses[Index] as TGEDCOMPointer);
			}
			return Result;
		}

		internal int GetAliasesCount()
		{
			int Result;
			if (this.FAliasses == null)
			{
				Result = 0;
			}
			else
			{
				Result = this.FAliasses.Count;
			}
			return Result;
		}
		internal TGEDCOMPointer GetAncestorsInterest(int Index)
		{
			TGEDCOMPointer Result;
			if (this.FAncestorsInterest == null)
			{
				Result = null;
			}
			else
			{
				Result = (this.FAncestorsInterest[Index] as TGEDCOMPointer);
			}
			return Result;
		}
		internal int GetAncestorsInterestCount()
		{
			int Result;
			if (this.FAncestorsInterest == null)
			{
				Result = 0;
			}
			else
			{
				Result = this.FAncestorsInterest.Count;
			}
			return Result;
		}
		internal TGEDCOMPointer GetDescendantsInterest(int Index)
		{
			TGEDCOMPointer Result;
			if (this.FDescendantsInterest == null)
			{
				Result = null;
			}
			else
			{
				Result = (this.FDescendantsInterest[Index] as TGEDCOMPointer);
			}
			return Result;
		}
		internal int GetDescendantsInterestCount()
		{
			int Result;
			if (this.FDescendantsInterest == null)
			{
				Result = 0;
			}
			else
			{
				Result = this.FDescendantsInterest.Count;
			}
			return Result;
		}
		internal string GetStringTag(int Index)
		{
			string Result = "";
			if (Index != 1)
			{
				if (Index != 2)
				{
					if (Index == 3)
					{
						Result = base.GetTagStringValue("RIN");
					}
				}
				else
				{
					Result = base.GetTagStringValue("AFN");
				}
			}
			else
			{
				Result = base.GetTagStringValue("RFN");
			}
			return Result;
		}
		internal void SetStringTag(int Index, [In] string Value)
		{
			if (Index != 1)
			{
				if (Index != 2)
				{
					if (Index == 3)
					{
						base.SetTagStringValue("RIN", Value);
					}
				}
				else
				{
					base.SetTagStringValue("AFN", Value);
				}
			}
			else
			{
				base.SetTagStringValue("RFN", Value);
			}
		}

		internal TGEDCOMPointer GetGroup(int Index)
		{
			TGEDCOMPointer Result;
			if (this.FGroups == null)
			{
				Result = null;
			}
			else
			{
				Result = (this.FGroups[Index] as TGEDCOMPointer);
			}
			return Result;
		}

		internal int GetGroupsCount()
		{
			int Result;
			if (this.FGroups == null)
			{
				Result = 0;
			}
			else
			{
				Result = this.FGroups.Count;
			}
			return Result;
		}
		internal bool GetPatriarch()
		{
			return base.FindTag("_PATRIARCH", 0) != null;
		}
		internal void SetPatriarch([In] bool Value)
		{
			if (Value)
			{
				if (base.FindTag("_PATRIARCH", 0) == null)
				{
					this.AddTag("_PATRIARCH", "", null);
				}
			}
			else
			{
				base.DeleteTag("_PATRIARCH");
			}
		}
		internal bool GetBookmark()
		{
			return base.FindTag("_BOOKMARK", 0) != null;
		}
		internal void SetBookmark([In] bool Value)
		{
			if (Value)
			{
				if (base.FindTag("_BOOKMARK", 0) == null)
				{
					this.AddTag("_BOOKMARK", "", null);
				}
			}
			else
			{
				base.DeleteTag("_BOOKMARK");
			}
		}
		protected internal override void CreateObj(TGEDCOMObject AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			base.SetLists(TEnumSet.Create(new Enum[]
			{
				TGEDCOMObject.TGEDCOMSubList.stNotes, 
				TGEDCOMObject.TGEDCOMSubList.stSource, 
				TGEDCOMObject.TGEDCOMSubList.stMultimedia
			}));
			this.FRecordType = TGEDCOMRecord.TGEDCOMRecordType.rtIndividual;
			this.FName = "INDI";
			this.FPersonalNames = null;
			this.FIndividualEvents = null;
			this.FIndividualOrdinances = null;
			this.FChildToFamilyLinks = null;
			this.FSpouseToFamilyLinks = null;
			this.FSubmittors = null;
			this.FAssociations = null;
			this.FAliasses = null;
			this.FAncestorsInterest = null;
			this.FDescendantsInterest = null;
			this.FNotes = null;
			this.FSourceCitations = null;
			this.FMultimediaLinks = null;
			this.FGroups = null;
		}

		public override void Dispose()
		{
			if (!this.Disposed_)
			{
				if (this.FPersonalNames != null)
				{
					this.FPersonalNames.Free();
				}
				if (this.FIndividualEvents != null)
				{
					this.FIndividualEvents.Free();
				}
				if (this.FIndividualOrdinances != null)
				{
					this.FIndividualOrdinances.Free();
				}
				if (this.FChildToFamilyLinks != null)
				{
					this.FChildToFamilyLinks.Free();
				}
				if (this.FSpouseToFamilyLinks != null)
				{
					this.FSpouseToFamilyLinks.Free();
				}
				if (this.FSubmittors != null)
				{
					this.FSubmittors.Free();
				}
				if (this.FAssociations != null)
				{
					this.FAssociations.Free();
				}
				if (this.FAliasses != null)
				{
					this.FAliasses.Free();
				}
				if (this.FAncestorsInterest != null)
				{
					this.FAncestorsInterest.Free();
				}
				if (this.FDescendantsInterest != null)
				{
					this.FDescendantsInterest.Free();
				}
				if (this.FGroups != null)
				{
					this.FGroups.Free();
				}
				base.Dispose();
				this.Disposed_ = true;
			}
		}

		public override TGEDCOMTag AddTag([In] string ATag, [In] string AValue, Type AClass)
		{
			TGEDCOMTag Result;
			if (ATag.Equals("NAME"))
			{
				Result = this.AddPersonalName(new TGEDCOMPersonalName(base.Owner, this, ATag, AValue));
			}
			else
			{
				if (ATag.Equals("FAMC"))
				{
					Result = this.AddChildToFamilyLink(new TGEDCOMChildToFamilyLink(base.Owner, this, ATag, AValue));
				}
				else
				{
					if (ATag.Equals("FAMS"))
					{
						Result = this.AddSpouseToFamilyLink(new TGEDCOMSpouseToFamilyLink(base.Owner, this, ATag, AValue));
					}
					else
					{
						if (ATag.Equals("BIRT") || ATag.Equals("CHR") || ATag.Equals("DEAT") || ATag.Equals("BURI") || ATag.Equals("CREM") || ATag.Equals("ADOP") || ATag.Equals("BAPM") || ATag.Equals("BARM") || ATag.Equals("BASM") || ATag.Equals("BLES") || ATag.Equals("CHRA") || ATag.Equals("CONF") || ATag.Equals("FCOM") || ATag.Equals("ORDN") || ATag.Equals("NATU") || ATag.Equals("EMIG") || ATag.Equals("IMMI") || ATag.Equals("CENS") || ATag.Equals("PROB") || ATag.Equals("WILL") || ATag.Equals("GRAD") || ATag.Equals("RETI") || ATag.Equals("EVEN"))
						{
							Result = this.AddIndividualEvent(new TGEDCOMIndividualEvent(base.Owner, this, ATag, AValue));
						}
						else
						{
							if (ATag.Equals("CAST") || ATag.Equals("DSCR") || ATag.Equals("EDUC") || ATag.Equals("IDNO") || BDSSystem.WStrCmp(ATag, "NATI") == 0 || BDSSystem.WStrCmp(ATag, "NCHI") == 0 || BDSSystem.WStrCmp(ATag, "NMR") == 0 || BDSSystem.WStrCmp(ATag, "OCCU") == 0 || BDSSystem.WStrCmp(ATag, "PROP") == 0 || BDSSystem.WStrCmp(ATag, "RELI") == 0 || BDSSystem.WStrCmp(ATag, "RESI") == 0 || BDSSystem.WStrCmp(ATag, "SSN") == 0 || BDSSystem.WStrCmp(ATag, "TITL") == 0 || BDSSystem.WStrCmp(ATag, "FACT") == 0 || BDSSystem.WStrCmp(ATag, "_TRAVEL") == 0 || BDSSystem.WStrCmp(ATag, "_HOBBY") == 0 || BDSSystem.WStrCmp(ATag, "_AWARD") == 0 || BDSSystem.WStrCmp(ATag, "_MILI") == 0 || ATag.Equals("_MILI_IND") || ATag.Equals("_MILI_DIS") || ATag.Equals("_MILI_RANK"))
							{
								Result = this.AddIndividualEvent(new TGEDCOMIndividualAttribute(base.Owner, this, ATag, AValue));
							}
							else
							{
								if (ATag.Equals("SUBM"))
								{
									Result = this.AddSubmittor(new TGEDCOMPointer(base.Owner, this, ATag, AValue));
								}
								else
								{
									if (ATag.Equals("BAPL") || ATag.Equals("CONL") || ATag.Equals("ENDL") || ATag.Equals("SLGC"))
									{
										Result = this.AddIndividualOrdinance(new TGEDCOMIndividualOrdinance(base.Owner, this, ATag, AValue));
									}
									else
									{
										if (ATag.Equals("ASSO"))
										{
											Result = this.AddAssociation(new TGEDCOMAssociation(base.Owner, this, ATag, AValue));
										}
										else
										{
											if (ATag.Equals("ALIA"))
											{
												Result = this.AddAlias(new TGEDCOMPointer(base.Owner, this, ATag, AValue));
											}
											else
											{
												if (ATag.Equals("ANCI"))
												{
													Result = this.AddAncestorsInterest(new TGEDCOMPointer(base.Owner, this, ATag, AValue));
												}
												else
												{
													if (ATag.Equals("DESI"))
													{
														Result = this.AddDescendantsInterest(new TGEDCOMPointer(base.Owner, this, ATag, AValue));
													}
													else
													{
														if (ATag.Equals("REFN"))
														{
															Result = base.AddUserReference(new TGEDCOMUserReference(base.Owner, this, ATag, AValue));
														}
														else
														{
															if (ATag.Equals("_GROUP"))
															{
																Result = this.AddGroup(new TGEDCOMPointer(base.Owner, this, ATag, AValue));
															}
															else
															{
																Result = base.AddTag(ATag, AValue, AClass);
															}
														}
													}
												}
											}
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
		public override void Clear()
		{
			base.Clear();
			if (this.FPersonalNames != null)
			{
				this.FPersonalNames.Clear();
			}
			if (this.FIndividualEvents != null)
			{
				this.FIndividualEvents.Clear();
			}
			if (this.FIndividualOrdinances != null)
			{
				this.FIndividualOrdinances.Clear();
			}
			if (this.FChildToFamilyLinks != null)
			{
				this.FChildToFamilyLinks.Clear();
			}
			if (this.FSpouseToFamilyLinks != null)
			{
				this.FSpouseToFamilyLinks.Clear();
			}
			if (this.FSubmittors != null)
			{
				this.FSubmittors.Clear();
			}
			if (this.FAssociations != null)
			{
				this.FAssociations.Clear();
			}
			if (this.FAliasses != null)
			{
				this.FAliasses.Clear();
			}
			if (this.FAncestorsInterest != null)
			{
				this.FAncestorsInterest.Clear();
			}
			if (this.FDescendantsInterest != null)
			{
				this.FDescendantsInterest.Clear();
			}
			if (this.FGroups != null)
			{
				this.FGroups.Clear();
			}
		}
		public override bool IsEmpty()
		{
			return base.IsEmpty() && this.GetPersonalNamesCount() == 0 && this.GetIndividualEventsCount() == 0 && this.GetIndividualOrdinancesCount() == 0 && this.GetChildToFamilyLinksCount() == 0 && this.GetSpouseToFamilyLinksCount() == 0 && this.GetSubmittorsCount() == 0 && this.GetAssociationsCount() == 0 && this.GetAliasesCount() == 0 && this.GetAncestorsInterestCount() == 0 && this.GetDescendantsInterestCount() == 0 && this.GetGroupsCount() == 0;
		}
		public TGEDCOMPointer AddAlias(TGEDCOMPointer Value)
		{
			if (this.FAliasses == null)
			{
				this.FAliasses = new TGEDCOMList(this);
			}
			if (Value != null)
			{
				this.FAliasses.Add(Value);
			}
			return Value;
		}
		public TGEDCOMPointer AddAncestorsInterest(TGEDCOMPointer Value)
		{
			if (this.FAncestorsInterest == null)
			{
				this.FAncestorsInterest = new TGEDCOMList(this);
			}
			if (Value != null)
			{
				this.FAncestorsInterest.Add(Value);
			}
			return Value;
		}
		public TGEDCOMAssociation AddAssociation(TGEDCOMAssociation Value)
		{
			if (this.FAssociations == null)
			{
				this.FAssociations = new TGEDCOMList(this);
			}
			if (Value != null)
			{
				this.FAssociations.Add(Value);
			}
			return Value;
		}
		public TGEDCOMChildToFamilyLink AddChildToFamilyLink(TGEDCOMChildToFamilyLink Value)
		{
			if (this.FChildToFamilyLinks == null)
			{
				this.FChildToFamilyLinks = new TGEDCOMList(this);
			}
			if (Value != null)
			{
				this.FChildToFamilyLinks.Add(Value);
			}
			return Value;
		}
		public TGEDCOMPointer AddDescendantsInterest(TGEDCOMPointer Value)
		{
			if (this.FDescendantsInterest == null)
			{
				this.FDescendantsInterest = new TGEDCOMList(this);
			}
			if (Value != null)
			{
				this.FDescendantsInterest.Add(Value);
			}
			return Value;
		}
		public TGEDCOMCustomEvent AddIndividualEvent(TGEDCOMCustomEvent Value)
		{
			if (this.FIndividualEvents == null)
			{
				this.FIndividualEvents = new TGEDCOMList(this);
			}
			if (Value != null)
			{
				Value.SetLevel(base.Level + 1);
				this.FIndividualEvents.Add(Value);
			}
			return Value;
		}
		public TGEDCOMIndividualOrdinance AddIndividualOrdinance(TGEDCOMIndividualOrdinance Value)
		{
			if (this.FIndividualOrdinances == null)
			{
				this.FIndividualOrdinances = new TGEDCOMList(this);
			}
			if (Value != null)
			{
				this.FIndividualOrdinances.Add(Value);
			}
			return Value;
		}
		public TGEDCOMPersonalName AddPersonalName(TGEDCOMPersonalName Value)
		{
			if (this.FPersonalNames == null)
			{
				this.FPersonalNames = new TGEDCOMList(this);
			}
			if (Value != null)
			{
				Value.SetLevel(base.Level + 1);
				this.FPersonalNames.Add(Value);
			}
			return Value;
		}
		public TGEDCOMSpouseToFamilyLink AddSpouseToFamilyLink(TGEDCOMSpouseToFamilyLink Value)
		{
			if (this.FSpouseToFamilyLinks == null)
			{
				this.FSpouseToFamilyLinks = new TGEDCOMList(this);
			}
			if (Value != null)
			{
				this.FSpouseToFamilyLinks.Add(Value);
			}
			return Value;
		}
		public TGEDCOMPointer AddSubmittor(TGEDCOMPointer Value)
		{
			if (this.FSubmittors == null)
			{
				this.FSubmittors = new TGEDCOMList(this);
			}
			if (Value != null)
			{
				this.FSubmittors.Add(Value);
			}
			return Value;
		}
		public TGEDCOMPointer AddGroup(TGEDCOMPointer Value)
		{
			if (this.FGroups == null)
			{
				this.FGroups = new TGEDCOMList(this);
			}
			if (Value != null)
			{
				this.FGroups.Add(Value);
			}
			return Value;
		}
		public void DeleteGroup(int aIndex)
		{
			if (this.FGroups != null)
			{
				this.FGroups.Delete(aIndex);
			}
		}
		public int IndexOfGroup(TGEDCOMGroupRecord aGroup)
		{
			int Result = -1;
			if (this.FGroups != null)
			{
				int arg_19_0 = 0;
				int num = this.FGroups.Count - 1;
				int i = arg_19_0;
				if (num >= i)
				{
					num++;
					while (BDSSystem.WStrCmp((this.FGroups[i] as TGEDCOMPointer).XRef, aGroup.XRef) != 0)
					{
						i++;
						if (i == num)
						{
							return Result;
						}
					}
					Result = i;
				}
			}
			return Result;
		}
		public int IndexOfEvent(TGEDCOMCustomEvent Event)
		{
			int Result = -1;
			if (this.FIndividualEvents != null)
			{
				int arg_19_0 = 0;
				int num = this.FIndividualEvents.Count - 1;
				int i = arg_19_0;
				if (num >= i)
				{
					num++;
					while (!object.Equals(this.FIndividualEvents[i], Event))
					{
						i++;
						if (i == num)
						{
							return Result;
						}
					}
					Result = i;
				}
			}
			return Result;
		}
		public int IndexOfSpouse(TGEDCOMFamilyRecord Family)
		{
			int Result = -1;
			if (this.FSpouseToFamilyLinks != null)
			{
				int arg_19_0 = 0;
				int num = this.FSpouseToFamilyLinks.Count - 1;
				int i = arg_19_0;
				if (num >= i)
				{
					num++;
					while (!object.Equals((this.FSpouseToFamilyLinks[i] as TGEDCOMSpouseToFamilyLink).Family, Family))
					{
						i++;
						if (i == num)
						{
							return Result;
						}
					}
					Result = i;
				}
			}
			return Result;
		}
		public void DeleteAssociation(int aIndex)
		{
			if (this.FAssociations != null)
			{
				this.FAssociations.Delete(aIndex);
			}
		}
		public void DeleteAssociation(TGEDCOMAssociation aAssociation)
		{
			if (this.FAssociations != null)
			{
				this.FAssociations.DeleteObject(aAssociation);
			}
		}
		public void DeleteIndividualEvent(TGEDCOMCustomEvent aEvent)
		{
			if (this.FIndividualEvents != null)
			{
				this.FIndividualEvents.DeleteObject(aEvent);
			}
		}
		public void DeleteIndividualEvent(int Index)
		{
			if (this.FIndividualEvents != null)
			{
				this.FIndividualEvents.Delete(Index);
			}
		}
		public void DeleteSpouseToFamilyLink(TGEDCOMFamilyRecord Family)
		{
			if (this.FSpouseToFamilyLinks != null)
			{
				int arg_17_0 = 0;
				int num = this.FSpouseToFamilyLinks.Count - 1;
				int i = arg_17_0;
				if (num >= i)
				{
					num++;
					while (!object.Equals((this.FSpouseToFamilyLinks[i] as TGEDCOMSpouseToFamilyLink).Family, Family))
					{
						i++;
						if (i == num)
						{
							return;
						}
					}
					this.FSpouseToFamilyLinks.Delete(i);
				}
			}
		}
		public void DeleteSpouseToFamilyLink(int Index)
		{
			if (this.FSpouseToFamilyLinks != null)
			{
				this.FSpouseToFamilyLinks.Delete(Index);
			}
		}
		public void DeleteChildToFamilyLink(TGEDCOMFamilyRecord Family)
		{
			if (this.FChildToFamilyLinks != null)
			{
				int arg_17_0 = 0;
				int num = this.FChildToFamilyLinks.Count - 1;
				int i = arg_17_0;
				if (num >= i)
				{
					num++;
					while (!object.Equals((this.FChildToFamilyLinks[i] as TGEDCOMChildToFamilyLink).Family, Family))
					{
						i++;
						if (i == num)
						{
							return;
						}
					}
					this.FChildToFamilyLinks.Delete(i);
				}
			}
		}
		public void DeleteChildToFamilyLink(int Index)
		{
			if (this.FChildToFamilyLinks == null)
			{
				this.FChildToFamilyLinks.Delete(Index);
			}
		}
		public void ExchangeEvents(int Index1, int Index2)
		{
			this.FIndividualEvents.Exchange(Index1, Index2);
		}
		public void ExchangeSpouses(int Index1, int Index2)
		{
			if (this.FSpouseToFamilyLinks != null)
			{
				this.FSpouseToFamilyLinks.Exchange(Index1, Index2);
			}
		}
		public TGEDCOMCustomEvent GetIndividualEvent(string evName)
		{
			TGEDCOMCustomEvent Result = null;
			if (this.FIndividualEvents != null)
			{
				int arg_19_0 = 0;
				int num = this.FIndividualEvents.Count - 1;
				int i = arg_19_0;
				if (num >= i)
				{
					num++;
					TGEDCOMCustomEvent @event;
					while (true)
					{
						@event = (this.FIndividualEvents[i] as TGEDCOMCustomEvent);
						if (BDSSystem.WStrCmp(@event.Name, evName) == 0)
						{
							break;
						}
						i++;
						if (i == num)
						{
							return Result;
						}
					}
					Result = @event;
				}
			}
			return Result;
		}
		public bool IsLive()
		{
			return this.GetIndividualEvent("DEAT") == null;
		}
		public override void MoveTo(TGEDCOMRecord aToRecord, bool aClearDest)
		{
			if (!aClearDest)
			{
				base.DeleteTag("SEX");
				base.DeleteTag("_UID");
			}
			base.MoveTo(aToRecord, aClearDest);
			TGEDCOMIndividualRecord toRec = aToRecord as TGEDCOMIndividualRecord;
			if (this.FPersonalNames != null && aClearDest)
			{
				while (this.FPersonalNames.Count > 0)
				{
					TGEDCOMObject obj = this.FPersonalNames.Extract(0);
					toRec.AddPersonalName(obj as TGEDCOMPersonalName);
				}
			}
			if (toRec.ChildToFamilyLinksCount == 0 && this.ChildToFamilyLinksCount != 0 && this.FChildToFamilyLinks != null)
			{
				TGEDCOMChildToFamilyLink ctf_link = this.FChildToFamilyLinks.Extract(0) as TGEDCOMChildToFamilyLink;
				TGEDCOMFamilyRecord family = ctf_link.Family;
				int arg_A4_0 = 0;
				int num = family.ChildrenCount - 1;
				int idx = arg_A4_0;
				if (num >= idx)
				{
					num++;
					while (BDSSystem.WStrCmp(family.GetChildren(idx).StringValue, "@" + base.XRef + "@") != 0)
					{
						idx++;
						if (idx == num)
						{
							goto IL_10B;
						}
					}
					family.GetChildren(idx).StringValue = "@" + aToRecord.XRef + "@";
				}
				IL_10B:
				ctf_link.ResetParent(toRec);
				toRec.AddChildToFamilyLink(ctf_link);
			}
			if (this.FSpouseToFamilyLinks != null)
			{
				while (this.FSpouseToFamilyLinks.Count > 0)
				{
					TGEDCOMSpouseToFamilyLink stf_link = this.FSpouseToFamilyLinks.Extract(0) as TGEDCOMSpouseToFamilyLink;
					TGEDCOMFamilyRecord family = stf_link.Family;
					if (BDSSystem.WStrCmp(family.Husband.StringValue, "@" + base.XRef + "@") == 0)
					{
						family.Husband.StringValue = "@" + aToRecord.XRef + "@";
					}
					else
					{
						if (BDSSystem.WStrCmp(family.Wife.StringValue, "@" + base.XRef + "@") == 0)
						{
							family.Wife.StringValue = "@" + aToRecord.XRef + "@";
						}
					}
					stf_link.ResetParent(toRec);
					toRec.AddSpouseToFamilyLink(stf_link);
				}
			}
			if (this.FIndividualEvents != null)
			{
				while (this.FIndividualEvents.Count > 0)
				{
					TGEDCOMObject obj = this.FIndividualEvents.Extract(0);
					(obj as TGEDCOMCustomTag).ResetParent(toRec);
					toRec.AddIndividualEvent(obj as TGEDCOMIndividualEvent);
				}
			}
			if (this.FIndividualOrdinances != null)
			{
				while (this.FIndividualOrdinances.Count > 0)
				{
					TGEDCOMObject obj = this.FIndividualOrdinances.Extract(0);
					(obj as TGEDCOMCustomTag).ResetParent(toRec);
					toRec.AddIndividualOrdinance(obj as TGEDCOMIndividualOrdinance);
				}
			}
			if (this.FSubmittors != null)
			{
				while (this.FSubmittors.Count > 0)
				{
					TGEDCOMObject obj = this.FSubmittors.Extract(0);
					(obj as TGEDCOMCustomTag).ResetParent(toRec);
					toRec.AddSubmittor(obj as TGEDCOMPointer);
				}
			}
			if (this.FAssociations != null)
			{
				while (this.FAssociations.Count > 0)
				{
					TGEDCOMObject obj = this.FAssociations.Extract(0);
					(obj as TGEDCOMCustomTag).ResetParent(toRec);
					toRec.AddAssociation(obj as TGEDCOMAssociation);
				}
			}
			if (this.FAliasses != null)
			{
				while (this.FAliasses.Count > 0)
				{
					TGEDCOMObject obj = this.FAliasses.Extract(0);
					(obj as TGEDCOMCustomTag).ResetParent(toRec);
					toRec.AddAlias(obj as TGEDCOMPointer);
				}
			}
			if (this.FAncestorsInterest != null)
			{
				while (this.FAncestorsInterest.Count > 0)
				{
					TGEDCOMObject obj = this.FAncestorsInterest.Extract(0);
					(obj as TGEDCOMCustomTag).ResetParent(toRec);
					toRec.AddAncestorsInterest(obj as TGEDCOMPointer);
				}
			}
			if (this.FDescendantsInterest != null)
			{
				while (this.FDescendantsInterest.Count > 0)
				{
					TGEDCOMObject obj = this.FDescendantsInterest.Extract(0);
					(obj as TGEDCOMCustomTag).ResetParent(toRec);
					toRec.AddDescendantsInterest(obj as TGEDCOMPointer);
				}
			}
			if (this.FGroups != null)
			{
				while (this.FGroups.Count > 0)
				{
					TGEDCOMObject obj = this.FGroups.Extract(0);
					(obj as TGEDCOMCustomTag).ResetParent(toRec);
					toRec.AddGroup(obj as TGEDCOMPointer);
				}
			}
		}
		public override void Pack()
		{
			base.Pack();
			if (this.FPersonalNames != null)
			{
				this.FPersonalNames.Pack();
			}
			if (this.FChildToFamilyLinks != null)
			{
				this.FChildToFamilyLinks.Pack();
			}
			if (this.FSpouseToFamilyLinks != null)
			{
				this.FSpouseToFamilyLinks.Pack();
			}
			if (this.FIndividualEvents != null)
			{
				this.FIndividualEvents.Pack();
			}
			if (this.FIndividualOrdinances != null)
			{
				this.FIndividualOrdinances.Pack();
			}
			if (this.FSubmittors != null)
			{
				this.FSubmittors.Pack();
			}
			if (this.FAssociations != null)
			{
				this.FAssociations.Pack();
			}
			if (this.FAliasses != null)
			{
				this.FAliasses.Pack();
			}
			if (this.FAncestorsInterest != null)
			{
				this.FAncestorsInterest.Pack();
			}
			if (this.FDescendantsInterest != null)
			{
				this.FDescendantsInterest.Pack();
			}
			if (this.FGroups != null)
			{
				this.FGroups.Pack();
			}
		}
		public override void ReplaceXRefs(TXRefReplaceMap aMap)
		{
			base.ReplaceXRefs(aMap);
			if (this.FPersonalNames != null)
			{
				this.FPersonalNames.ReplaceXRefs(aMap);
			}
			if (this.FChildToFamilyLinks != null)
			{
				this.FChildToFamilyLinks.ReplaceXRefs(aMap);
			}
			if (this.FSpouseToFamilyLinks != null)
			{
				this.FSpouseToFamilyLinks.ReplaceXRefs(aMap);
			}
			if (this.FIndividualEvents != null)
			{
				this.FIndividualEvents.ReplaceXRefs(aMap);
			}
			if (this.FIndividualOrdinances != null)
			{
				this.FIndividualOrdinances.ReplaceXRefs(aMap);
			}
			if (this.FSubmittors != null)
			{
				this.FSubmittors.ReplaceXRefs(aMap);
			}
			if (this.FAssociations != null)
			{
				this.FAssociations.ReplaceXRefs(aMap);
			}
			if (this.FAliasses != null)
			{
				this.FAliasses.ReplaceXRefs(aMap);
			}
			if (this.FAncestorsInterest != null)
			{
				this.FAncestorsInterest.ReplaceXRefs(aMap);
			}
			if (this.FDescendantsInterest != null)
			{
				this.FDescendantsInterest.ReplaceXRefs(aMap);
			}
			if (this.FGroups != null)
			{
				this.FGroups.ReplaceXRefs(aMap);
			}
		}
		public override void ResetOwner(TGEDCOMObject AOwner)
		{
			base.ResetOwner(AOwner);
			if (this.FPersonalNames != null)
			{
				this.FPersonalNames.ResetOwner(AOwner);
			}
			if (this.FChildToFamilyLinks != null)
			{
				this.FChildToFamilyLinks.ResetOwner(AOwner);
			}
			if (this.FSpouseToFamilyLinks != null)
			{
				this.FSpouseToFamilyLinks.ResetOwner(AOwner);
			}
			if (this.FIndividualEvents != null)
			{
				this.FIndividualEvents.ResetOwner(AOwner);
			}
			if (this.FIndividualOrdinances != null)
			{
				this.FIndividualOrdinances.ResetOwner(AOwner);
			}
			if (this.FSubmittors != null)
			{
				this.FSubmittors.ResetOwner(AOwner);
			}
			if (this.FAssociations != null)
			{
				this.FAssociations.ResetOwner(AOwner);
			}
			if (this.FAliasses != null)
			{
				this.FAliasses.ResetOwner(AOwner);
			}
			if (this.FAncestorsInterest != null)
			{
				this.FAncestorsInterest.ResetOwner(AOwner);
			}
			if (this.FDescendantsInterest != null)
			{
				this.FDescendantsInterest.ResetOwner(AOwner);
			}
			if (this.FGroups != null)
			{
				this.FGroups.ResetOwner(AOwner);
			}
		}
		public override void SaveToStream(StreamWriter AStream)
		{
			base.SaveToStream(AStream);
			if (this.FPersonalNames != null)
			{
				this.FPersonalNames.SaveToStream(AStream);
			}
			if (this.FChildToFamilyLinks != null)
			{
				this.FChildToFamilyLinks.SaveToStream(AStream);
			}
			if (this.FSpouseToFamilyLinks != null)
			{
				this.FSpouseToFamilyLinks.SaveToStream(AStream);
			}
			if (this.FIndividualEvents != null)
			{
				this.FIndividualEvents.SaveToStream(AStream);
			}
			if (this.FIndividualOrdinances != null)
			{
				this.FIndividualOrdinances.SaveToStream(AStream);
			}
			if (this.FSubmittors != null)
			{
				this.FSubmittors.SaveToStream(AStream);
			}
			if (this.FAssociations != null)
			{
				this.FAssociations.SaveToStream(AStream);
			}
			if (this.FAliasses != null)
			{
				this.FAliasses.SaveToStream(AStream);
			}
			if (this.FAncestorsInterest != null)
			{
				this.FAncestorsInterest.SaveToStream(AStream);
			}
			if (this.FDescendantsInterest != null)
			{
				this.FDescendantsInterest.SaveToStream(AStream);
			}
			if (this.FGroups != null)
			{
				this.FGroups.SaveToStream(AStream);
			}
		}

		public TGEDCOMIndividualRecord(TGEDCOMObject AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
