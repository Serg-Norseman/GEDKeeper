using System;
using System.IO;
using System.Runtime.InteropServices;

using Ext.Utils;

namespace GedCom551
{
	public sealed class TGEDCOMIndividualRecord : TGEDCOMRecord
	{
		private TGEDCOMListEx<TGEDCOMPersonalName> _PersonalNames;
		private TGEDCOMListEx<TGEDCOMCustomEvent> _IndividualEvents;
		private TGEDCOMListEx<TGEDCOMIndividualOrdinance> _IndividualOrdinances;
		private TGEDCOMListEx<TGEDCOMChildToFamilyLink> _ChildToFamilyLinks;
		private TGEDCOMListEx<TGEDCOMSpouseToFamilyLink> _SpouseToFamilyLinks;
		private TGEDCOMListEx<TGEDCOMPointer> _Submittors;
		private TGEDCOMListEx<TGEDCOMAssociation> _Associations;
		private TGEDCOMListEx<TGEDCOMPointer> _Aliasses;
		private TGEDCOMListEx<TGEDCOMPointer> _AncestorsInterest;
		private TGEDCOMListEx<TGEDCOMPointer> _DescendantsInterest;
		private TGEDCOMListEx<TGEDCOMPointer> _Groups;

		public string AncestralFileNumber
		{
			get { return base.GetTagStringValue("AFN"); }
			set { base.SetTagStringValue("AFN", value); }
		}

		public string PermanentRecordFileNumber
		{
			get { return base.GetTagStringValue("RFN"); }
			set { base.SetTagStringValue("RFN", value); }
		}

		public TGEDCOMListEx<TGEDCOMCustomEvent> IndividualEvents
		{
			get { return this._IndividualEvents; }
		}

		public TGEDCOMListEx<TGEDCOMIndividualOrdinance> IndividualOrdinances
		{
			get { return this._IndividualOrdinances; }
		}

		public TGEDCOMListEx<TGEDCOMPersonalName> PersonalNames
		{
			get { return this._PersonalNames; }
		}

		public TGEDCOMRestriction Restriction
		{
			get { return GetRestrictionVal(base.GetTagStringValue("RESN").Trim().ToUpper()); }
			set { base.SetTagStringValue("RESN", GetRestrictionStr(value)); }
		}

		public TGEDCOMSex Sex
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

		public TGEDCOMListEx<TGEDCOMChildToFamilyLink> ChildToFamilyLinks
		{
			get { return this._ChildToFamilyLinks; }
		}

		public TGEDCOMListEx<TGEDCOMSpouseToFamilyLink> SpouseToFamilyLinks
		{
			get { return this._SpouseToFamilyLinks; }
		}

		public TGEDCOMListEx<TGEDCOMPointer> Submittors
		{
			get { return this._Submittors; }
		}

		public TGEDCOMListEx<TGEDCOMAssociation> Associations
		{
			get { return this._Associations; }
		}

		public TGEDCOMListEx<TGEDCOMPointer> Aliasses
		{
			get { return this._Aliasses; }
		}

		public TGEDCOMListEx<TGEDCOMPointer> AncestorsInterest
		{
			get { return this._AncestorsInterest; }
		}

		public TGEDCOMListEx<TGEDCOMPointer> DescendantsInterest
		{
			get { return this._DescendantsInterest; }
		}

		public TGEDCOMListEx<TGEDCOMPointer> Groups
		{
			get { return this._Groups; }
		}

		/*
		public new TGEDCOMNotes Notes
		{
			get { return base.GetNote(Index); }
		}*/

		/*
		public new int NotesCount
		{
			get
			{
				return base.NotesCount;
			}
		}*/

		/*
		public new TGEDCOMSourceCitation SourceCitations
		{
			get { return base.GetSourceCitation(Index); }
		}*/

		/*
		public new int SourceCitationsCount
		{
			get { return base.SourceCitationsCount; }
		}*/

		/*
		public new TGEDCOMMultimediaLink MultimediaLinks
		{
			get { return base.GetMultimediaLink(Index); }
		}*/

		/*
		public new int MultimediaLinksCount
		{
			get
			{
				return base.MultimediaLinksCount;
			}
		}*/

		private TGEDCOMSex GetSex()
		{
			string S = base.GetTagStringValue("SEX").Trim().ToUpper();
			TGEDCOMSex Result;

			switch (S) {
				case "M":
					Result = TGEDCOMSex.svMale;
					break;
				case "F":
					Result = TGEDCOMSex.svFemale;
					break;
				case "U":
					Result = TGEDCOMSex.svUndetermined;
					break;
				default:
					Result = TGEDCOMSex.svNone;
					break;
			}
			
			return Result;
		}

		private void SetSex([In] TGEDCOMSex Value)
		{
			string S = "";
			if (Value != TGEDCOMSex.svNone)
			{
				if (Value != TGEDCOMSex.svMale)
				{
					if (Value != TGEDCOMSex.svFemale)
					{
						if (Value == TGEDCOMSex.svUndetermined)
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

		private bool GetPatriarch()
		{
			return base.FindTag("_PATRIARCH", 0) != null;
		}

		private void SetPatriarch([In] bool Value)
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

		private bool GetBookmark()
		{
			return base.FindTag("_BOOKMARK", 0) != null;
		}

		private void SetBookmark([In] bool Value)
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

		protected override void CreateObj(TGEDCOMTree AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			base.SetLists(EnumSet.Create(new Enum[]
			{
				TGEDCOMSubList.stNotes, 
				TGEDCOMSubList.stSource, 
				TGEDCOMSubList.stMultimedia
			}));
			this.FRecordType = TGEDCOMRecordType.rtIndividual;
			this.FName = "INDI";

			this._PersonalNames = new TGEDCOMListEx<TGEDCOMPersonalName>(this);
			this._IndividualEvents = new TGEDCOMListEx<TGEDCOMCustomEvent>(this);
			this._IndividualOrdinances = new TGEDCOMListEx<TGEDCOMIndividualOrdinance>(this);
			this._ChildToFamilyLinks = new TGEDCOMListEx<TGEDCOMChildToFamilyLink>(this);
			this._SpouseToFamilyLinks = new TGEDCOMListEx<TGEDCOMSpouseToFamilyLink>(this);
			this._Submittors = new TGEDCOMListEx<TGEDCOMPointer>(this);
			this._Associations = new TGEDCOMListEx<TGEDCOMAssociation>(this);
			this._Aliasses = new TGEDCOMListEx<TGEDCOMPointer>(this);
			this._AncestorsInterest = new TGEDCOMListEx<TGEDCOMPointer>(this);
			this._DescendantsInterest = new TGEDCOMListEx<TGEDCOMPointer>(this);
			this._Groups = new TGEDCOMListEx<TGEDCOMPointer>(this);
		}

		public override void Dispose()
		{
			if (!this.Disposed_)
			{
				this._PersonalNames.Dispose();
				this._IndividualEvents.Dispose();
				this._IndividualOrdinances.Dispose();
				this._ChildToFamilyLinks.Dispose();
				this._SpouseToFamilyLinks.Dispose();
				this._Submittors.Dispose();
				this._Associations.Dispose();
				this._Aliasses.Dispose();
				this._AncestorsInterest.Dispose();
				this._DescendantsInterest.Dispose();
				this._Groups.Dispose();

				base.Dispose();
				this.Disposed_ = true;
			}
		}

		public override TGEDCOMTag AddTag([In] string ATag, [In] string AValue, TagConstructor ATagConstructor)
		{
			TGEDCOMTag Result;
			if (ATag == "NAME")
			{
				Result = this.AddPersonalName(new TGEDCOMPersonalName(base.Owner, this, ATag, AValue));
			}
			else
			{
				if (ATag == "FAMC")
				{
					Result = this.ChildToFamilyLinks.Add(new TGEDCOMChildToFamilyLink(base.Owner, this, ATag, AValue));
				}
				else
				{
					if (ATag == "FAMS")
					{
						Result = this.SpouseToFamilyLinks.Add(new TGEDCOMSpouseToFamilyLink(base.Owner, this, ATag, AValue));
					}
					else
					{
						if (ATag == "BIRT" || ATag == "CHR" || ATag == "DEAT" || ATag == "BURI" || ATag == "CREM" || 
							ATag == "ADOP" || ATag == "BAPM" || ATag == "BARM" || ATag == "BASM" || ATag == "BLES" || 
							ATag == "CHRA" || ATag == "CONF" || ATag == "FCOM" || ATag == "ORDN" || ATag == "NATU" || 
							ATag == "EMIG" || ATag == "IMMI" || ATag == "CENS" || ATag == "PROB" || ATag == "WILL" || 
							ATag == "GRAD" || ATag == "RETI" || ATag == "EVEN")
						{
							Result = this.AddIndividualEvent(new TGEDCOMIndividualEvent(base.Owner, this, ATag, AValue));
						}
						else
						{
							if (ATag == "CAST" || ATag == "DSCR" || ATag == "EDUC" || ATag == "IDNO" || ATag == "NATI" || 
							    ATag == "NCHI" || ATag == "NMR" || ATag == "OCCU" || ATag == "PROP" || ATag == "RELI" || 
							    ATag == "RESI" || ATag == "SSN" || ATag == "TITL" || ATag == "FACT" || ATag == "_TRAVEL" || 
							    ATag == "_HOBBY" || ATag == "_AWARD" || ATag == "_MILI" || ATag == "_MILI_IND" || 
							    ATag == "_MILI_DIS" || ATag == "_MILI_RANK")
							{
								Result = this.AddIndividualEvent(new TGEDCOMIndividualAttribute(base.Owner, this, ATag, AValue));
							}
							else
							{
								if (ATag == "SUBM")
								{
									Result = this.Submittors.Add(new TGEDCOMPointer(base.Owner, this, ATag, AValue));
								}
								else
								{
									if (ATag == "BAPL" || ATag == "CONL" || ATag == "ENDL" || ATag == "SLGC")
									{
										Result = this.IndividualOrdinances.Add(new TGEDCOMIndividualOrdinance(base.Owner, this, ATag, AValue));
									}
									else
									{
										if (ATag == "ASSO")
										{
											Result = this.Associations.Add(new TGEDCOMAssociation(base.Owner, this, ATag, AValue));
										}
										else
										{
											if (ATag == "ALIA")
											{
												Result = this.Aliasses.Add(new TGEDCOMPointer(base.Owner, this, ATag, AValue));
											}
											else
											{
												if (ATag == "ANCI")
												{
													Result = this.AncestorsInterest.Add(new TGEDCOMPointer(base.Owner, this, ATag, AValue));
												}
												else
												{
													if (ATag == "DESI")
													{
														Result = this.DescendantsInterest.Add(new TGEDCOMPointer(base.Owner, this, ATag, AValue));
													}
													else
													{
														if (ATag == "_GROUP")
														{
															Result = this._Groups.Add(new TGEDCOMPointer(base.Owner, this, ATag, AValue));
														}
														else
														{
															Result = base.AddTag(ATag, AValue, ATagConstructor);
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

			this._PersonalNames.Clear();
			this._IndividualEvents.Clear();
			this._IndividualOrdinances.Clear();
			this._ChildToFamilyLinks.Clear();
			this._SpouseToFamilyLinks.Clear();
			this._Submittors.Clear();
			this._Associations.Clear();
			this._Aliasses.Clear();
			this._AncestorsInterest.Clear();
			this._DescendantsInterest.Clear();
			this._Groups.Clear();
		}

		public override bool IsEmpty()
		{
			return base.IsEmpty() && this._PersonalNames.Count == 0 && this._IndividualEvents.Count == 0 
				&& this._IndividualOrdinances.Count == 0 && this._ChildToFamilyLinks.Count == 0 
				&& this._SpouseToFamilyLinks.Count == 0 && this._Submittors.Count == 0 
				&& this._Associations.Count == 0 && this._Aliasses.Count == 0 
				&& this._AncestorsInterest.Count == 0 && this._DescendantsInterest.Count == 0 
				&& this._Groups.Count == 0;
		}

		public TGEDCOMCustomEvent AddIndividualEvent(TGEDCOMCustomEvent Value)
		{
			if (Value != null)
			{
				Value.SetLevel(base.Level + 1);
				this._IndividualEvents.Add(Value);
			}

			return Value;
		}

		public TGEDCOMPersonalName AddPersonalName(TGEDCOMPersonalName Value)
		{
			if (Value != null)
			{
				Value.SetLevel(base.Level + 1);
				this._PersonalNames.Add(Value);
			}
			return Value;
		}

		public int IndexOfGroup(TGEDCOMGroupRecord aGroup)
		{
			int Result = -1;
			if (this._Groups != null)
			{
				int num = this._Groups.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					if (this._Groups[i].XRef == aGroup.XRef)
					{
						Result = i;
						break;
					}
				}
			}
			return Result;
		}

		public int IndexOfSpouse(TGEDCOMFamilyRecord aFamily)
		{
			int Result = -1;
			if (this._SpouseToFamilyLinks != null)
			{
				int num = this._SpouseToFamilyLinks.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					if (this._SpouseToFamilyLinks[i].Family == aFamily)
					{
						Result = i;
						break;
					}
				}
			}
			return Result;
		}

		public void DeleteSpouseToFamilyLink(TGEDCOMFamilyRecord Family)
		{
			int num = this._SpouseToFamilyLinks.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				if (this._SpouseToFamilyLinks[i].Family == Family)
				{
					this._SpouseToFamilyLinks.Delete(i);
					break;
				}
			}
		}

		public void DeleteChildToFamilyLink(TGEDCOMFamilyRecord Family)
		{
			int num = this._ChildToFamilyLinks.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				if (this._ChildToFamilyLinks[i].Family == Family)
				{
					this._ChildToFamilyLinks.Delete(i);
					break;
				}
			}
		}

		public void ExchangeSpouses(int Index1, int Index2)
		{
			this._SpouseToFamilyLinks.Exchange(Index1, Index2);
		}

		public TGEDCOMCustomEvent GetIndividualEvent(string evName)
		{
			TGEDCOMCustomEvent Result = null;

			TGEDCOMCustomEvent evt;
			int num = this._IndividualEvents.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				evt = this._IndividualEvents[i];
				if (evt.Name == evName) {
					Result = evt;
					break;
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

			if (this._PersonalNames != null && aClearDest)
			{
				while (this._PersonalNames.Count > 0)
				{
					TGEDCOMObject obj = this._PersonalNames.Extract(0);
					toRec.AddPersonalName(obj as TGEDCOMPersonalName);
				}
			}

			if (toRec.ChildToFamilyLinks.Count == 0 && this.ChildToFamilyLinks.Count != 0 && this._ChildToFamilyLinks != null)
			{
				TGEDCOMChildToFamilyLink ctf_link = this._ChildToFamilyLinks.Extract(0) as TGEDCOMChildToFamilyLink;
				TGEDCOMFamilyRecord family = ctf_link.Family;

				int num = family.Childrens.Count - 1;
				int idx = 0;
				if (num >= idx)
				{
					num++;
					while (family.Childrens[idx].StringValue != "@" + base.XRef + "@")
					{
						idx++;
						if (idx == num)
						{
							goto IL_10B;
						}
					}
					family.Childrens[idx].StringValue = "@" + aToRecord.XRef + "@";
				}
				IL_10B:
				ctf_link.ResetParent(toRec);
				toRec.ChildToFamilyLinks.Add(ctf_link);
			}

			while (this._SpouseToFamilyLinks.Count > 0)
			{
				TGEDCOMSpouseToFamilyLink stf_link = this._SpouseToFamilyLinks.Extract(0) as TGEDCOMSpouseToFamilyLink;
				TGEDCOMFamilyRecord family = stf_link.Family;
				if (family.Husband.StringValue == "@" + base.XRef + "@")
				{
					family.Husband.StringValue = "@" + aToRecord.XRef + "@";
				}
				else
				{
					if (family.Wife.StringValue == "@" + base.XRef + "@")
					{
						family.Wife.StringValue = "@" + aToRecord.XRef + "@";
					}
				}
				stf_link.ResetParent(toRec);
				toRec.SpouseToFamilyLinks.Add(stf_link);
			}

			while (this._IndividualEvents.Count > 0)
			{
				TGEDCOMCustomEvent evt = (TGEDCOMCustomEvent)this._IndividualEvents.Extract(0);
				evt.ResetParent(toRec);
				toRec.AddIndividualEvent(evt);
			}

			while (this._IndividualOrdinances.Count > 0)
			{
				TGEDCOMIndividualOrdinance ord = (TGEDCOMIndividualOrdinance)this._IndividualOrdinances.Extract(0);
				ord.ResetParent(toRec);
				toRec.IndividualOrdinances.Add(ord);
			}

			while (this._Submittors.Count > 0)
			{
				TGEDCOMObject obj = this._Submittors.Extract(0);
				(obj as TGEDCOMCustomTag).ResetParent(toRec);
				toRec.Submittors.Add(obj as TGEDCOMPointer);
			}

			while (this._Associations.Count > 0)
			{
				TGEDCOMObject obj = this._Associations.Extract(0);
				(obj as TGEDCOMCustomTag).ResetParent(toRec);
				toRec.Associations.Add(obj as TGEDCOMAssociation);
			}

			while (this._Aliasses.Count > 0)
			{
				TGEDCOMObject obj = this._Aliasses.Extract(0);
				(obj as TGEDCOMCustomTag).ResetParent(toRec);
				toRec.Aliasses.Add(obj as TGEDCOMPointer);
			}

			while (this._AncestorsInterest.Count > 0)
			{
				TGEDCOMObject obj = this._AncestorsInterest.Extract(0);
				(obj as TGEDCOMCustomTag).ResetParent(toRec);
				toRec.AncestorsInterest.Add(obj as TGEDCOMPointer);
			}

			while (this._DescendantsInterest.Count > 0)
			{
				TGEDCOMObject obj = this._DescendantsInterest.Extract(0);
				(obj as TGEDCOMCustomTag).ResetParent(toRec);
				toRec.DescendantsInterest.Add(obj as TGEDCOMPointer);
			}

			while (this._Groups.Count > 0)
			{
				TGEDCOMObject obj = this._Groups.Extract(0);
				(obj as TGEDCOMCustomTag).ResetParent(toRec);
				toRec.Groups.Add(obj as TGEDCOMPointer);
			}
		}

		public override void Pack()
		{
			base.Pack();

			this._PersonalNames.Pack();
			this._ChildToFamilyLinks.Pack();
			this._SpouseToFamilyLinks.Pack();
			this._IndividualEvents.Pack();
			this._IndividualOrdinances.Pack();
			this._Submittors.Pack();
			this._Associations.Pack();
			this._Aliasses.Pack();
			this._AncestorsInterest.Pack();
			this._DescendantsInterest.Pack();
			this._Groups.Pack();
		}

		public override void ReplaceXRefs(TXRefReplaceMap aMap)
		{
			base.ReplaceXRefs(aMap);

			this._PersonalNames.ReplaceXRefs(aMap);
			this._ChildToFamilyLinks.ReplaceXRefs(aMap);
			this._SpouseToFamilyLinks.ReplaceXRefs(aMap);
			this._IndividualEvents.ReplaceXRefs(aMap);
			this._IndividualOrdinances.ReplaceXRefs(aMap);
			this._Submittors.ReplaceXRefs(aMap);
			this._Associations.ReplaceXRefs(aMap);
			this._Aliasses.ReplaceXRefs(aMap);
			this._AncestorsInterest.ReplaceXRefs(aMap);
			this._DescendantsInterest.ReplaceXRefs(aMap);
			this._Groups.ReplaceXRefs(aMap);
		}

		public override void ResetOwner(TGEDCOMTree AOwner)
		{
			base.ResetOwner(AOwner);

			this._PersonalNames.ResetOwner(AOwner);
			this._ChildToFamilyLinks.ResetOwner(AOwner);
			this._SpouseToFamilyLinks.ResetOwner(AOwner);
			this._IndividualEvents.ResetOwner(AOwner);
			this._IndividualOrdinances.ResetOwner(AOwner);
			this._Submittors.ResetOwner(AOwner);
			this._Associations.ResetOwner(AOwner);
			this._Aliasses.ResetOwner(AOwner);
			this._AncestorsInterest.ResetOwner(AOwner);
			this._DescendantsInterest.ResetOwner(AOwner);
			this._Groups.ResetOwner(AOwner);
		}

		public override void SaveToStream(StreamWriter AStream)
		{
			base.SaveToStream(AStream);

			this._PersonalNames.SaveToStream(AStream);
			this._ChildToFamilyLinks.SaveToStream(AStream);
			this._SpouseToFamilyLinks.SaveToStream(AStream);
			this._IndividualEvents.SaveToStream(AStream);
			this._IndividualOrdinances.SaveToStream(AStream);
			this._Submittors.SaveToStream(AStream);
			this._Associations.SaveToStream(AStream);
			this._Aliasses.SaveToStream(AStream);
			this._AncestorsInterest.SaveToStream(AStream);
			this._DescendantsInterest.SaveToStream(AStream);
			this._Groups.SaveToStream(AStream);
		}

		public TGEDCOMIndividualRecord(TGEDCOMTree AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}

		public new static TGEDCOMCustomTag Create(TGEDCOMTree AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue)
		{
			return new TGEDCOMIndividualRecord(AOwner, AParent, AName, AValue);
		}

		public void aux_GetLifeDates(out TGEDCOMCustomEvent aBirthEvent, out TGEDCOMCustomEvent aDeathEvent)
		{
			aBirthEvent = null;
			aDeathEvent = null;

			int num = this.IndividualEvents.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				TGEDCOMCustomEvent ev = this.IndividualEvents[i];
				if (ev.Name == "BIRT")
				{
					aBirthEvent = ev;
				}
				else
				{
					if (ev.Name == "DEAT")
					{
						aDeathEvent = ev;
					}
				}
			}
		}

		public void aux_GetNameParts(out string aFamily, out string aName, out string aPatronymic)
		{
			if (this.PersonalNames.Count > 0)
			{
				TGEDCOMPersonalName np = this.PersonalNames[0];

				string firstPart /*, dummy*/;
				np.GetNameParts(out firstPart, out aFamily /*, out dummy*/);

				string[] parts = firstPart.Split(' ');
				if (parts.Length > 1)
				{
					aName = parts[0];
					aPatronymic = parts[1];
				} else {
					aName = firstPart;
					aPatronymic = "";
				}
			} else {
				aFamily = "";
				aName = "";
				aPatronymic = "";
			}
		}

		public void aux_GetParents(out TGEDCOMIndividualRecord father, out TGEDCOMIndividualRecord mother)
		{
			TGEDCOMFamilyRecord fam = (this.ChildToFamilyLinks.Count < 1) ? null : this.ChildToFamilyLinks[0].Value as TGEDCOMFamilyRecord;

			if (fam == null) {
				father = null;
				mother = null;
			} else {
				father = fam.Husband.Value as TGEDCOMIndividualRecord;
				mother = fam.Wife.Value as TGEDCOMIndividualRecord;
			}
		}

		public string aux_GetNameStr(bool aByFamily, bool aPieces)
		{
			string Result;
			if (this._PersonalNames.Count > 0)
			{
				TGEDCOMPersonalName np = this._PersonalNames[0];

				string firstPart, surname/*, dummy*/;
				np.GetNameParts(out firstPart, out surname /*, out dummy*/);

				if (aByFamily)
				{
					Result = surname + " " + firstPart;
				}
				else
				{
					Result = firstPart + " " + surname;
				}

				if (aPieces)
				{
					string nick = np.Pieces.Nickname;
					if (nick != "") Result = Result + " [" + nick + "]";
				}
			}
			else
			{
				Result = "";
			}
			return Result;
		}

		public string aux_GetNickStr()
		{
			string Result;
			if (this._PersonalNames.Count > 0)
			{
				TGEDCOMPersonalName np = this._PersonalNames[0];
				Result = np.Pieces.Nickname;
			}
			else
			{
				Result = "";
			}
			return Result;
		}

		public bool aux_GetIndivName(bool only_np, ref string aName)
		{
			bool Result;

			if (only_np)
			{
				string f, i, p;
				aux_GetNameParts(out f, out i, out p);
				aName = i + " " + p;
				string text = aName;
				Result = (text.Length > 3);
			}
			else
			{
				TGEDCOMPersonalName np = _PersonalNames[0];
				aName = np.StringValue;
				string firstPart = np.FirstPart;
				Result = (((firstPart != null) ? firstPart.Length : 0) > 3);
			}

			return Result;
		}

		public bool IsMatchX(TGEDCOMRecord record, bool indistinctMatching, float nameAccuracy, bool birthYear, int yearInaccuracy, bool only_np)
		{
			bool match = false;

			if (record != null) {
				TGEDCOMIndividualRecord kRec = (TGEDCOMIndividualRecord)record;

				string iName = "";
				if (this.aux_GetIndivName(only_np, ref iName)) {
					string kName = "";
					if (kRec.aux_GetIndivName(only_np, ref kName) && this.Sex == kRec.Sex && (!only_np || (this.Sex == TGEDCOMSex.svFemale && kRec.Sex == TGEDCOMSex.svFemale)))
					{
						if (!indistinctMatching) {
							match = (iName == kName);
						} else {
							match = (IndistinctMatching.GetSimilarity(iName, kName) >= nameAccuracy);
						}

						if (match && birthYear)
						{
							TGEDCOMCustomEvent ev;
							int year1 = -1, year2 = -1;
							ushort m, d;

							ev = this.GetIndividualEvent("BIRT");
							if (ev != null) ev.Detail.Date.aux_GetIndependentDate(out year1, out m, out d);

							ev = kRec.GetIndividualEvent("BIRT");
							if (ev != null) ev.Detail.Date.aux_GetIndependentDate(out year2, out m, out d);

							match = (match && year1 >= 0 && year2 >= 0 && Math.Abs(year1 - year2) <= yearInaccuracy);
						}
					}
				}

			}
			
			return match;
		}

		public override bool IsMatch(TGEDCOMRecord record, float matchThreshold)
		{
			float match = 0.0F;

			TGEDCOMIndividualRecord indi = (TGEDCOMIndividualRecord)record;

			// check name
			float nameMatch = 0F;
			for (int i = 0; i <= indi.PersonalNames.Count - 1; i++)
			{
				for (int k = 0; k <= _PersonalNames.Count - 1; k++)
				{
					float currentNameMatch = _PersonalNames[k].IsMatch(indi.PersonalNames[i]);

					nameMatch = Math.Max(nameMatch, currentNameMatch);
				}
			}

			// 0% name match would be pointless checking other details
			if (nameMatch != 0)
			{			
				// check gender
				float genderMatch = ((Sex == indi.Sex) ? 100.0F : 0.0F);

				float birthMatch = 0F;
				float deathMatch = 0F;

				TGEDCOMCustomEvent birth = null;
				TGEDCOMCustomEvent death = null;
				TGEDCOMCustomEvent indiBirth = null;
				TGEDCOMCustomEvent indiDeath = null;

				aux_GetLifeDates(out birth, out death);
				indi.aux_GetLifeDates(out indiBirth, out indiDeath);

				if (birth != null && indiBirth != null) {
					birthMatch = birth.IsMatch(indiBirth);
				} else if (birth == null && indiBirth == null) {
					birthMatch = 100.0F;
				}

				if (death != null && indiDeath != null) {
					deathMatch = death.IsMatch(indiDeath);
				} else if (death == null && indiDeath == null) {
					deathMatch = 100.0F;
				}

				// FIXME: check parents ?
				match = (nameMatch + genderMatch + birthMatch + deathMatch) / 4.0F;
			}

			return (match >= matchThreshold);
		}

	}
}
