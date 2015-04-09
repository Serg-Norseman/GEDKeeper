using System.IO;
using ExtUtils;
using GKCommon.GEDCOM.Enums;

namespace GKCommon.GEDCOM
{
	public sealed class GEDCOMIndividualRecord : GEDCOMRecord
	{
        private static readonly GEDCOMFactory fTagsFactory;
        
        private GEDCOMList<GEDCOMPersonalName> fPersonalNames;
		private GEDCOMList<GEDCOMCustomEvent> fIndividualEvents;
		private GEDCOMList<GEDCOMIndividualOrdinance> fIndividualOrdinances;
		private GEDCOMList<GEDCOMChildToFamilyLink> fChildToFamilyLinks;
		private GEDCOMList<GEDCOMSpouseToFamilyLink> fSpouseToFamilyLinks;
		private GEDCOMList<GEDCOMPointer> fSubmittors;
		private GEDCOMList<GEDCOMAssociation> fAssociations;
		private GEDCOMList<GEDCOMAlias> fAliasses;
		private GEDCOMList<GEDCOMPointer> fAncestorsInterest;
		private GEDCOMList<GEDCOMPointer> fDescendantsInterest;
		private GEDCOMList<GEDCOMPointer> fGroups;


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

		public GEDCOMList<GEDCOMCustomEvent> IndividualEvents
		{
			get { return this.fIndividualEvents; }
		}

		public GEDCOMList<GEDCOMIndividualOrdinance> IndividualOrdinances
		{
			get { return this.fIndividualOrdinances; }
		}

		public GEDCOMList<GEDCOMPersonalName> PersonalNames
		{
			get { return this.fPersonalNames; }
		}

		public GEDCOMRestriction Restriction
		{
			get { return GEDCOMUtils.GetRestrictionVal(base.GetTagStringValue("RESN")); }
			set { base.SetTagStringValue("RESN", GEDCOMUtils.GetRestrictionStr(value)); }
		}

		public GEDCOMSex Sex
		{
			get { return GEDCOMUtils.GetSexVal(base.GetTagStringValue("SEX")); }
			set { base.SetTagStringValue("SEX", GEDCOMUtils.GetSexStr(value)); }
		}

		public bool Bookmark
		{
			get {
				return base.FindTag("_BOOKMARK", 0) != null;
			}
			set {
				if (value) {
					if (base.FindTag("_BOOKMARK", 0) == null) {
						this.AddTag("_BOOKMARK", "", null);
					}
				} else {
					base.DeleteTag("_BOOKMARK");
				}
			}
		}

		public bool Patriarch
		{
			get {
				return base.FindTag("_PATRIARCH", 0) != null;
			}
			set {
				if (value) {
					if (base.FindTag("_PATRIARCH", 0) == null) {
						this.AddTag("_PATRIARCH", "", null);
					}
				} else {
					base.DeleteTag("_PATRIARCH");
				}
			}
		}

		public GEDCOMList<GEDCOMChildToFamilyLink> ChildToFamilyLinks
		{
			get { return this.fChildToFamilyLinks; }
		}

		public GEDCOMList<GEDCOMSpouseToFamilyLink> SpouseToFamilyLinks
		{
			get { return this.fSpouseToFamilyLinks; }
		}

		public GEDCOMList<GEDCOMPointer> Submittors
		{
			get { return this.fSubmittors; }
		}

		public GEDCOMList<GEDCOMAssociation> Associations
		{
			get { return this.fAssociations; }
		}

		public GEDCOMList<GEDCOMAlias> Aliasses
		{
			get { return this.fAliasses; }
		}

		public GEDCOMList<GEDCOMPointer> AncestorsInterest
		{
			get { return this.fAncestorsInterest; }
		}

		public GEDCOMList<GEDCOMPointer> DescendantsInterest
		{
			get { return this.fDescendantsInterest; }
		}

		public GEDCOMList<GEDCOMPointer> Groups
		{
			get { return this.fGroups; }
		}

		protected override void CreateObj(GEDCOMTree owner, GEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.fRecordType = GEDCOMRecordType.rtIndividual;
			this.fName = "INDI";

			this.fPersonalNames = new GEDCOMList<GEDCOMPersonalName>(this);
			this.fIndividualEvents = new GEDCOMList<GEDCOMCustomEvent>(this);
			this.fIndividualOrdinances = new GEDCOMList<GEDCOMIndividualOrdinance>(this);
			this.fChildToFamilyLinks = new GEDCOMList<GEDCOMChildToFamilyLink>(this);
			this.fSpouseToFamilyLinks = new GEDCOMList<GEDCOMSpouseToFamilyLink>(this);
			this.fSubmittors = new GEDCOMList<GEDCOMPointer>(this);
			this.fAssociations = new GEDCOMList<GEDCOMAssociation>(this);
			this.fAliasses = new GEDCOMList<GEDCOMAlias>(this);
			this.fAncestorsInterest = new GEDCOMList<GEDCOMPointer>(this);
			this.fDescendantsInterest = new GEDCOMList<GEDCOMPointer>(this);
			this.fGroups = new GEDCOMList<GEDCOMPointer>(this);
		}

		protected override void Dispose(bool disposing)
		{
			if (disposing)
			{
				this.fPersonalNames.Dispose();
				this.fIndividualEvents.Dispose();
				this.fIndividualOrdinances.Dispose();
				this.fChildToFamilyLinks.Dispose();
				this.fSpouseToFamilyLinks.Dispose();
				this.fSubmittors.Dispose();
				this.fAssociations.Dispose();
				this.fAliasses.Dispose();
				this.fAncestorsInterest.Dispose();
				this.fDescendantsInterest.Dispose();
				this.fGroups.Dispose();
			}
			base.Dispose(disposing);
		}

		static GEDCOMIndividualRecord()
		{
			GEDCOMFactory f = new GEDCOMFactory();
			fTagsFactory = f;

			//f.RegisterTag("xxxx", xxxxxx.Create);
			
			f.RegisterTag("FAMC", GEDCOMChildToFamilyLink.Create);
			f.RegisterTag("FAMS", GEDCOMSpouseToFamilyLink.Create);
			f.RegisterTag("ASSO", GEDCOMAssociation.Create);
			f.RegisterTag("ALIA", GEDCOMAlias.Create);
			
			f.RegisterTag("BAPL", GEDCOMIndividualOrdinance.Create);
			f.RegisterTag("CONL", GEDCOMIndividualOrdinance.Create);
			f.RegisterTag("ENDL", GEDCOMIndividualOrdinance.Create);
			f.RegisterTag("SLGC", GEDCOMIndividualOrdinance.Create);
			
			// // //

			f.RegisterTag("BIRT", GEDCOMIndividualEvent.Create);
			f.RegisterTag("CHR", GEDCOMIndividualEvent.Create);
			f.RegisterTag("DEAT", GEDCOMIndividualEvent.Create);
			f.RegisterTag("BURI", GEDCOMIndividualEvent.Create);
			f.RegisterTag("CREM", GEDCOMIndividualEvent.Create);
			
			f.RegisterTag("ADOP", GEDCOMIndividualEvent.Create);
			f.RegisterTag("BAPM", GEDCOMIndividualEvent.Create);
			f.RegisterTag("BARM", GEDCOMIndividualEvent.Create);
			f.RegisterTag("BASM", GEDCOMIndividualEvent.Create);
			f.RegisterTag("BLES", GEDCOMIndividualEvent.Create);

	        f.RegisterTag("CHRA", GEDCOMIndividualEvent.Create);
			f.RegisterTag("CONF", GEDCOMIndividualEvent.Create);
			f.RegisterTag("FCOM", GEDCOMIndividualEvent.Create);
			f.RegisterTag("ORDN", GEDCOMIndividualEvent.Create);
			f.RegisterTag("NATU", GEDCOMIndividualEvent.Create);

			f.RegisterTag("EMIG", GEDCOMIndividualEvent.Create);
			f.RegisterTag("IMMI", GEDCOMIndividualEvent.Create);
			f.RegisterTag("CENS", GEDCOMIndividualEvent.Create);
			f.RegisterTag("PROB", GEDCOMIndividualEvent.Create);
			f.RegisterTag("WILL", GEDCOMIndividualEvent.Create);

			f.RegisterTag("GRAD", GEDCOMIndividualEvent.Create);
			f.RegisterTag("RETI", GEDCOMIndividualEvent.Create);
			f.RegisterTag("EVEN", GEDCOMIndividualEvent.Create);


			f.RegisterTag("CAST", GEDCOMIndividualAttribute.Create);
			f.RegisterTag("DSCR", GEDCOMIndividualAttribute.Create);
			f.RegisterTag("EDUC", GEDCOMIndividualAttribute.Create);
			f.RegisterTag("IDNO", GEDCOMIndividualAttribute.Create);
			f.RegisterTag("NATI", GEDCOMIndividualAttribute.Create);

			f.RegisterTag("NCHI", GEDCOMIndividualAttribute.Create);
			f.RegisterTag("NMR", GEDCOMIndividualAttribute.Create);
			f.RegisterTag("OCCU", GEDCOMIndividualAttribute.Create);
			f.RegisterTag("PROP", GEDCOMIndividualAttribute.Create);
			f.RegisterTag("RELI", GEDCOMIndividualAttribute.Create);

			f.RegisterTag("RESI", GEDCOMIndividualAttribute.Create);
			f.RegisterTag("SSN", GEDCOMIndividualAttribute.Create);
			f.RegisterTag("TITL", GEDCOMIndividualAttribute.Create);
			f.RegisterTag("FACT", GEDCOMIndividualAttribute.Create);
			f.RegisterTag("_TRAVEL", GEDCOMIndividualAttribute.Create);

			f.RegisterTag("_HOBBY", GEDCOMIndividualAttribute.Create);
			f.RegisterTag("_AWARD", GEDCOMIndividualAttribute.Create);
			f.RegisterTag("_MILI", GEDCOMIndividualAttribute.Create);
			f.RegisterTag("_MILI_IND", GEDCOMIndividualAttribute.Create);
			f.RegisterTag("_MILI_DIS", GEDCOMIndividualAttribute.Create);
			f.RegisterTag("_MILI_RANK", GEDCOMIndividualAttribute.Create);

			//f.RegisterTag("_BGRO", GEDCOMBloodGroup.Create);
		}

		public override GEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
		{
			GEDCOMTag result;

			if (tagName == "NAME")
			{
				result = this.AddPersonalName(new GEDCOMPersonalName(base.Owner, this, tagName, tagValue));
			}
			else if (tagName == "SUBM")
			{
				result = this.Submittors.Add(new GEDCOMPointer(base.Owner, this, tagName, tagValue));
			}
			else if (tagName == "ANCI")
			{
				result = this.AncestorsInterest.Add(new GEDCOMPointer(base.Owner, this, tagName, tagValue));
			}
			else if (tagName == "DESI")
			{
				result = this.DescendantsInterest.Add(new GEDCOMPointer(base.Owner, this, tagName, tagValue));
			}
			else if (tagName == "_GROUP")
			{
				result = this.fGroups.Add(new GEDCOMPointer(base.Owner, this, tagName, tagValue));
			}
			else
			{
				result = fTagsFactory.CreateTag(this.Owner, this, tagName, tagValue);

				if (result != null)
				{
					if (result is GEDCOMChildToFamilyLink) {
						result = this.ChildToFamilyLinks.Add(result as GEDCOMChildToFamilyLink);
					} else if (result is GEDCOMSpouseToFamilyLink) {
						result = this.SpouseToFamilyLinks.Add(result as GEDCOMSpouseToFamilyLink);
					} else if (result is GEDCOMIndividualOrdinance) {
						result = this.IndividualOrdinances.Add(result as GEDCOMIndividualOrdinance);
					} else if (result is GEDCOMAssociation) {
						result = this.Associations.Add(result as GEDCOMAssociation);
					} else if (result is GEDCOMIndividualEvent) {
						result = this.AddIndividualEvent(result as GEDCOMCustomEvent);
					} else if (result is GEDCOMIndividualAttribute) {
						result = this.AddIndividualEvent(result as GEDCOMCustomEvent);
					} else if (result is GEDCOMAlias) {
						result = this.Aliasses.Add(result as GEDCOMAlias);
					}
				} else {
					result = base.AddTag(tagName, tagValue, tagConstructor);
				}
			}

			return result;
		}

		public override void Clear()
		{
			base.Clear();

			this.fPersonalNames.Clear();
			this.fIndividualEvents.Clear();
			this.fIndividualOrdinances.Clear();
			this.fChildToFamilyLinks.Clear();
			this.fSpouseToFamilyLinks.Clear();
			this.fSubmittors.Clear();
			this.fAssociations.Clear();
			this.fAliasses.Clear();
			this.fAncestorsInterest.Clear();
			this.fDescendantsInterest.Clear();
			this.fGroups.Clear();
		}

		public override bool IsEmpty()
		{
			return base.IsEmpty() && this.fPersonalNames.Count == 0 && this.fIndividualEvents.Count == 0 
				&& this.fIndividualOrdinances.Count == 0 && this.fChildToFamilyLinks.Count == 0 
				&& this.fSpouseToFamilyLinks.Count == 0 && this.fSubmittors.Count == 0 
				&& this.fAssociations.Count == 0 && this.fAliasses.Count == 0 
				&& this.fAncestorsInterest.Count == 0 && this.fDescendantsInterest.Count == 0 
				&& this.fGroups.Count == 0;
		}

		public GEDCOMCustomEvent AddIndividualEvent(GEDCOMCustomEvent value)
		{
			if (value != null)
			{
				value.SetLevel(base.Level + 1);
				this.fIndividualEvents.Add(value);
			}

			return value;
		}

		public GEDCOMPersonalName AddPersonalName(GEDCOMPersonalName value)
		{
			if (value != null)
			{
				value.SetLevel(base.Level + 1);
				this.fPersonalNames.Add(value);
			}
			return value;
		}

		public int IndexOfGroup(GEDCOMGroupRecord groupRec)
		{
			int result = -1;
            if (groupRec == null) return result;

			if (this.fGroups != null)
			{
				int num = this.fGroups.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					if (this.fGroups[i].XRef == groupRec.XRef)
					{
						result = i;
						break;
					}
				}
			}
			return result;
		}

		public int IndexOfSpouse(GEDCOMFamilyRecord familyRec)
		{
			int result = -1;
			if (this.fSpouseToFamilyLinks != null)
			{
				int num = this.fSpouseToFamilyLinks.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					if (this.fSpouseToFamilyLinks[i].Family == familyRec)
					{
						result = i;
						break;
					}
				}
			}
			return result;
		}

		public void DeleteSpouseToFamilyLink(GEDCOMFamilyRecord familyRec)
		{
			int num = this.fSpouseToFamilyLinks.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				if (this.fSpouseToFamilyLinks[i].Family == familyRec)
				{
					this.fSpouseToFamilyLinks.Delete(i);
					break;
				}
			}
		}

		public void DeleteChildToFamilyLink(GEDCOMFamilyRecord familyRec)
		{
			int num = this.fChildToFamilyLinks.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				if (this.fChildToFamilyLinks[i].Family == familyRec)
				{
					this.fChildToFamilyLinks.Delete(i);
					break;
				}
			}
		}

		public void ExchangeSpouses(int index1, int index2)
		{
			this.fSpouseToFamilyLinks.Exchange(index1, index2);
		}

		public GEDCOMCustomEvent GetIndividualEvent(string eventName)
		{
			GEDCOMCustomEvent result = null;

			int num = this.fIndividualEvents.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				GEDCOMCustomEvent evt = this.fIndividualEvents[i];
				if (evt.Name == eventName) {
					result = evt;
					break;
				}
			}

			return result;
		}

		public bool IsLive()
		{
			return this.GetIndividualEvent("DEAT") == null;
		}

		public override void MoveTo(GEDCOMRecord targetRecord, bool clearDest)
		{
			if (!clearDest)
			{
				base.DeleteTag("SEX");
				base.DeleteTag("_UID");
			}

			base.MoveTo(targetRecord, clearDest);

			GEDCOMIndividualRecord toRec = targetRecord as GEDCOMIndividualRecord;

			if (this.fPersonalNames != null && clearDest)
			{
				while (this.fPersonalNames.Count > 0)
				{
                    GEDCOMPersonalName obj = this.fPersonalNames.Extract(0);
                    obj.ResetParent(toRec);
					toRec.AddPersonalName(obj);
				}
			}

			if (toRec.ChildToFamilyLinks.Count == 0 && this.ChildToFamilyLinks.Count != 0 && this.fChildToFamilyLinks != null)
			{
				GEDCOMChildToFamilyLink ctf_link = this.fChildToFamilyLinks.Extract(0);
				GEDCOMFamilyRecord family = ctf_link.Family;

				int num = family.Childrens.Count - 1;
				for (int idx = 0; idx <= num; idx++)
				{
					if (family.Childrens[idx].StringValue == "@" + base.XRef + "@") {
						family.Childrens[idx].StringValue = "@" + targetRecord.XRef + "@";
					}
				}
				
				ctf_link.ResetParent(toRec);
				toRec.ChildToFamilyLinks.Add(ctf_link);
			}

			while (this.fSpouseToFamilyLinks.Count > 0)
			{
				GEDCOMSpouseToFamilyLink stf_link = this.fSpouseToFamilyLinks.Extract(0);
				GEDCOMFamilyRecord family = stf_link.Family;

				if (family.Husband.StringValue == "@" + base.XRef + "@") {
					family.Husband.StringValue = "@" + targetRecord.XRef + "@";
				} else
					if (family.Wife.StringValue == "@" + base.XRef + "@") {
						family.Wife.StringValue = "@" + targetRecord.XRef + "@";
					}

				stf_link.ResetParent(toRec);
				toRec.SpouseToFamilyLinks.Add(stf_link);
			}

			while (this.fIndividualEvents.Count > 0)
			{
				GEDCOMCustomEvent evt = this.fIndividualEvents.Extract(0);
				evt.ResetParent(toRec);
				toRec.AddIndividualEvent(evt);
			}

			while (this.fIndividualOrdinances.Count > 0)
			{
				GEDCOMIndividualOrdinance ord = this.fIndividualOrdinances.Extract(0);
				ord.ResetParent(toRec);
				toRec.IndividualOrdinances.Add(ord);
			}

			while (this.fSubmittors.Count > 0)
			{
                GEDCOMPointer obj = this.fSubmittors.Extract(0);
                obj.ResetParent(toRec);
				toRec.Submittors.Add(obj);
			}

			while (this.fAssociations.Count > 0)
			{
                GEDCOMAssociation obj = this.fAssociations.Extract(0);
                obj.ResetParent(toRec);
				toRec.Associations.Add(obj);
			}

			while (this.fAliasses.Count > 0)
			{
                GEDCOMAlias obj = this.fAliasses.Extract(0);
                obj.ResetParent(toRec);
				toRec.Aliasses.Add(obj);
			}

			while (this.fAncestorsInterest.Count > 0)
			{
                GEDCOMPointer obj = this.fAncestorsInterest.Extract(0);
                obj.ResetParent(toRec);
				toRec.AncestorsInterest.Add(obj);
			}

			while (this.fDescendantsInterest.Count > 0)
			{
                GEDCOMPointer obj = this.fDescendantsInterest.Extract(0);
                obj.ResetParent(toRec);
				toRec.DescendantsInterest.Add(obj);
			}

			while (this.fGroups.Count > 0)
			{
                GEDCOMPointer obj = this.fGroups.Extract(0);
                obj.ResetParent(toRec);
				toRec.Groups.Add(obj);
			}
		}

		public override void Pack()
		{
			base.Pack();

			this.fPersonalNames.Pack();
			this.fChildToFamilyLinks.Pack();
			this.fSpouseToFamilyLinks.Pack();
			this.fIndividualEvents.Pack();
			this.fIndividualOrdinances.Pack();
			this.fSubmittors.Pack();
			this.fAssociations.Pack();
			this.fAliasses.Pack();
			this.fAncestorsInterest.Pack();
			this.fDescendantsInterest.Pack();
			this.fGroups.Pack();
		}

        public override void ReplaceXRefs(XRefReplacer map)
		{
            base.ReplaceXRefs(map);

            this.fPersonalNames.ReplaceXRefs(map);
            this.fChildToFamilyLinks.ReplaceXRefs(map);
            this.fSpouseToFamilyLinks.ReplaceXRefs(map);
            this.fIndividualEvents.ReplaceXRefs(map);
            this.fIndividualOrdinances.ReplaceXRefs(map);
            this.fSubmittors.ReplaceXRefs(map);
            this.fAssociations.ReplaceXRefs(map);
            this.fAliasses.ReplaceXRefs(map);
            this.fAncestorsInterest.ReplaceXRefs(map);
            this.fDescendantsInterest.ReplaceXRefs(map);
            this.fGroups.ReplaceXRefs(map);
		}

		public override void ResetOwner(GEDCOMTree newOwner)
		{
			base.ResetOwner(newOwner);

			this.fPersonalNames.ResetOwner(newOwner);
			this.fChildToFamilyLinks.ResetOwner(newOwner);
			this.fSpouseToFamilyLinks.ResetOwner(newOwner);
			this.fIndividualEvents.ResetOwner(newOwner);
			this.fIndividualOrdinances.ResetOwner(newOwner);
			this.fSubmittors.ResetOwner(newOwner);
			this.fAssociations.ResetOwner(newOwner);
			this.fAliasses.ResetOwner(newOwner);
			this.fAncestorsInterest.ResetOwner(newOwner);
			this.fDescendantsInterest.ResetOwner(newOwner);
			this.fGroups.ResetOwner(newOwner);
		}

		public override void SaveToStream(StreamWriter stream)
		{
			base.SaveToStream(stream);

			this.fPersonalNames.SaveToStream(stream);
			this.fChildToFamilyLinks.SaveToStream(stream);
			this.fSpouseToFamilyLinks.SaveToStream(stream);
			this.fIndividualEvents.SaveToStream(stream);
			this.fIndividualOrdinances.SaveToStream(stream);
			this.fSubmittors.SaveToStream(stream);
			this.fAssociations.SaveToStream(stream);
			this.fAliasses.SaveToStream(stream);
			this.fAncestorsInterest.SaveToStream(stream);
			this.fDescendantsInterest.SaveToStream(stream);
			this.fGroups.SaveToStream(stream);
		}

		public GEDCOMIndividualRecord(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static GEDCOMTag Create(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
		{
			return new GEDCOMIndividualRecord(owner, parent, tagName, tagValue);
		}

        #region Auxiliary

        public void aux_GetLifeDates(out GEDCOMCustomEvent birthEvent, out GEDCOMCustomEvent deathEvent)
		{
			birthEvent = null;
			deathEvent = null;

			int num = this.IndividualEvents.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				GEDCOMCustomEvent ev = this.IndividualEvents[i];
				if (ev.Name == "BIRT")
				{
					birthEvent = ev;
				}
				else
				{
					if (ev.Name == "DEAT")
					{
						deathEvent = ev;
					}
				}
			}
		}

		public void aux_GetNameParts(out string surname, out string name, out string patronymic)
		{
			if (this.fPersonalNames.Count > 0)
			{
				GEDCOMPersonalName np = this.fPersonalNames[0];

				string firstPart /*, dummy*/;
				np.GetNameParts(out firstPart, out surname /*, out dummy*/);

				string[] parts = firstPart.Split(' ');
				if (parts.Length > 1)
				{
					name = parts[0];
					patronymic = parts[1];
				} else {
					name = firstPart;
					patronymic = "";
				}
			} else {
				surname = "";
				name = "";
				patronymic = "";
			}
		}

		public void aux_GetParents(out GEDCOMIndividualRecord father, out GEDCOMIndividualRecord mother)
		{
			GEDCOMFamilyRecord fam = (this.ChildToFamilyLinks.Count < 1) ? null : this.ChildToFamilyLinks[0].Value as GEDCOMFamilyRecord;

			if (fam == null) {
				father = null;
				mother = null;
			} else {
				father = fam.Husband.Value as GEDCOMIndividualRecord;
				mother = fam.Wife.Value as GEDCOMIndividualRecord;
			}
		}

		/*public string aux_GetNameStr(bool aByFamily, bool aPieces)
		{
			
		}*/
		
		public string aux_GetNameStr(bool aByFamily, bool aPieces)
		{
			string result;
			if (this.fPersonalNames.Count > 0)
			{
				GEDCOMPersonalName np = this.fPersonalNames[0];

				string firstPart, surname/*, dummy*/;
				np.GetNameParts(out firstPart, out surname /*, out dummy*/);

				if (aByFamily)
				{
					result = surname + " " + firstPart;
				}
				else
				{
					result = firstPart + " " + surname;
				}

				if (aPieces)
				{
					string nick = np.Pieces.Nickname;
                    if (!string.IsNullOrEmpty(nick)) result = result + " [" + nick + "]";
				}
			}
			else
			{
				result = "";
			}
			return result;
		}

		public string aux_GetNickStr()
		{
			string result;
			if (this.fPersonalNames.Count > 0)
			{
				GEDCOMPersonalName np = this.fPersonalNames[0];
				result = np.Pieces.Nickname;
			}
			else
			{
				result = "";
			}
			return result;
		}

		private bool aux_GetIndivName(bool rusNames, bool womanMode, ref string aName)
		{
			string firstPart, surname;
			GEDCOMPersonalName np = this.fPersonalNames[0];
			np.GetNameParts(out firstPart, out surname);

			aName = ((womanMode && rusNames) ? firstPart : np.StringValue);
			bool result = (aName.Length > 3);

			// russian names form - with patronymics, and woman marriage families
			if (rusNames) {
				string[] parts = firstPart.Split(' ');
				result = result && ((parts.Length > 1) && (parts[0].Length > 1) && (parts[1].Length > 1));
			}

			return result;
		}

		public override float IsMatch(GEDCOMTag tag, MatchParams matchParams)
		{
			if (tag == null) return 0.0f;

			GEDCOMIndividualRecord indi = tag as GEDCOMIndividualRecord;
			if (this.Sex != indi.Sex) return 0.0f;

			float match = 0.0f;

			// check name
			float nameMatch = 0.0f;
			/*for (int i = 0; i <= indi.PersonalNames.Count - 1; i++)
			{
				for (int k = 0; k <= fPersonalNames.Count - 1; k++)
				{
					float currentNameMatch = fPersonalNames[k].IsMatch(indi.PersonalNames[i]);
					nameMatch = Math.Max(nameMatch, currentNameMatch);
				}
			}*/
			bool womanMode = (this.Sex == GEDCOMSex.svFemale);
			string iName = "";
			string kName = "";
			bool res = this.aux_GetIndivName(matchParams.RusNames, womanMode, ref iName);
			res = res && indi.aux_GetIndivName(matchParams.RusNames, womanMode, ref kName);
			if (res)
			{
				if (matchParams.NamesIndistinctThreshold >= 0.99f) {
					if (iName == kName) {
						nameMatch = 100.0f;
					}
				} else {
					double sim = IndistinctMatching.GetSimilarity(iName, kName);
					if (sim >= matchParams.NamesIndistinctThreshold) {
						nameMatch = 100.0f;
					}
				}
			}

			// 0% name match would be pointless checking other details
			if (nameMatch != 0.0f && matchParams.DatesCheck)
			{
				float matches = 1.0f; // nameMatch

				float birthMatch = 0.0f;
				float deathMatch = 0.0f;

				GEDCOMCustomEvent birth = null;
				GEDCOMCustomEvent death = null;
				GEDCOMCustomEvent indiBirth = null;
				GEDCOMCustomEvent indiDeath = null;

				this.aux_GetLifeDates(out birth, out death);
				indi.aux_GetLifeDates(out indiBirth, out indiDeath);

				if (birth != null && indiBirth != null) {
					birthMatch = birth.IsMatch(indiBirth, matchParams);
					matches++;
				} else if (birth == null && indiBirth == null) {
					birthMatch = 100.0f;
					matches++;
				} else {
					matches++;
				}

				/*if (death != null && indiDeath != null) {
					deathMatch = death.IsMatch(indiDeath, matchParams);
					matches++;
				} else if (death == null && indiDeath == null) {
					deathMatch = 100.0f;
					matches++;
				} else {
					matches++;
				}*/

				match = (nameMatch + birthMatch + deathMatch) / matches;
			} else {
				match = (nameMatch);
			}

			return match;
		}

		public GEDCOMAssociation aux_AddAssociation(string aRel, GEDCOMIndividualRecord aRelPerson)
		{
			GEDCOMAssociation result = new GEDCOMAssociation(this.Owner, this, "", "");
			result.Relation = aRel;
			result.Individual = aRelPerson;
			this.Associations.Add(result);
			return result;
		}

		public GEDCOMMultimediaLink aux_SetPrimaryMultimediaLink(GEDCOMMultimediaRecord mediaRec)
		{
			GEDCOMMultimediaLink mmLink = null;

			int num = this.MultimediaLinks.Count - 1;
			for (int i = 0; i <= num; i++) {
				if (this.MultimediaLinks[i].Value == mediaRec) {
					mmLink = this.MultimediaLinks[i];
					break;
				}
			}

			if (mmLink == null) {
				mmLink = this.AddMultimedia(mediaRec);
			}

			mmLink.IsPrimary = true;
			return mmLink;
		}

		public GEDCOMMultimediaLink aux_GetPrimaryMultimediaLink()
		{
			GEDCOMMultimediaLink result = null;

			int num = this.MultimediaLinks.Count - 1;
			for (int i = 0; i <= num; i++) {
				GEDCOMMultimediaLink mmLink = this.MultimediaLinks[i];
				if (mmLink.IsPrimary) {
					result = mmLink;
					break;
				}
			}

			return result;
		}

		public int aux_GetChildsCount()
		{
			int result = 0;

			if (this.SpouseToFamilyLinks.Count > 0)
			{
				int num = this.SpouseToFamilyLinks.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					GEDCOMFamilyRecord family = this.SpouseToFamilyLinks[i].Family;
					result += family.Childrens.Count;
				}
			}

			return result;
		}

		#endregion
	}
}
