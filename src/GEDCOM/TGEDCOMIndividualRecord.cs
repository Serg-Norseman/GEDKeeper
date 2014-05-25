using System;
using System.IO;
using ExtUtils;

namespace GedCom551
{
	public sealed class TGEDCOMIndividualRecord : TGEDCOMRecord
	{
        private static readonly GEDCOMFactory fTagsFactory;
        
        private GEDCOMList<TGEDCOMPersonalName> fPersonalNames;
		private GEDCOMList<TGEDCOMCustomEvent> fIndividualEvents;
		private GEDCOMList<TGEDCOMIndividualOrdinance> fIndividualOrdinances;
		private GEDCOMList<TGEDCOMChildToFamilyLink> fChildToFamilyLinks;
		private GEDCOMList<TGEDCOMSpouseToFamilyLink> fSpouseToFamilyLinks;
		private GEDCOMList<TGEDCOMPointer> fSubmittors;
		private GEDCOMList<TGEDCOMAssociation> fAssociations;
		private GEDCOMList<TGEDCOMAlias> fAliasses;
		private GEDCOMList<TGEDCOMPointer> fAncestorsInterest;
		private GEDCOMList<TGEDCOMPointer> fDescendantsInterest;
		private GEDCOMList<TGEDCOMPointer> fGroups;


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

		public GEDCOMList<TGEDCOMCustomEvent> IndividualEvents
		{
			get { return this.fIndividualEvents; }
		}

		public GEDCOMList<TGEDCOMIndividualOrdinance> IndividualOrdinances
		{
			get { return this.fIndividualOrdinances; }
		}

		public GEDCOMList<TGEDCOMPersonalName> PersonalNames
		{
			get { return this.fPersonalNames; }
		}

		public TGEDCOMRestriction Restriction
		{
			get { return GEDCOMUtils.GetRestrictionVal(base.GetTagStringValue("RESN")); }
			set { base.SetTagStringValue("RESN", GEDCOMUtils.GetRestrictionStr(value)); }
		}

		public TGEDCOMSex Sex
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

		public GEDCOMList<TGEDCOMChildToFamilyLink> ChildToFamilyLinks
		{
			get { return this.fChildToFamilyLinks; }
		}

		public GEDCOMList<TGEDCOMSpouseToFamilyLink> SpouseToFamilyLinks
		{
			get { return this.fSpouseToFamilyLinks; }
		}

		public GEDCOMList<TGEDCOMPointer> Submittors
		{
			get { return this.fSubmittors; }
		}

		public GEDCOMList<TGEDCOMAssociation> Associations
		{
			get { return this.fAssociations; }
		}

		public GEDCOMList<TGEDCOMAlias> Aliasses
		{
			get { return this.fAliasses; }
		}

		public GEDCOMList<TGEDCOMPointer> AncestorsInterest
		{
			get { return this.fAncestorsInterest; }
		}

		public GEDCOMList<TGEDCOMPointer> DescendantsInterest
		{
			get { return this.fDescendantsInterest; }
		}

		public GEDCOMList<TGEDCOMPointer> Groups
		{
			get { return this.fGroups; }
		}

		protected override void CreateObj(TGEDCOMTree owner, GEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.fRecordType = TGEDCOMRecordType.rtIndividual;
			this.fName = "INDI";

			this.fPersonalNames = new GEDCOMList<TGEDCOMPersonalName>(this);
			this.fIndividualEvents = new GEDCOMList<TGEDCOMCustomEvent>(this);
			this.fIndividualOrdinances = new GEDCOMList<TGEDCOMIndividualOrdinance>(this);
			this.fChildToFamilyLinks = new GEDCOMList<TGEDCOMChildToFamilyLink>(this);
			this.fSpouseToFamilyLinks = new GEDCOMList<TGEDCOMSpouseToFamilyLink>(this);
			this.fSubmittors = new GEDCOMList<TGEDCOMPointer>(this);
			this.fAssociations = new GEDCOMList<TGEDCOMAssociation>(this);
			this.fAliasses = new GEDCOMList<TGEDCOMAlias>(this);
			this.fAncestorsInterest = new GEDCOMList<TGEDCOMPointer>(this);
			this.fDescendantsInterest = new GEDCOMList<TGEDCOMPointer>(this);
			this.fGroups = new GEDCOMList<TGEDCOMPointer>(this);
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

		static TGEDCOMIndividualRecord()
		{
			GEDCOMFactory f = new GEDCOMFactory();
			fTagsFactory = f;

			//f.RegisterTag("xxxx", xxxxxx.Create);
			
			f.RegisterTag("FAMC", TGEDCOMChildToFamilyLink.Create);
			f.RegisterTag("FAMS", TGEDCOMSpouseToFamilyLink.Create);
			f.RegisterTag("ASSO", TGEDCOMAssociation.Create);
			f.RegisterTag("ALIA", TGEDCOMAlias.Create);
			
			f.RegisterTag("BAPL", TGEDCOMIndividualOrdinance.Create);
			f.RegisterTag("CONL", TGEDCOMIndividualOrdinance.Create);
			f.RegisterTag("ENDL", TGEDCOMIndividualOrdinance.Create);
			f.RegisterTag("SLGC", TGEDCOMIndividualOrdinance.Create);
			
			// // //

			f.RegisterTag("BIRT", TGEDCOMIndividualEvent.Create);
			f.RegisterTag("CHR", TGEDCOMIndividualEvent.Create);
			f.RegisterTag("DEAT", TGEDCOMIndividualEvent.Create);
			f.RegisterTag("BURI", TGEDCOMIndividualEvent.Create);
			f.RegisterTag("CREM", TGEDCOMIndividualEvent.Create);
			
			f.RegisterTag("ADOP", TGEDCOMIndividualEvent.Create);
			f.RegisterTag("BAPM", TGEDCOMIndividualEvent.Create);
			f.RegisterTag("BARM", TGEDCOMIndividualEvent.Create);
			f.RegisterTag("BASM", TGEDCOMIndividualEvent.Create);
			f.RegisterTag("BLES", TGEDCOMIndividualEvent.Create);

	        f.RegisterTag("CHRA", TGEDCOMIndividualEvent.Create);
			f.RegisterTag("CONF", TGEDCOMIndividualEvent.Create);
			f.RegisterTag("FCOM", TGEDCOMIndividualEvent.Create);
			f.RegisterTag("ORDN", TGEDCOMIndividualEvent.Create);
			f.RegisterTag("NATU", TGEDCOMIndividualEvent.Create);

			f.RegisterTag("EMIG", TGEDCOMIndividualEvent.Create);
			f.RegisterTag("IMMI", TGEDCOMIndividualEvent.Create);
			f.RegisterTag("CENS", TGEDCOMIndividualEvent.Create);
			f.RegisterTag("PROB", TGEDCOMIndividualEvent.Create);
			f.RegisterTag("WILL", TGEDCOMIndividualEvent.Create);

			f.RegisterTag("GRAD", TGEDCOMIndividualEvent.Create);
			f.RegisterTag("RETI", TGEDCOMIndividualEvent.Create);
			f.RegisterTag("EVEN", TGEDCOMIndividualEvent.Create);


			f.RegisterTag("CAST", TGEDCOMIndividualAttribute.Create);
			f.RegisterTag("DSCR", TGEDCOMIndividualAttribute.Create);
			f.RegisterTag("EDUC", TGEDCOMIndividualAttribute.Create);
			f.RegisterTag("IDNO", TGEDCOMIndividualAttribute.Create);
			f.RegisterTag("NATI", TGEDCOMIndividualAttribute.Create);

			f.RegisterTag("NCHI", TGEDCOMIndividualAttribute.Create);
			f.RegisterTag("NMR", TGEDCOMIndividualAttribute.Create);
			f.RegisterTag("OCCU", TGEDCOMIndividualAttribute.Create);
			f.RegisterTag("PROP", TGEDCOMIndividualAttribute.Create);
			f.RegisterTag("RELI", TGEDCOMIndividualAttribute.Create);

			f.RegisterTag("RESI", TGEDCOMIndividualAttribute.Create);
			f.RegisterTag("SSN", TGEDCOMIndividualAttribute.Create);
			f.RegisterTag("TITL", TGEDCOMIndividualAttribute.Create);
			f.RegisterTag("FACT", TGEDCOMIndividualAttribute.Create);
			f.RegisterTag("_TRAVEL", TGEDCOMIndividualAttribute.Create);

			f.RegisterTag("_HOBBY", TGEDCOMIndividualAttribute.Create);
			f.RegisterTag("_AWARD", TGEDCOMIndividualAttribute.Create);
			f.RegisterTag("_MILI", TGEDCOMIndividualAttribute.Create);
			f.RegisterTag("_MILI_IND", TGEDCOMIndividualAttribute.Create);
			f.RegisterTag("_MILI_DIS", TGEDCOMIndividualAttribute.Create);
			f.RegisterTag("_MILI_RANK", TGEDCOMIndividualAttribute.Create);

			//f.RegisterTag("_BGRO", TGEDCOMBloodGroup.Create);
		}

		public override TGEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
		{
			TGEDCOMTag result;

			if (tagName == "NAME")
			{
				result = this.AddPersonalName(new TGEDCOMPersonalName(base.Owner, this, tagName, tagValue));
			}
			else if (tagName == "SUBM")
			{
				result = this.Submittors.Add(new TGEDCOMPointer(base.Owner, this, tagName, tagValue));
			}
			else if (tagName == "ANCI")
			{
				result = this.AncestorsInterest.Add(new TGEDCOMPointer(base.Owner, this, tagName, tagValue));
			}
			else if (tagName == "DESI")
			{
				result = this.DescendantsInterest.Add(new TGEDCOMPointer(base.Owner, this, tagName, tagValue));
			}
			else if (tagName == "_GROUP")
			{
				result = this.fGroups.Add(new TGEDCOMPointer(base.Owner, this, tagName, tagValue));
			}
			else
			{
				result = fTagsFactory.CreateTag(this.Owner, this, tagName, tagValue);

				if (result != null)
				{
					if (result is TGEDCOMChildToFamilyLink) {
						result = this.ChildToFamilyLinks.Add(result as TGEDCOMChildToFamilyLink);
					} else if (result is TGEDCOMSpouseToFamilyLink) {
						result = this.SpouseToFamilyLinks.Add(result as TGEDCOMSpouseToFamilyLink);
					} else if (result is TGEDCOMIndividualOrdinance) {
						result = this.IndividualOrdinances.Add(result as TGEDCOMIndividualOrdinance);
					} else if (result is TGEDCOMAssociation) {
						result = this.Associations.Add(result as TGEDCOMAssociation);
					} else if (result is TGEDCOMIndividualEvent) {
						result = this.AddIndividualEvent(result as TGEDCOMCustomEvent);
					} else if (result is TGEDCOMIndividualAttribute) {
						result = this.AddIndividualEvent(result as TGEDCOMCustomEvent);
					} else if (result is TGEDCOMAlias) {
						result = this.Aliasses.Add(result as TGEDCOMAlias);
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

		public TGEDCOMCustomEvent AddIndividualEvent(TGEDCOMCustomEvent value)
		{
			if (value != null)
			{
				value.SetLevel(base.Level + 1);
				this.fIndividualEvents.Add(value);
			}

			return value;
		}

		public TGEDCOMPersonalName AddPersonalName(TGEDCOMPersonalName value)
		{
			if (value != null)
			{
				value.SetLevel(base.Level + 1);
				this.fPersonalNames.Add(value);
			}
			return value;
		}

		public int IndexOfGroup(TGEDCOMGroupRecord groupRec)
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

		public int IndexOfSpouse(TGEDCOMFamilyRecord familyRec)
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

		public void DeleteSpouseToFamilyLink(TGEDCOMFamilyRecord familyRec)
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

		public void DeleteChildToFamilyLink(TGEDCOMFamilyRecord familyRec)
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

		public TGEDCOMCustomEvent GetIndividualEvent(string eventName)
		{
			TGEDCOMCustomEvent result = null;

			int num = this.fIndividualEvents.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				TGEDCOMCustomEvent evt = this.fIndividualEvents[i];
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

		public override void MoveTo(TGEDCOMRecord targetRecord, bool clearDest)
		{
			if (!clearDest)
			{
				base.DeleteTag("SEX");
				base.DeleteTag("_UID");
			}

			base.MoveTo(targetRecord, clearDest);

			TGEDCOMIndividualRecord toRec = targetRecord as TGEDCOMIndividualRecord;

			if (this.fPersonalNames != null && clearDest)
			{
				while (this.fPersonalNames.Count > 0)
				{
                    TGEDCOMPersonalName obj = this.fPersonalNames.Extract(0);
                    obj.ResetParent(toRec);
					toRec.AddPersonalName(obj);
				}
			}

			if (toRec.ChildToFamilyLinks.Count == 0 && this.ChildToFamilyLinks.Count != 0 && this.fChildToFamilyLinks != null)
			{
				TGEDCOMChildToFamilyLink ctf_link = this.fChildToFamilyLinks.Extract(0);
				TGEDCOMFamilyRecord family = ctf_link.Family;

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
				TGEDCOMSpouseToFamilyLink stf_link = this.fSpouseToFamilyLinks.Extract(0);
				TGEDCOMFamilyRecord family = stf_link.Family;

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
				TGEDCOMCustomEvent evt = this.fIndividualEvents.Extract(0);
				evt.ResetParent(toRec);
				toRec.AddIndividualEvent(evt);
			}

			while (this.fIndividualOrdinances.Count > 0)
			{
				TGEDCOMIndividualOrdinance ord = this.fIndividualOrdinances.Extract(0);
				ord.ResetParent(toRec);
				toRec.IndividualOrdinances.Add(ord);
			}

			while (this.fSubmittors.Count > 0)
			{
                TGEDCOMPointer obj = this.fSubmittors.Extract(0);
                obj.ResetParent(toRec);
				toRec.Submittors.Add(obj);
			}

			while (this.fAssociations.Count > 0)
			{
                TGEDCOMAssociation obj = this.fAssociations.Extract(0);
                obj.ResetParent(toRec);
				toRec.Associations.Add(obj);
			}

			while (this.fAliasses.Count > 0)
			{
                TGEDCOMAlias obj = this.fAliasses.Extract(0);
                obj.ResetParent(toRec);
				toRec.Aliasses.Add(obj);
			}

			while (this.fAncestorsInterest.Count > 0)
			{
                TGEDCOMPointer obj = this.fAncestorsInterest.Extract(0);
                obj.ResetParent(toRec);
				toRec.AncestorsInterest.Add(obj);
			}

			while (this.fDescendantsInterest.Count > 0)
			{
                TGEDCOMPointer obj = this.fDescendantsInterest.Extract(0);
                obj.ResetParent(toRec);
				toRec.DescendantsInterest.Add(obj);
			}

			while (this.fGroups.Count > 0)
			{
                TGEDCOMPointer obj = this.fGroups.Extract(0);
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

		public override void ResetOwner(TGEDCOMTree newOwner)
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

		public TGEDCOMIndividualRecord(TGEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static TGEDCOMTag Create(TGEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
		{
			return new TGEDCOMIndividualRecord(owner, parent, tagName, tagValue);
		}

        #region Auxiliary

        public void aux_GetLifeDates(out TGEDCOMCustomEvent birthEvent, out TGEDCOMCustomEvent deathEvent)
		{
			birthEvent = null;
			deathEvent = null;

			int num = this.IndividualEvents.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				TGEDCOMCustomEvent ev = this.IndividualEvents[i];
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
				TGEDCOMPersonalName np = this.fPersonalNames[0];

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
			string result;
			if (this.fPersonalNames.Count > 0)
			{
				TGEDCOMPersonalName np = this.fPersonalNames[0];

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
				TGEDCOMPersonalName np = this.fPersonalNames[0];
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
			TGEDCOMPersonalName np = this.fPersonalNames[0];
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

		public override float IsMatch(TGEDCOMTag tag, MatchParams matchParams)
		{
			if (tag == null) return 0.0f;

			TGEDCOMIndividualRecord indi = tag as TGEDCOMIndividualRecord;
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
			bool womanMode = (this.Sex == TGEDCOMSex.svFemale);
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

				TGEDCOMCustomEvent birth = null;
				TGEDCOMCustomEvent death = null;
				TGEDCOMCustomEvent indiBirth = null;
				TGEDCOMCustomEvent indiDeath = null;

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

		public TGEDCOMAssociation aux_AddAssociation(string aRel, TGEDCOMIndividualRecord aRelPerson)
		{
			TGEDCOMAssociation result = new TGEDCOMAssociation(this.Owner, this, "", "");
			result.Relation = aRel;
			result.Individual = aRelPerson;
			this.Associations.Add(result);
			return result;
		}

		public TGEDCOMMultimediaLink aux_SetPrimaryMultimediaLink(TGEDCOMMultimediaRecord mediaRec)
		{
			TGEDCOMMultimediaLink mmLink = null;

			int num = this.MultimediaLinks.Count - 1;
			for (int i = 0; i <= num; i++) {
				if (this.MultimediaLinks[i].Value == mediaRec) {
					mmLink = this.MultimediaLinks[i];
					break;
				}
			}

			if (mmLink == null) {
				mmLink = this.aux_AddMultimedia(mediaRec);
			}

			mmLink.IsPrimary = true;
			return mmLink;
		}

		public TGEDCOMMultimediaLink aux_GetPrimaryMultimediaLink()
		{
			TGEDCOMMultimediaLink result = null;

			int num = this.MultimediaLinks.Count - 1;
			for (int i = 0; i <= num; i++) {
				TGEDCOMMultimediaLink mmLink = this.MultimediaLinks[i];
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
					TGEDCOMFamilyRecord family = this.SpouseToFamilyLinks[i].Family;
					result += family.Childrens.Count;
				}
			}

			return result;
		}

		#endregion
	}
}
