using System;
using System.IO;
using Ext.Utils;

namespace GedCom551
{
	public sealed class TGEDCOMIndividualRecord : TGEDCOMRecord
	{
		private GEDCOMList<TGEDCOMPersonalName> _PersonalNames;
		private GEDCOMList<TGEDCOMCustomEvent> _IndividualEvents;
		private GEDCOMList<TGEDCOMIndividualOrdinance> _IndividualOrdinances;
		private GEDCOMList<TGEDCOMChildToFamilyLink> _ChildToFamilyLinks;
		private GEDCOMList<TGEDCOMSpouseToFamilyLink> _SpouseToFamilyLinks;
		private GEDCOMList<TGEDCOMPointer> _Submittors;
		private GEDCOMList<TGEDCOMAssociation> _Associations;
		private GEDCOMList<TGEDCOMAlias> _Aliasses;
		private GEDCOMList<TGEDCOMPointer> _AncestorsInterest;
		private GEDCOMList<TGEDCOMPointer> _DescendantsInterest;
		private GEDCOMList<TGEDCOMPointer> _Groups;
		private static GEDCOMFactory fTagsFactory;


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
			get { return this._IndividualEvents; }
		}

		public GEDCOMList<TGEDCOMIndividualOrdinance> IndividualOrdinances
		{
			get { return this._IndividualOrdinances; }
		}

		public GEDCOMList<TGEDCOMPersonalName> PersonalNames
		{
			get { return this._PersonalNames; }
		}

		public TGEDCOMRestriction Restriction
		{
			get { return GEDCOMUtils.GetRestrictionVal(base.GetTagStringValue("RESN").Trim().ToUpper()); }
			set { base.SetTagStringValue("RESN", GEDCOMUtils.GetRestrictionStr(value)); }
		}

		public TGEDCOMSex Sex
		{
			get { return GEDCOMUtils.GetSexVal(base.GetTagStringValue("SEX").Trim().ToUpper()); }
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
			get { return this._ChildToFamilyLinks; }
		}

		public GEDCOMList<TGEDCOMSpouseToFamilyLink> SpouseToFamilyLinks
		{
			get { return this._SpouseToFamilyLinks; }
		}

		public GEDCOMList<TGEDCOMPointer> Submittors
		{
			get { return this._Submittors; }
		}

		public GEDCOMList<TGEDCOMAssociation> Associations
		{
			get { return this._Associations; }
		}

		public GEDCOMList<TGEDCOMAlias> Aliasses
		{
			get { return this._Aliasses; }
		}

		public GEDCOMList<TGEDCOMPointer> AncestorsInterest
		{
			get { return this._AncestorsInterest; }
		}

		public GEDCOMList<TGEDCOMPointer> DescendantsInterest
		{
			get { return this._DescendantsInterest; }
		}

		public GEDCOMList<TGEDCOMPointer> Groups
		{
			get { return this._Groups; }
		}

		protected override void CreateObj(TGEDCOMTree owner, TGEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.FRecordType = TGEDCOMRecordType.rtIndividual;
			this.FName = "INDI";

			this._PersonalNames = new GEDCOMList<TGEDCOMPersonalName>(this);
			this._IndividualEvents = new GEDCOMList<TGEDCOMCustomEvent>(this);
			this._IndividualOrdinances = new GEDCOMList<TGEDCOMIndividualOrdinance>(this);
			this._ChildToFamilyLinks = new GEDCOMList<TGEDCOMChildToFamilyLink>(this);
			this._SpouseToFamilyLinks = new GEDCOMList<TGEDCOMSpouseToFamilyLink>(this);
			this._Submittors = new GEDCOMList<TGEDCOMPointer>(this);
			this._Associations = new GEDCOMList<TGEDCOMAssociation>(this);
			this._Aliasses = new GEDCOMList<TGEDCOMAlias>(this);
			this._AncestorsInterest = new GEDCOMList<TGEDCOMPointer>(this);
			this._DescendantsInterest = new GEDCOMList<TGEDCOMPointer>(this);
			this._Groups = new GEDCOMList<TGEDCOMPointer>(this);
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
				result = this._Groups.Add(new TGEDCOMPointer(base.Owner, this, tagName, tagValue));
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

		public int IndexOfGroup(TGEDCOMGroupRecord groupRec)
		{
			int Result = -1;
			if (this._Groups != null)
			{
				int num = this._Groups.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					if (this._Groups[i].XRef == groupRec.XRef)
					{
						Result = i;
						break;
					}
				}
			}
			return Result;
		}

		public int IndexOfSpouse(TGEDCOMFamilyRecord familyRec)
		{
			int Result = -1;
			if (this._SpouseToFamilyLinks != null)
			{
				int num = this._SpouseToFamilyLinks.Count - 1;
				for (int i = 0; i <= num; i++)
				{
					if (this._SpouseToFamilyLinks[i].Family == familyRec)
					{
						Result = i;
						break;
					}
				}
			}
			return Result;
		}

		public void DeleteSpouseToFamilyLink(TGEDCOMFamilyRecord familyRec)
		{
			int num = this._SpouseToFamilyLinks.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				if (this._SpouseToFamilyLinks[i].Family == familyRec)
				{
					this._SpouseToFamilyLinks.Delete(i);
					break;
				}
			}
		}

		public void DeleteChildToFamilyLink(TGEDCOMFamilyRecord familyRec)
		{
			int num = this._ChildToFamilyLinks.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				if (this._ChildToFamilyLinks[i].Family == familyRec)
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

		public TGEDCOMCustomEvent GetIndividualEvent(string eventName)
		{
			TGEDCOMCustomEvent Result = null;

			TGEDCOMCustomEvent evt;
			int num = this._IndividualEvents.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				evt = this._IndividualEvents[i];
				if (evt.Name == eventName) {
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

		public override void MoveTo(TGEDCOMRecord aToRecord, bool clearDest)
		{
			if (!clearDest)
			{
				base.DeleteTag("SEX");
				base.DeleteTag("_UID");
			}

			base.MoveTo(aToRecord, clearDest);

			TGEDCOMIndividualRecord toRec = aToRecord as TGEDCOMIndividualRecord;

			if (this._PersonalNames != null && clearDest)
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
                (obj as TGEDCOMTag).ResetParent(toRec);
				toRec.Submittors.Add(obj as TGEDCOMPointer);
			}

			while (this._Associations.Count > 0)
			{
				TGEDCOMObject obj = this._Associations.Extract(0);
                (obj as TGEDCOMTag).ResetParent(toRec);
				toRec.Associations.Add(obj as TGEDCOMAssociation);
			}

			while (this._Aliasses.Count > 0)
			{
				TGEDCOMObject obj = this._Aliasses.Extract(0);
                (obj as TGEDCOMTag).ResetParent(toRec);
				toRec.Aliasses.Add(obj as TGEDCOMAlias);
			}

			while (this._AncestorsInterest.Count > 0)
			{
				TGEDCOMObject obj = this._AncestorsInterest.Extract(0);
                (obj as TGEDCOMTag).ResetParent(toRec);
				toRec.AncestorsInterest.Add(obj as TGEDCOMPointer);
			}

			while (this._DescendantsInterest.Count > 0)
			{
				TGEDCOMObject obj = this._DescendantsInterest.Extract(0);
                (obj as TGEDCOMTag).ResetParent(toRec);
				toRec.DescendantsInterest.Add(obj as TGEDCOMPointer);
			}

			while (this._Groups.Count > 0)
			{
				TGEDCOMObject obj = this._Groups.Extract(0);
                (obj as TGEDCOMTag).ResetParent(toRec);
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

		public override void ReplaceXRefs(XRefReplacer aMap)
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

		public override void ResetOwner(TGEDCOMTree owner)
		{
			base.ResetOwner(owner);

			this._PersonalNames.ResetOwner(owner);
			this._ChildToFamilyLinks.ResetOwner(owner);
			this._SpouseToFamilyLinks.ResetOwner(owner);
			this._IndividualEvents.ResetOwner(owner);
			this._IndividualOrdinances.ResetOwner(owner);
			this._Submittors.ResetOwner(owner);
			this._Associations.ResetOwner(owner);
			this._Aliasses.ResetOwner(owner);
			this._AncestorsInterest.ResetOwner(owner);
			this._DescendantsInterest.ResetOwner(owner);
			this._Groups.ResetOwner(owner);
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

		public TGEDCOMIndividualRecord(TGEDCOMTree owner, TGEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static TGEDCOMTag Create(TGEDCOMTree owner, TGEDCOMObject parent, string tagName, string tagValue)
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
			if (this._PersonalNames.Count > 0)
			{
				TGEDCOMPersonalName np = this._PersonalNames[0];

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
                    if (!string.IsNullOrEmpty(nick)) Result = Result + " [" + nick + "]";
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

		private bool aux_GetIndivName(bool rusNames, bool womanMode, ref string aName)
		{
			bool result;

			string firstPart, surname;
			TGEDCOMPersonalName np = this._PersonalNames[0];
			np.GetNameParts(out firstPart, out surname);

			aName = ((womanMode && rusNames) ? firstPart : np.StringValue);
			result = (aName.Length > 3);

			// russian names form - with patronymics, and woman marriage families
			if (rusNames) {
				string[] parts = firstPart.Split(' ');
				result = result && ((parts.Length > 1) && (parts[0].Length > 1) && (parts[1].Length > 1));
			}

			return result;
		}

		public override bool aux_IsMatch(TGEDCOMRecord record, float matchThreshold, MatchParams mParams)
		{
			bool match = false;

			if (record != null) {
				TGEDCOMIndividualRecord kRec = record as TGEDCOMIndividualRecord;

				if (this.Sex == kRec.Sex) {
					bool womanMode = (this.Sex == TGEDCOMSex.svFemale);
					string iName = "";
					string kName = "";

					if (this.aux_GetIndivName(mParams.RusNames, womanMode, ref iName) && kRec.aux_GetIndivName(mParams.RusNames, womanMode, ref kName)) {
						if (!mParams.IndistinctNameMatching) {
							match = (iName == kName);
						} else {
							match = (IndistinctMatching.GetSimilarity(iName, kName) >= mParams.IndistinctThreshold);
						}

						if (match && mParams.CheckBirthYear)
						{
							TGEDCOMCustomEvent ev;
							int year1 = -1, year2 = -1;
							ushort m, d;

							ev = this.GetIndividualEvent("BIRT");
							if (ev != null) ev.Detail.Date.aux_GetIndependentDate(out year1, out m, out d);

							ev = kRec.GetIndividualEvent("BIRT");
							if (ev != null) ev.Detail.Date.aux_GetIndependentDate(out year2, out m, out d);

							match = (match && year1 >= 0 && year2 >= 0 && Math.Abs(year1 - year2) <= mParams.YearInaccuracy);
						}
					}
				}
			}

			return match;

			/*float match = 0.0F;

			TGEDCOMIndividualRecord indi = record as TGEDCOMIndividualRecord;

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

			return (match >= matchThreshold);*/
		}

		public TGEDCOMAssociation aux_AddAssociation(string aRel, TGEDCOMIndividualRecord aRelPerson)
		{
			TGEDCOMAssociation Result = new TGEDCOMAssociation(this.Owner, this, "", "");
			Result.Relation = aRel;
			Result.Individual = aRelPerson;
			this.Associations.Add(Result);
			return Result;
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

		#endregion
	}
}
