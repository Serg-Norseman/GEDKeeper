using System;
using System.IO;
using ExtUtils;
using GKCommon.GEDCOM.Enums;

namespace GKCommon.GEDCOM
{
	public sealed class GEDCOMFamilyRecord : GEDCOMRecordWithEvents
	{
        private static readonly GEDCOMFactory fTagsFactory;
        
		private GEDCOMList<GEDCOMPointer> fChildrens;
		private GEDCOMList<GEDCOMSpouseSealing> fSpouseSealings;

		public GEDCOMList<GEDCOMPointer> Childrens
		{
			get { return this.fChildrens; }
		}

		public GEDCOMPointer Husband
		{
			get { return base.TagClass("HUSB", GEDCOMPointer.Create) as GEDCOMPointer; }
		}

		public GEDCOMPointer Wife
		{
			get { return base.TagClass("WIFE", GEDCOMPointer.Create) as GEDCOMPointer; }
		}

		public GEDCOMPointer Submitter
		{
			get { return base.TagClass("SUBM", GEDCOMPointer.Create) as GEDCOMPointer; }
		}

		public GEDCOMRestriction Restriction
		{
			get { return GEDCOMUtils.GetRestrictionVal(base.GetTagStringValue("RESN")); }
			set { base.SetTagStringValue("RESN", GEDCOMUtils.GetRestrictionStr(value)); }
		}

		public GEDCOMList<GEDCOMSpouseSealing> SpouseSealings
		{
			get { return this.fSpouseSealings; }
		}

		protected override void CreateObj(GEDCOMTree owner, GEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.fRecordType = GEDCOMRecordType.rtFamily;
			this.fName = "FAM";

			this.fChildrens = new GEDCOMList<GEDCOMPointer>(this);
			this.fSpouseSealings = new GEDCOMList<GEDCOMSpouseSealing>(this);
		}

		protected override void Dispose(bool disposing)
		{
			if (disposing)
			{
				this.fChildrens.Dispose();
				this.fSpouseSealings.Dispose();
			}
			base.Dispose(disposing);
		}

		public GEDCOMIndividualRecord GetHusband()
		{
			return this.Husband.Value as GEDCOMIndividualRecord;
		}

		public GEDCOMIndividualRecord GetWife()
		{
			return this.Wife.Value as GEDCOMIndividualRecord;
		}

		static GEDCOMFamilyRecord()
		{
			GEDCOMFactory f = new GEDCOMFactory();
			fTagsFactory = f;

			f.RegisterTag("SLGS", GEDCOMSpouseSealing.Create);

			f.RegisterTag("ANUL", GEDCOMFamilyEvent.Create);
			f.RegisterTag("CENS", GEDCOMFamilyEvent.Create);
			f.RegisterTag("DIV", GEDCOMFamilyEvent.Create);
			f.RegisterTag("DIVF", GEDCOMFamilyEvent.Create);
			f.RegisterTag("ENGA", GEDCOMFamilyEvent.Create);
			f.RegisterTag("MARB", GEDCOMFamilyEvent.Create);
			f.RegisterTag("MARC", GEDCOMFamilyEvent.Create);
			f.RegisterTag("MARR", GEDCOMFamilyEvent.Create);
			f.RegisterTag("MARL", GEDCOMFamilyEvent.Create);
			f.RegisterTag("MARS", GEDCOMFamilyEvent.Create);
			f.RegisterTag("RESI", GEDCOMFamilyEvent.Create);
			f.RegisterTag("EVEN", GEDCOMFamilyEvent.Create);
		}

		public override GEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
		{
			GEDCOMTag result;

			if (tagName == "HUSB" || tagName == "WIFE")
			{
				result = base.AddTag(tagName, tagValue, GEDCOMPointer.Create);
			}
			else if (tagName == "CHIL")
			{
				result = this.fChildrens.Add(new GEDCOMPointer(base.Owner, this, tagName, tagValue));
			}
			else
			{
				result = fTagsFactory.CreateTag(this.Owner, this, tagName, tagValue);

				if (result != null)
				{
					if (result is GEDCOMFamilyEvent) {
						result = this.AddEvent(result as GEDCOMFamilyEvent);
					} else if (result is GEDCOMSpouseSealing) {
						result = this.fSpouseSealings.Add(result as GEDCOMSpouseSealing);
					}
				} else {
					result = base.AddTag(tagName, tagValue, tagConstructor);
				}
			}

			return result;
		}

		public override GEDCOMCustomEvent AddEvent(GEDCOMCustomEvent evt)
		{
			if (evt != null) {
				if (evt is GEDCOMFamilyEvent) {
					// SetLevel need for events created outside!
					evt.SetLevel(this.Level + 1);
					this.Events.Add(evt);
				} else {
					throw new ArgumentException("Event has the invalid type", "evt");
				}
			}

			return evt;
		}

		public override void Clear()
		{
			base.Clear();

			this.fChildrens.Clear();
			this.fSpouseSealings.Clear();
		}

		public override bool IsEmpty()
		{
			return base.IsEmpty() && this.fChildrens.Count == 0 && this.fSpouseSealings.Count == 0;
		}

		public void DeleteChild(GEDCOMRecord childRec)
		{
			for (int i = this.fChildrens.Count - 1; i >= 0; i--)
			{
				if (this.fChildrens[i].Value == childRec)
				{
					this.fChildrens.DeleteAt(i);
					break;
				}
			}
		}

		public int IndexOfChild(GEDCOMRecord childRec)
		{
			int result = -1;

			for (int i = this.fChildrens.Count - 1; i >= 0; i--)
			{
				if (this.fChildrens[i].Value == childRec)
				{
					result = i;
					break;
				}
			}

			return result;
		}

        public override void MoveTo(GEDCOMRecord targetRecord, bool clearDest)
		{
            base.MoveTo(targetRecord, clearDest);

			GEDCOMFamilyRecord targetFamily = targetRecord as GEDCOMFamilyRecord;

			while (this.fChildrens.Count > 0)
			{
                GEDCOMPointer obj = this.fChildrens.Extract(0);
                obj.ResetParent(targetFamily);
				targetFamily.Childrens.Add(obj);
			}

			while (this.fSpouseSealings.Count > 0)
			{
                GEDCOMSpouseSealing obj = this.fSpouseSealings.Extract(0);
                obj.ResetParent(targetFamily);
				targetFamily.SpouseSealings.Add(obj);
			}
		}

		public override void Pack()
		{
			base.Pack();

			this.fChildrens.Pack();
			this.fSpouseSealings.Pack();
		}

        public override void ReplaceXRefs(XRefReplacer map)
		{
            base.ReplaceXRefs(map);

			if (this.Husband != null) {
                this.Husband.StringValue = GEDCOMUtils.EncloseXRef(map.FindNewXRef(this.Husband.StringValue));
			}

            if (this.Wife != null) {
                this.Wife.StringValue = GEDCOMUtils.EncloseXRef(map.FindNewXRef(this.Wife.StringValue));
			}

            this.fChildrens.ReplaceXRefs(map);
            this.fSpouseSealings.ReplaceXRefs(map);
		}

		public override void ResetOwner(GEDCOMTree newOwner)
		{
			base.ResetOwner(newOwner);

			this.fChildrens.ResetOwner(newOwner);
			this.fSpouseSealings.ResetOwner(newOwner);
		}

		public override void SaveToStream(StreamWriter stream)
		{
			base.SaveToStream(stream);

			this.fChildrens.SaveToStream(stream);
			this.Events.SaveToStream(stream); // for files content compatibility
			this.fSpouseSealings.SaveToStream(stream);
		}

		public GEDCOMFamilyRecord(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static GEDCOMTag Create(GEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
		{
			return new GEDCOMFamilyRecord(owner, parent, tagName, tagValue);
		}

		public override float IsMatch(GEDCOMTag tag, MatchParams matchParams)
		{
			if (tag == null) return 0.0f;
			float match = 0.0f;

			GEDCOMFamilyRecord fam = tag as GEDCOMFamilyRecord;
			string title1 = this.GetFamilyString(null, null);
			string title2 = fam.GetFamilyString(null, null);
			if (string.Compare(title1, title2, true) == 0) {
				match = 100.0f;
			}

			return match;
		}

        #region Auxiliary

		public void SortChilds()
		{
			int num = this.fChildrens.Count;
			for (int i = 0; i < num; i++)
			{
				for (int j = i + 1; j < num; j++)
				{
					GEDCOMIndividualRecord iChild = this.fChildrens[i].Value as GEDCOMIndividualRecord;
					AbsDate iDate = GEDCOMUtils.GetAbstractDate(iChild, "BIRT");

					GEDCOMIndividualRecord kChild = this.fChildrens[j].Value as GEDCOMIndividualRecord;
					AbsDate kDate = GEDCOMUtils.GetAbstractDate(kChild, "BIRT");

					if (iDate > kDate) this.fChildrens.Exchange(i, j);
				}
			}
		}

		public string GetFamilyString(string unkHusband, string unkWife)
		{
			string result = "";

			GEDCOMIndividualRecord spouse = this.GetHusband();
			if (spouse == null)
			{
				if (unkHusband == null) unkHusband = "?";
				result += unkHusband;
			}
			else
			{
				result += spouse.GetNameString(true, false);
			}

			result += " - ";

			spouse = this.GetWife();
			if (spouse == null)
			{
				if (unkWife == null) unkWife = "?";
				result += unkWife;
			}
			else
			{
				result += spouse.GetNameString(true, false);
			}

			return result;
		}

		public GEDCOMIndividualRecord GetSpouseBy(GEDCOMIndividualRecord spouse)
		{
			GEDCOMIndividualRecord husb = this.GetHusband();
			GEDCOMIndividualRecord wife = this.GetWife();

			if (spouse == husb) {
				return wife;
			} else {
				return husb;
			}
		}

        public bool AddSpouse(GEDCOMIndividualRecord spouse)
		{
        	if (spouse == null)
        	{
        		return false;
        	}

            GEDCOMSex sex = spouse.Sex;
            if (sex == GEDCOMSex.svNone || sex == GEDCOMSex.svUndetermined)
			{
            	return false;
            }

            switch (sex)
            {
            	case GEDCOMSex.svMale:
            		this.Husband.Value = spouse;
            		break;

            	case GEDCOMSex.svFemale:
            		this.Wife.Value = spouse;
            		break;
            }

            GEDCOMSpouseToFamilyLink spLink = new GEDCOMSpouseToFamilyLink(this.Owner, spouse, "", "");
            spLink.Family = this;
            spouse.SpouseToFamilyLinks.Add(spLink);

            return true;
        }

		public void RemoveSpouse(GEDCOMIndividualRecord spouse)
		{
		    if (spouse == null) return;

		    spouse.DeleteSpouseToFamilyLink(this);

            switch (spouse.Sex)
            {
                case GEDCOMSex.svMale:
    		        this.Husband.Value = null;
                    break;

                case GEDCOMSex.svFemale:
		            this.Wife.Value = null;
                    break;
            }
		}

		public bool AddChild(GEDCOMIndividualRecord child)
		{
            if (child == null) return false;

			bool result;
			try
			{
				GEDCOMPointer ptr = new GEDCOMPointer(this.Owner, this, "", "");
				ptr.SetNamedValue("CHIL", child);
				this.Childrens.Add(ptr);

				GEDCOMChildToFamilyLink chLink = new GEDCOMChildToFamilyLink(this.Owner, child, "", "");
				chLink.Family = this;
				child.ChildToFamilyLinks.Add(chLink);

				result = true;
			}
			catch (Exception ex)
			{
                SysUtils.LogWrite("GEDCOMFamilyRecord.AddChild(): " + ex.Message);
				result = false;
			}
			return result;
		}

		public bool RemoveChild(GEDCOMIndividualRecord child)
		{
		    if (child == null) return false;
            bool result;

			try
			{
				this.DeleteChild(child);
				child.DeleteChildToFamilyLink(this);
				result = true;
			}
			catch (Exception ex)
			{
                SysUtils.LogWrite("GEDCOMFamilyRecord.RemoveChild(): " + ex.Message);
                result = false;
			}
			return result;
		}

		#endregion
	}
}
