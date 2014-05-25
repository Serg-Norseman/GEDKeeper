using System;
using System.IO;

using ExtUtils;

namespace GedCom551
{
	public sealed class TGEDCOMFamilyRecord : TGEDCOMRecord
	{
        private static readonly GEDCOMFactory fTagsFactory;
        
        private GEDCOMList<TGEDCOMFamilyEvent> fFamilyEvents;
		private GEDCOMList<TGEDCOMPointer> fChildrens;
		private GEDCOMList<TGEDCOMSpouseSealing> fSpouseSealings;

		public GEDCOMList<TGEDCOMPointer> Childrens
		{
			get { return this.fChildrens; }
		}

		public GEDCOMList<TGEDCOMFamilyEvent> FamilyEvents
		{
			get { return this.fFamilyEvents; }
		}

		public TGEDCOMPointer Husband
		{
			get { return base.TagClass("HUSB", TGEDCOMPointer.Create) as TGEDCOMPointer; }
		}

		public TGEDCOMPointer Wife
		{
			get { return base.TagClass("WIFE", TGEDCOMPointer.Create) as TGEDCOMPointer; }
		}

		public TGEDCOMPointer Submitter
		{
			get { return base.TagClass("SUBM", TGEDCOMPointer.Create) as TGEDCOMPointer; }
		}

		public TGEDCOMRestriction Restriction
		{
			get { return GEDCOMUtils.GetRestrictionVal(base.GetTagStringValue("RESN")); }
			set { base.SetTagStringValue("RESN", GEDCOMUtils.GetRestrictionStr(value)); }
		}

		public GEDCOMList<TGEDCOMSpouseSealing> SpouseSealings
		{
			get { return this.fSpouseSealings; }
		}

		protected override void CreateObj(TGEDCOMTree owner, GEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.fRecordType = TGEDCOMRecordType.rtFamily;
			this.fName = "FAM";

			this.fFamilyEvents = new GEDCOMList<TGEDCOMFamilyEvent>(this);
			this.fChildrens = new GEDCOMList<TGEDCOMPointer>(this);
			this.fSpouseSealings = new GEDCOMList<TGEDCOMSpouseSealing>(this);
		}

		protected override void Dispose(bool disposing)
		{
			if (disposing)
			{
				this.fFamilyEvents.Dispose();
				this.fChildrens.Dispose();
				this.fSpouseSealings.Dispose();
			}
			base.Dispose(disposing);
		}

		static TGEDCOMFamilyRecord()
		{
			GEDCOMFactory f = new GEDCOMFactory();
			fTagsFactory = f;

			f.RegisterTag("SLGS", TGEDCOMSpouseSealing.Create);

			f.RegisterTag("ANUL", TGEDCOMFamilyEvent.Create);
			f.RegisterTag("CENS", TGEDCOMFamilyEvent.Create);
			f.RegisterTag("DIV", TGEDCOMFamilyEvent.Create);
			f.RegisterTag("DIVF", TGEDCOMFamilyEvent.Create);
			f.RegisterTag("ENGA", TGEDCOMFamilyEvent.Create);
			f.RegisterTag("MARB", TGEDCOMFamilyEvent.Create);
			f.RegisterTag("MARC", TGEDCOMFamilyEvent.Create);
			f.RegisterTag("MARR", TGEDCOMFamilyEvent.Create);
			f.RegisterTag("MARL", TGEDCOMFamilyEvent.Create);
			f.RegisterTag("MARS", TGEDCOMFamilyEvent.Create);
			f.RegisterTag("RESI", TGEDCOMFamilyEvent.Create);
			f.RegisterTag("EVEN", TGEDCOMFamilyEvent.Create);
		}

		public override TGEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
		{
			TGEDCOMTag result;

			if (tagName == "HUSB" || tagName == "WIFE")
			{
				result = base.AddTag(tagName, tagValue, TGEDCOMPointer.Create);
			}
			else if (tagName == "CHIL")
			{
				result = this.fChildrens.Add(new TGEDCOMPointer(base.Owner, this, tagName, tagValue));
			}
			else
			{
				result = fTagsFactory.CreateTag(this.Owner, this, tagName, tagValue);

				if (result != null)
				{
					if (result is TGEDCOMFamilyEvent) {
						result = this.fFamilyEvents.Add(result as TGEDCOMFamilyEvent);
					} else if (result is TGEDCOMSpouseSealing) {
						result = this.fSpouseSealings.Add(result as TGEDCOMSpouseSealing);
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

			this.fFamilyEvents.Clear();
			this.fChildrens.Clear();
			this.fSpouseSealings.Clear();
		}

		public override bool IsEmpty()
		{
			return base.IsEmpty() && this.fFamilyEvents.Count == 0 && this.fChildrens.Count == 0 && this.fSpouseSealings.Count == 0;
		}

		public void DeleteChild(TGEDCOMRecord childRec)
		{
			for (int i = this.fChildrens.Count - 1; i >= 0; i--)
			{
				if (this.fChildrens[i].Value == childRec)
				{
					this.fChildrens.Delete(i);
					break;
				}
			}
		}

		public int IndexOfChild(TGEDCOMRecord childRec)
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

        public override void MoveTo(TGEDCOMRecord targetRecord, bool clearDest)
		{
            base.MoveTo(targetRecord, clearDest);

			TGEDCOMFamilyRecord targetFamily = targetRecord as TGEDCOMFamilyRecord;

			while (this.fFamilyEvents.Count > 0)
			{
                TGEDCOMFamilyEvent obj = this.fFamilyEvents.Extract(0);
                obj.ResetParent(targetFamily);
				targetFamily.FamilyEvents.Add(obj);
			}

			while (this.fChildrens.Count > 0)
			{
                TGEDCOMPointer obj = this.fChildrens.Extract(0);
                obj.ResetParent(targetFamily);
				targetFamily.Childrens.Add(obj);
			}

			while (this.fSpouseSealings.Count > 0)
			{
                TGEDCOMSpouseSealing obj = this.fSpouseSealings.Extract(0);
                obj.ResetParent(targetFamily);
				targetFamily.SpouseSealings.Add(obj);
			}
		}

		public override void Pack()
		{
			base.Pack();

			this.fChildrens.Pack();
			this.fFamilyEvents.Pack();
			this.fSpouseSealings.Pack();
		}

        public override void ReplaceXRefs(XRefReplacer map)
		{
            base.ReplaceXRefs(map);

			if (this.Husband != null)
			{
                this.Husband.StringValue = GEDCOMUtils.EncloseXRef(map.FindNewXRef(this.Husband.StringValue));
			}
			if (this.Wife != null)
			{
                this.Wife.StringValue = GEDCOMUtils.EncloseXRef(map.FindNewXRef(this.Wife.StringValue));
			}

            this.fChildrens.ReplaceXRefs(map);
            this.fFamilyEvents.ReplaceXRefs(map);
            this.fSpouseSealings.ReplaceXRefs(map);
		}

		public override void ResetOwner(TGEDCOMTree newOwner)
		{
			base.ResetOwner(newOwner);

			this.fChildrens.ResetOwner(newOwner);
			this.fFamilyEvents.ResetOwner(newOwner);
			this.fSpouseSealings.ResetOwner(newOwner);
		}

		public override void SaveToStream(StreamWriter stream)
		{
			base.SaveToStream(stream);

			this.fChildrens.SaveToStream(stream);
			this.fFamilyEvents.SaveToStream(stream);
			this.fSpouseSealings.SaveToStream(stream);
		}

		public TGEDCOMFamilyRecord(TGEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static TGEDCOMTag Create(TGEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
		{
			return new TGEDCOMFamilyRecord(owner, parent, tagName, tagValue);
		}

		public override float IsMatch(TGEDCOMTag tag, MatchParams matchParams)
		{
			if (tag == null) return 0.0f;
			float match = 0.0f;

			TGEDCOMFamilyRecord fam = tag as TGEDCOMFamilyRecord;
			string title1 = this.aux_GetFamilyStr(null, null);
			string title2 = fam.aux_GetFamilyStr(null, null);
			if (string.Compare(title1, title2, true) == 0) {
				match = 100.0f;
			}

			return match;
		}

        #region Auxiliary

		public void aux_SortChilds()
		{
			int num = this.fChildrens.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				int num2 = this.fChildrens.Count - 1;
				for (int j = i + 1; j <= num2; j++)
				{
					TGEDCOMIndividualRecord iChild = this.fChildrens[i].Value as TGEDCOMIndividualRecord;
					TGEDCOMCustomEvent iEv = iChild.GetIndividualEvent("BIRT");

					DateTime iDate = ((iEv != null) ? iEv.Detail.Date.aux_GetDate() : new DateTime(0));

					TGEDCOMIndividualRecord kChild = this.fChildrens[j].Value as TGEDCOMIndividualRecord;
					TGEDCOMCustomEvent kEv = kChild.GetIndividualEvent("BIRT");

					DateTime kDate = ((kEv != null) ? kEv.Detail.Date.aux_GetDate() : new DateTime(0));

					if (iDate > kDate) this.fChildrens.Exchange(i, j);
				}
			}
		}

		public TGEDCOMIndividualRecord aux_GetSpouse(TGEDCOMIndividualRecord spouse)
		{
			TGEDCOMIndividualRecord husb = this.Husband.Value as TGEDCOMIndividualRecord;
			TGEDCOMIndividualRecord wife = this.Wife.Value as TGEDCOMIndividualRecord;

			if (spouse == husb) {
				return wife;
			} else {
				return husb;
			}
		}

		public TGEDCOMFamilyEvent aux_GetFamilyEvent(string evName)
		{
			TGEDCOMFamilyEvent result = null;

			int num = fFamilyEvents.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				TGEDCOMFamilyEvent evt = fFamilyEvents[i];
				if (evt.Name == evName)
				{
					result = evt;
					break;
				}
			}

			return result;
		}

		public string aux_GetFamilyStr(string unkHusband, string unkWife)
		{
			string result = "";

			TGEDCOMIndividualRecord spouse = this.Husband.Value as TGEDCOMIndividualRecord;
			if (spouse == null)
			{
				if (unkHusband == null) unkHusband = "?";
				result += unkHusband;
			}
			else
			{
				result += spouse.aux_GetNameStr(true, false);
			}

			result += " - ";

			spouse = (this.Wife.Value as TGEDCOMIndividualRecord);
			if (spouse == null)
			{
				if (unkWife == null) unkWife = "?";
				result += unkWife;
			}
			else
			{
				result += spouse.aux_GetNameStr(true, false);
			}

			return result;
		}

        public void aux_AddSpouse(TGEDCOMIndividualRecord spouse)
		{
            if (spouse == null) return;

            TGEDCOMSex sex = spouse.Sex;
            if (sex != TGEDCOMSex.svNone && sex != TGEDCOMSex.svUndetermined)
			{
                switch (sex)
                {
                    case TGEDCOMSex.svMale:
                        this.Husband.Value = spouse;
                        break;
                    case TGEDCOMSex.svFemale:
                        this.Wife.Value = spouse;
                        break;
                }

                TGEDCOMSpouseToFamilyLink spLink = new TGEDCOMSpouseToFamilyLink(this.Owner, spouse, "", "");
				spLink.Family = this;
                spouse.SpouseToFamilyLinks.Add(spLink);
			}
		}

		public void aux_RemoveSpouse(TGEDCOMIndividualRecord spouse)
		{
		    if (spouse == null) return;

		    spouse.DeleteSpouseToFamilyLink(this);

            switch (spouse.Sex)
            {
                case TGEDCOMSex.svMale:
    		        this.Husband.Value = null;
                    break;

                case TGEDCOMSex.svFemale:
		            this.Wife.Value = null;
                    break;
            }
		}

		public bool aux_AddChild(TGEDCOMIndividualRecord child)
		{
            if (child == null) return false;

			bool result;
			try
			{
				TGEDCOMPointer ptr = new TGEDCOMPointer(this.Owner, this, "", "");
				ptr.SetNamedValue("CHIL", child);
				this.Childrens.Add(ptr);
				TGEDCOMChildToFamilyLink chLink = new TGEDCOMChildToFamilyLink(this.Owner, child, "", "");
				chLink.Family = this;
				child.ChildToFamilyLinks.Add(chLink);
				result = true;
			}
			catch (Exception ex)
			{
                SysUtils.LogWrite("TGEDCOMFamilyRecord.AddFamilyChild(): " + ex.Message);
				result = false;
			}
			return result;
		}

		public bool aux_RemoveChild(TGEDCOMIndividualRecord child)
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
                SysUtils.LogWrite("TGEDCOMFamilyRecord.RemoveFamilyChild(): " + ex.Message);
                result = false;
			}
			return result;
		}

		#endregion
	}
}
