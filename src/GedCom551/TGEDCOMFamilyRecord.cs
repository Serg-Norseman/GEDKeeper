using System;
using System.IO;

using Ext.Utils;

namespace GedCom551
{
	public sealed class TGEDCOMFamilyRecord : TGEDCOMRecord
	{
		private GEDCOMList<TGEDCOMFamilyEvent> _FamilyEvents;
		private GEDCOMList<TGEDCOMPointer> _Childrens;
		private GEDCOMList<TGEDCOMSpouseSealing> _SpouseSealings;

		public GEDCOMList<TGEDCOMPointer> Childrens
		{
			get { return this._Childrens; }
		}

		public GEDCOMList<TGEDCOMFamilyEvent> FamilyEvents
		{
			get { return this._FamilyEvents; }
		}

		public TGEDCOMPointer Husband
		{
			get { return base.TagClass("HUSB", typeof(TGEDCOMPointer), TGEDCOMPointer.Create) as TGEDCOMPointer; }
		}

		public TGEDCOMPointer Wife
		{
			get { return base.TagClass("WIFE", typeof(TGEDCOMPointer), TGEDCOMPointer.Create) as TGEDCOMPointer; }
		}

		public TGEDCOMPointer Submitter
		{
			get { return base.TagClass("SUBM", typeof(TGEDCOMPointer), TGEDCOMPointer.Create) as TGEDCOMPointer; }
		}

		public TGEDCOMRestriction Restriction
		{
			get { return GetRestrictionVal(base.GetTagStringValue("RESN").Trim().ToUpper()); }
			set { base.SetTagStringValue("RESN", GetRestrictionStr(value)); }
		}

		public GEDCOMList<TGEDCOMSpouseSealing> SpouseSealings
		{
			get { return this._SpouseSealings; }
		}

		protected override void CreateObj(TGEDCOMTree owner, TGEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.FRecordType = TGEDCOMRecordType.rtFamily;
			this.FName = "FAM";

			this._FamilyEvents = new GEDCOMList<TGEDCOMFamilyEvent>(this);
			this._Childrens = new GEDCOMList<TGEDCOMPointer>(this);
			this._SpouseSealings = new GEDCOMList<TGEDCOMSpouseSealing>(this);
		}

		public override void Dispose()
		{
			if (!this.Disposed_)
			{
				this._FamilyEvents.Dispose();
				this._Childrens.Dispose();
				this._SpouseSealings.Dispose();

				base.Dispose();
				this.Disposed_ = true;
			}
		}

		private static GEDCOMFactory _factory;
		
		static TGEDCOMFamilyRecord()
		{
			GEDCOMFactory f = new GEDCOMFactory();
			
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

			_factory = f;
		}
		
		public override TGEDCOMTag AddTag(string ATag, string AValue, TagConstructor ATagConstructor)
		{
			TGEDCOMTag result;

			if (ATag == "HUSB" || ATag == "WIFE")
			{
				result = base.AddTag(ATag, AValue, TGEDCOMPointer.Create);
			}
			else if (ATag == "CHIL")
			{
				result = this._Childrens.Add(new TGEDCOMPointer(base.Owner, this, ATag, AValue));
			}
			else
			{
				result = _factory.CreateTag(this.Owner, this, ATag, AValue);

				if (result != null)
				{
					if (result is TGEDCOMFamilyEvent) {
						result = this._FamilyEvents.Add(result as TGEDCOMFamilyEvent);
					} else if (result is TGEDCOMSpouseSealing) {
						result = this._SpouseSealings.Add(result as TGEDCOMSpouseSealing);
					}
				} else {
					result = base.AddTag(ATag, AValue, ATagConstructor);
				}
			}

			return result;
		}

		public override void Clear()
		{
			base.Clear();

			this._FamilyEvents.Clear();
			this._Childrens.Clear();
			this._SpouseSealings.Clear();
		}

		public override bool IsEmpty()
		{
			return base.IsEmpty() && this._FamilyEvents.Count == 0 && this._Childrens.Count == 0 && this._SpouseSealings.Count == 0;
		}

		public void DeleteChild(TGEDCOMRecord ChildRec)
		{
			for (int i = this._Childrens.Count - 1; i >= 0; i--)
			{
				if (this._Childrens[i].Value == ChildRec)
				{
					this._Childrens.Delete(i);
					break;
				}
			}
		}

		public int IndexOfChild(TGEDCOMRecord ChildRec)
		{
			int Result = -1;

			for (int i = this._Childrens.Count - 1; i >= 0; i--)
			{
				if (this._Childrens[i].Value == ChildRec)
				{
					Result = i;
					break;
				}
			}

			return Result;
		}

		public override void MoveTo(TGEDCOMRecord aToRecord, bool aClearDest)
		{
			base.MoveTo(aToRecord, aClearDest);

			TGEDCOMFamilyRecord toRec = aToRecord as TGEDCOMFamilyRecord;

			while (this._FamilyEvents.Count > 0)
			{
				TGEDCOMObject obj = this._FamilyEvents.Extract(0);
                (obj as TGEDCOMTag).ResetParent(toRec);
				toRec.FamilyEvents.Add(obj as TGEDCOMFamilyEvent);
			}

			while (this._Childrens.Count > 0)
			{
				TGEDCOMObject obj = this._Childrens.Extract(0);
                (obj as TGEDCOMTag).ResetParent(toRec);
				toRec.Childrens.Add(obj as TGEDCOMPointer);
			}

			while (this._SpouseSealings.Count > 0)
			{
				TGEDCOMObject obj = this._SpouseSealings.Extract(0);
                (obj as TGEDCOMTag).ResetParent(toRec);
				toRec.SpouseSealings.Add(obj as TGEDCOMSpouseSealing);
			}
		}

		public override void Pack()
		{
			base.Pack();

			this._Childrens.Pack();
			this._FamilyEvents.Pack();
			this._SpouseSealings.Pack();
		}

		public override void ReplaceXRefs(TXRefReplaceMap aMap)
		{
			base.ReplaceXRefs(aMap);

			if (this.Husband != null)
			{
				this.Husband.StringValue = TGEDCOMObject.EncloseXRef(aMap.FindNewXRef(this.Husband.StringValue));
			}
			if (this.Wife != null)
			{
				this.Wife.StringValue = TGEDCOMObject.EncloseXRef(aMap.FindNewXRef(this.Wife.StringValue));
			}

			this._Childrens.ReplaceXRefs(aMap);
			this._FamilyEvents.ReplaceXRefs(aMap);
			this._SpouseSealings.ReplaceXRefs(aMap);
		}

		public override void ResetOwner(TGEDCOMTree AOwner)
		{
			base.ResetOwner(AOwner);

			this._Childrens.ResetOwner(AOwner);
			this._FamilyEvents.ResetOwner(AOwner);
			this._SpouseSealings.ResetOwner(AOwner);
		}

		public override void SaveToStream(StreamWriter AStream)
		{
			base.SaveToStream(AStream);

			this._Childrens.SaveToStream(AStream);
			this._FamilyEvents.SaveToStream(AStream);
			this._SpouseSealings.SaveToStream(AStream);
		}

		public void SortChilds()
		{
			int num = this._Childrens.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				int num2 = this._Childrens.Count - 1;
				for (int j = i + 1; j <= num2; j++)
				{
					TGEDCOMIndividualRecord iChild = this._Childrens[i].Value as TGEDCOMIndividualRecord;
					TGEDCOMCustomEvent iEv = iChild.GetIndividualEvent("BIRT");

					DateTime iDate = ((iEv != null) ? iEv.Detail.Date.aux_GetDate() : new DateTime(0));

					TGEDCOMIndividualRecord kChild = this._Childrens[j].Value as TGEDCOMIndividualRecord;
					TGEDCOMCustomEvent kEv = kChild.GetIndividualEvent("BIRT");

					DateTime kDate = ((kEv != null) ? kEv.Detail.Date.aux_GetDate() : new DateTime(0));

					if (iDate > kDate) this._Childrens.Exchange(i, j);
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

			int num = _FamilyEvents.Count - 1;
			for (int i = 0; i <= num; i++)
			{
				TGEDCOMFamilyEvent evt = _FamilyEvents[i];
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
			string Result = "";

			TGEDCOMIndividualRecord spouse = this.Husband.Value as TGEDCOMIndividualRecord;
			if (spouse == null)
			{
				if (unkHusband == null) unkHusband = "?";
				Result += unkHusband;
			}
			else
			{
				Result += spouse.aux_GetNameStr(true, false);
			}

			Result += " - ";

			spouse = (this.Wife.Value as TGEDCOMIndividualRecord);
			if (spouse == null)
			{
				if (unkWife == null) unkWife = "?";
				Result += unkWife;
			}
			else
			{
				Result += spouse.aux_GetNameStr(true, false);
			}

			return Result;
		}

		public override bool IsMatch(TGEDCOMRecord record, float matchThreshold, MatchParams matchParams)
		{
			bool match = false;

			if (record != null) {
				TGEDCOMFamilyRecord fam = record as TGEDCOMFamilyRecord;

				string title1 = this.aux_GetFamilyStr(null, null);
				string title2 = fam.aux_GetFamilyStr(null, null);

				match = (string.Compare(title1, title2, true) == 0);
			}

			return match;
		}

		public TGEDCOMFamilyRecord(TGEDCOMTree owner, TGEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static TGEDCOMTag Create(TGEDCOMTree owner, TGEDCOMObject parent, string tagName, string tagValue)
		{
			return new TGEDCOMFamilyRecord(owner, parent, tagName, tagValue);
		}

		public void aux_AddSpouse(TGEDCOMIndividualRecord aSpouse)
		{
			TGEDCOMSex sex = aSpouse.Sex;

            if (sex != TGEDCOMSex.svNone && sex != TGEDCOMSex.svUndetermined)
			{
                switch (sex)
                {
                    case TGEDCOMSex.svMale:
                        this.Husband.Value = aSpouse;
                        break;
                    case TGEDCOMSex.svFemale:
                        this.Wife.Value = aSpouse;
                        break;
                }

                TGEDCOMSpouseToFamilyLink spLink = new TGEDCOMSpouseToFamilyLink(this.Owner, aSpouse, "", "");
				spLink.Family = this;
				aSpouse.SpouseToFamilyLinks.Add(spLink);
			}
		}

		public void aux_RemoveSpouse(TGEDCOMIndividualRecord aSpouse)
		{
			if (aSpouse != null)
			{
				aSpouse.DeleteSpouseToFamilyLink(this);
				TGEDCOMSex sex = aSpouse.Sex;
				if (sex != TGEDCOMSex.svMale)
				{
					if (sex == TGEDCOMSex.svFemale)
					{
						this.Wife.Value = null;
					}
				}
				else
				{
					this.Husband.Value = null;
				}
			}
		}

		public bool aux_AddChild(TGEDCOMIndividualRecord aChild)
		{
			bool Result;
			try
			{
				TGEDCOMPointer ptr = new TGEDCOMPointer(this.Owner, this, "", "");
				ptr.SetNamedValue("CHIL", aChild);
				this.Childrens.Add(ptr);
				TGEDCOMChildToFamilyLink chLink = new TGEDCOMChildToFamilyLink(this.Owner, aChild, "", "");
				chLink.Family = this;
				aChild.ChildToFamilyLinks.Add(chLink);
				Result = true;
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("TGenEngine.AddFamilyChild(): " + E.Message);
				Result = false;
			}
			return Result;
		}

		public bool aux_RemoveChild(TGEDCOMIndividualRecord aChild)
		{
			bool Result;
			try
			{
				this.DeleteChild(aChild);
				aChild.DeleteChildToFamilyLink(this);
				Result = true;
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("TGenEngine.RemoveFamilyChild(): " + E.Message);
				Result = false;
			}
			return Result;
		}

	}
}
