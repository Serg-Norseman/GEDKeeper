using System;
using System.IO;
using System.Runtime.InteropServices;

using Ext.Utils;

namespace GedCom551
{
	public sealed class TGEDCOMFamilyRecord : TGEDCOMRecord
	{
		private TGEDCOMListEx<TGEDCOMFamilyEvent> _FamilyEvents;
		private TGEDCOMListEx<TGEDCOMPointer> _Childrens;
		private TGEDCOMListEx<TGEDCOMSpouseSealing> _SpouseSealings;

		public TGEDCOMListEx<TGEDCOMPointer> Childrens
		{
			get { return this._Childrens; }
		}

		public TGEDCOMListEx<TGEDCOMFamilyEvent> FamilyEvents
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

		public TGEDCOMListEx<TGEDCOMSpouseSealing> SpouseSealings
		{
			get { return this._SpouseSealings; }
		}

		protected override void CreateObj(TGEDCOMTree AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			base.SetLists(EnumSet.Create(new Enum[]
			{
				TGEDCOMSubList.stNotes, TGEDCOMSubList.stSource, TGEDCOMSubList.stMultimedia
			}));
			this.FRecordType = TGEDCOMRecordType.rtFamily;
			this.FName = "FAM";

			this._FamilyEvents = new TGEDCOMListEx<TGEDCOMFamilyEvent>(this);
			this._Childrens = new TGEDCOMListEx<TGEDCOMPointer>(this);
			this._SpouseSealings = new TGEDCOMListEx<TGEDCOMSpouseSealing>(this);
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

		public override TGEDCOMTag AddTag([In] string ATag, [In] string AValue, TagConstructor ATagConstructor)
		{
			TGEDCOMTag Result;

			if (ATag == "HUSB" || ATag == "WIFE")
			{
				Result = base.AddTag(ATag, AValue, TGEDCOMPointer.Create);
			}
			else
			{
				if (ATag == "CHIL")
				{
					Result = this._Childrens.Add(new TGEDCOMPointer(base.Owner, this, ATag, AValue));
				}
				else
				{
					if (ATag == "ANUL" || ATag == "CENS" || ATag == "DIV" || ATag == "DIVF" || ATag == "ENGA" || ATag == "MARB" || ATag == "MARC" || ATag == "MARR" || ATag == "MARL" || ATag == "MARS" || ATag == "RESI" || ATag == "EVEN")
					{
						Result = this._FamilyEvents.Add(new TGEDCOMFamilyEvent(base.Owner, this, ATag, AValue));
					}
					else
					{
						if (ATag == "SLGS")
						{
							Result = this._SpouseSealings.Add(new TGEDCOMSpouseSealing(base.Owner, this, ATag, AValue));
						}
						else
						{
							Result = base.AddTag(ATag, AValue, ATagConstructor);
						}
					}
				}
			}
			return Result;
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

		public TGEDCOMFamilyRecord(TGEDCOMTree AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}

        public new static TGEDCOMTag Create(TGEDCOMTree AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue)
		{
			return new TGEDCOMFamilyRecord(AOwner, AParent, AName, AValue);
		}
	}
}
