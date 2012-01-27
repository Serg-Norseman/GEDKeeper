using System;
using System.IO;
using System.Runtime.InteropServices;

using GKSys;

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
				(obj as TGEDCOMCustomTag).ResetParent(toRec);
				toRec.FamilyEvents.Add(obj as TGEDCOMFamilyEvent);
			}

			while (this._Childrens.Count > 0)
			{
				TGEDCOMObject obj = this._Childrens.Extract(0);
				(obj as TGEDCOMCustomTag).ResetParent(toRec);
				toRec.Childrens.Add(obj as TGEDCOMPointer);
			}

			while (this._SpouseSealings.Count > 0)
			{
				TGEDCOMObject obj = this._SpouseSealings.Extract(0);
				(obj as TGEDCOMCustomTag).ResetParent(toRec);
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
			
		}

		public TGEDCOMFamilyRecord(TGEDCOMTree AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}

		public new static TGEDCOMCustomTag Create(TGEDCOMTree AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue)
		{
			return new TGEDCOMFamilyRecord(AOwner, AParent, AName, AValue);
		}
	}
}
