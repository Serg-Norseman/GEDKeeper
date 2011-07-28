using GKSys;
using System;
using System.ComponentModel;
using System.IO;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GedCom551
{
	public class TGEDCOMFamilyRecord : TGEDCOMRecord
	{
		internal TGEDCOMList FFamilyEvents;
		internal TGEDCOMList FChildren;
		internal TGEDCOMList FSpouseSealings;

		public string AutomatedRecordID
		{
			get { return this.GetStringTag(1); }
			set { this.SetStringTag(1, value); }
		}

		/*public TGEDCOMPointer Children
		{
			get { return this.GetChildren(Index); }
		}*/

		public int ChildrenCount
		{
			get { return this.GetChildrenCount(); }
		}

		/*[Browsable(false)]
		public TGEDCOMFamilyEvent FamilyEvents
		{
			get { return this.GetFamilyEvents(Index); }
		}

		[Browsable(false)]
		public int FamilyEventCount
		{
			get { return this.GetFamilyEventCount(); }
		}*/

		public TGEDCOMPointer Husband
		{
			get { return this.GetHusband(); }
		}

		public TGEDCOMObject.TGEDCOMRestriction Restriction
		{
			get { return this.GetRestriction(); }
			set { this.SetRestriction(value); }
		}

		/*public TGEDCOMSpouseSealing SpouseSealing
		{
			get { return this.GetSpouseSealing(Index); }
		}*/

		public int SpouseSealingCount
		{
			get { return this.GetSpouseSealingCount(); }
		}

		public TGEDCOMPointer Submitter
		{
			get { return this.GetSubmittor(); }
		}

		public TGEDCOMPointer Wife
		{
			get { return this.GetWife(); }
		}

		internal string GetStringTag(int Index)
		{
			string Result = "";
			if (Index == 1)
			{
				Result = base.GetTagStringValue("RIN");
			}
			return Result;
		}

		internal void SetStringTag(int Index, [In] string Value)
		{
			if (Index == 1)
			{
				base.SetTagStringValue("RIN", Value);
			}
		}

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

		public TGEDCOMFamilyEvent GetFamilyEvent(int Index)
		{
			TGEDCOMFamilyEvent Result;
			if (this.FFamilyEvents == null)
			{
				Result = null;
			}
			else
			{
				Result = (this.FFamilyEvents[Index] as TGEDCOMFamilyEvent);
			}
			return Result;
		}

		public int GetFamilyEventCount()
		{
			int Result;
			if (this.FFamilyEvents == null)
			{
				Result = 0;
			}
			else
			{
				Result = this.FFamilyEvents.Count;
			}
			return Result;
		}

		public TGEDCOMPointer GetChildren(int Index)
		{
			TGEDCOMPointer Result;
			if (this.FChildren == null)
			{
				Result = null;
			}
			else
			{
				Result = (this.FChildren[Index] as TGEDCOMPointer);
			}
			return Result;
		}

		internal int GetChildrenCount()
		{
			int Result;
			if (this.FChildren == null)
			{
				Result = 0;
			}
			else
			{
				Result = this.FChildren.Count;
			}
			return Result;
		}

		internal TGEDCOMPointer GetHusband()
		{
			return base.TagClass("HUSB", typeof(TGEDCOMPointer)) as TGEDCOMPointer;
		}

		internal TGEDCOMPointer GetWife()
		{
			return base.TagClass("WIFE", typeof(TGEDCOMPointer)) as TGEDCOMPointer;
		}

		internal TGEDCOMPointer GetSubmittor()
		{
			return base.TagClass("SUBM", typeof(TGEDCOMPointer)) as TGEDCOMPointer;
		}

		internal TGEDCOMSpouseSealing GetSpouseSealing(int Index)
		{
			TGEDCOMSpouseSealing Result;
			if (this.FSpouseSealings == null)
			{
				Result = null;
			}
			else
			{
				Result = (this.FSpouseSealings[Index] as TGEDCOMSpouseSealing);
			}
			return Result;
		}

		internal int GetSpouseSealingCount()
		{
			int Result;
			if (this.FSpouseSealings == null)
			{
				Result = 0;
			}
			else
			{
				Result = this.FSpouseSealings.Count;
			}
			return Result;
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
			this.FRecordType = TGEDCOMRecord.TGEDCOMRecordType.rtFamily;
			this.FName = "FAM";
			this.FFamilyEvents = null;
			this.FChildren = null;
			this.FSpouseSealings = null;
		}

		public override void Dispose()
		{
			if (!this.Disposed_)
			{
				if (this.FFamilyEvents != null)
				{
					this.FFamilyEvents.Free();
				}
				if (this.FChildren != null)
				{
					this.FChildren.Free();
				}
				if (this.FSpouseSealings != null)
				{
					this.FSpouseSealings.Free();
				}
				base.Dispose();
				this.Disposed_ = true;
			}
		}

		public TGEDCOMPointer AddChild(TGEDCOMPointer APointer)
		{
			if (this.FChildren == null)
			{
				this.FChildren = new TGEDCOMList(this);
			}
			if (APointer != null)
			{
				this.FChildren.Add(APointer);
			}
			return APointer;
		}
		public TGEDCOMFamilyEvent AddFamilyEvent(TGEDCOMFamilyEvent AFamilyEvent)
		{
			if (this.FFamilyEvents == null)
			{
				this.FFamilyEvents = new TGEDCOMList(this);
			}
			if (AFamilyEvent != null)
			{
				this.FFamilyEvents.Add(AFamilyEvent);
			}
			return AFamilyEvent;
		}

		public TGEDCOMSpouseSealing AddSpouseSealing(TGEDCOMSpouseSealing ASpouseSealing)
		{
			if (this.FSpouseSealings == null)
			{
				this.FSpouseSealings = new TGEDCOMList(this);
			}
			if (ASpouseSealing != null)
			{
				this.FSpouseSealings.Add(ASpouseSealing);
			}
			return ASpouseSealing;
		}

		public override TGEDCOMTag AddTag([In] string ATag, [In] string AValue, Type AClass)
		{
			TGEDCOMTag Result;
			if (BDSSystem.WStrCmp(ATag, "HUSB") == 0 || BDSSystem.WStrCmp(ATag, "WIFE") == 0)
			{
				Result = base.AddTag(ATag, AValue, typeof(TGEDCOMPointer));
			}
			else
			{
				if (BDSSystem.WStrCmp(ATag, "CHIL") == 0)
				{
					Result = this.AddChild(new TGEDCOMPointer(base.Owner, this, ATag, AValue));
				}
				else
				{
					if (BDSSystem.WStrCmp(ATag, "ANUL") == 0 || BDSSystem.WStrCmp(ATag, "CENS") == 0 || BDSSystem.WStrCmp(ATag, "DIV") == 0 || BDSSystem.WStrCmp(ATag, "DIVF") == 0 || BDSSystem.WStrCmp(ATag, "ENGA") == 0 || BDSSystem.WStrCmp(ATag, "MARB") == 0 || BDSSystem.WStrCmp(ATag, "MARC") == 0 || BDSSystem.WStrCmp(ATag, "MARR") == 0 || BDSSystem.WStrCmp(ATag, "MARL") == 0 || BDSSystem.WStrCmp(ATag, "MARS") == 0 || BDSSystem.WStrCmp(ATag, "RESI") == 0 || BDSSystem.WStrCmp(ATag, "EVEN") == 0)
					{
						Result = this.AddFamilyEvent(new TGEDCOMFamilyEvent(base.Owner, this, ATag, AValue));
					}
					else
					{
						if (BDSSystem.WStrCmp(ATag, "SLGS") == 0)
						{
							Result = this.AddSpouseSealing(new TGEDCOMSpouseSealing(base.Owner, this, ATag, AValue));
						}
						else
						{
							if (BDSSystem.WStrCmp(ATag, "REFN") == 0)
							{
								Result = base.AddUserReference(new TGEDCOMUserReference(base.Owner, this, ATag, AValue));
							}
							else
							{
								Result = base.AddTag(ATag, AValue, AClass);
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
			if (this.FFamilyEvents != null)
			{
				this.FFamilyEvents.Clear();
			}
			if (this.FChildren != null)
			{
				this.FChildren.Clear();
			}
			if (this.FSpouseSealings != null)
			{
				this.FSpouseSealings.Clear();
			}
		}
		public override bool IsEmpty()
		{
			return base.IsEmpty() && this.GetFamilyEventCount() == 0 && this.GetChildrenCount() == 0 && this.GetSpouseSealingCount() == 0;
		}
		public void DeleteChild(TGEDCOMRecord ChildRec)
		{
			if (this.FChildren != null)
			{
				int i = this.FChildren.Count - 1;
				if (i >= 0)
				{
					while (!object.Equals((this.FChildren[i] as TGEDCOMPointer).Value, ChildRec))
					{
						i--;
						if (i == -1)
						{
							return;
						}
					}
					this.FChildren.Delete(i);
				}
			}
		}
		public void DeleteChild(int Index)
		{
			if (this.FChildren != null)
			{
				this.FChildren.Delete(Index);
			}
		}
		public void DeleteFamilyEvent(TGEDCOMFamilyEvent aEvent)
		{
			if (this.FFamilyEvents != null)
			{
				this.FFamilyEvents.DeleteObject(aEvent);
			}
		}
		public int IndexOfChild(TGEDCOMRecord ChildRec)
		{
			int Result = -1;
			if (this.FChildren != null)
			{
				int i = this.FChildren.Count - 1;
				if (i >= 0)
				{
					while (!object.Equals((this.FChildren[i] as TGEDCOMPointer).Value, ChildRec))
					{
						i--;
						if (i == -1)
						{
							return Result;
						}
					}
					Result = i;
				}
			}
			return Result;
		}
		public void ChildrenExchange(int Index1, int Index2)
		{
			if (this.FChildren != null)
			{
				this.FChildren.Exchange(Index1, Index2);
			}
		}
		public override void MoveTo(TGEDCOMRecord aToRecord, bool aClearDest)
		{
			base.MoveTo(aToRecord, aClearDest);
			TGEDCOMFamilyRecord toRec = aToRecord as TGEDCOMFamilyRecord;
			if (this.FFamilyEvents != null)
			{
				while (this.FFamilyEvents.Count > 0)
				{
					TGEDCOMObject obj = this.FFamilyEvents.Extract(0);
					(obj as TGEDCOMCustomTag).ResetParent(toRec);
					toRec.AddFamilyEvent(obj as TGEDCOMFamilyEvent);
				}
			}
			if (this.FChildren != null)
			{
				while (this.FChildren.Count > 0)
				{
					TGEDCOMObject obj = this.FChildren.Extract(0);
					(obj as TGEDCOMCustomTag).ResetParent(toRec);
					toRec.AddChild(obj as TGEDCOMPointer);
				}
			}
			if (this.FSpouseSealings != null)
			{
				while (this.FSpouseSealings.Count > 0)
				{
					TGEDCOMObject obj = this.FSpouseSealings.Extract(0);
					(obj as TGEDCOMCustomTag).ResetParent(toRec);
					toRec.AddSpouseSealing(obj as TGEDCOMSpouseSealing);
				}
			}
		}
		public override void Pack()
		{
			base.Pack();
			if (this.FChildren != null)
			{
				this.FChildren.Pack();
			}
			if (this.FFamilyEvents != null)
			{
				this.FFamilyEvents.Pack();
			}
			if (this.FSpouseSealings != null)
			{
				this.FSpouseSealings.Pack();
			}
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
			if (this.FChildren != null)
			{
				this.FChildren.ReplaceXRefs(aMap);
			}
			if (this.FFamilyEvents != null)
			{
				this.FFamilyEvents.ReplaceXRefs(aMap);
			}
			if (this.FSpouseSealings != null)
			{
				this.FSpouseSealings.ReplaceXRefs(aMap);
			}
		}
		public override void ResetOwner(TGEDCOMObject AOwner)
		{
			base.ResetOwner(AOwner);
			if (this.FChildren != null)
			{
				this.FChildren.ResetOwner(AOwner);
			}
			if (this.FFamilyEvents != null)
			{
				this.FFamilyEvents.ResetOwner(AOwner);
			}
			if (this.FSpouseSealings != null)
			{
				this.FSpouseSealings.ResetOwner(AOwner);
			}
		}
		public override void SaveToStream(StreamWriter AStream)
		{
			base.SaveToStream(AStream);
			if (this.FChildren != null)
			{
				this.FChildren.SaveToStream(AStream);
			}
			if (this.FFamilyEvents != null)
			{
				this.FFamilyEvents.SaveToStream(AStream);
			}
			if (this.FSpouseSealings != null)
			{
				this.FSpouseSealings.SaveToStream(AStream);
			}
		}

		public void SortChilds()
		{
		}

		public TGEDCOMFamilyRecord(TGEDCOMObject AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
