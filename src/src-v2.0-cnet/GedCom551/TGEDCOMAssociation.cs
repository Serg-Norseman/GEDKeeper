using GKSys;
using System;
using System.ComponentModel;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GedCom551
{
	public class TGEDCOMAssociation : TGEDCOMPointerWithNotes
	{

		internal TGEDCOMList FSourceCitations;

		[Browsable(false)]
		public TGEDCOMIndividualRecord Individual
		{
			get
			{
				return this.GetIndividual();
			}
			set
			{
				this.SetIndividual(value);
			}
		}

		[Browsable(false)]
		public string Relation
		{
			get
			{
				return this.GetRelation();
			}
			set
			{
				this.SetRelation(value);
			}
		}

		/*[Browsable(false)]
		public TGEDCOMSourceCitation SourceCitations
		{
			get
			{
				return this.GetSourceCitations(Index);
			}
		}*/

		[Browsable(false)]
		public int SourceCitationsCount
		{
			get
			{
				return this.GetSourceCitationsCount();
			}
		}

		internal string GetRelation()
		{
			return base.GetTagStringValue("RELA");
		}

		internal void SetRelation([In] string Value)
		{
			base.SetTagStringValue("RELA", Value);
		}

		internal TGEDCOMSourceCitation GetSourceCitations(int Index)
		{
			TGEDCOMSourceCitation Result;
			if (this.FSourceCitations == null)
			{
				Result = null;
			}
			else
			{
				Result = (this.FSourceCitations[Index] as TGEDCOMSourceCitation);
			}
			return Result;
		}

		internal int GetSourceCitationsCount()
		{
			int Result;
			if (this.FSourceCitations == null)
			{
				Result = 0;
			}
			else
			{
				Result = this.FSourceCitations.Count;
			}
			return Result;
		}

		internal TGEDCOMIndividualRecord GetIndividual()
		{
			return base.GetValue() as TGEDCOMIndividualRecord;
		}

		internal void SetIndividual([In] TGEDCOMIndividualRecord Value)
		{
			base.SetValue(Value);
		}

		protected internal override void CreateObj(TGEDCOMObject AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			this.FName = "ASSO";
			this.FSourceCitations = null;
		}

		public override void Dispose()
		{
			if (!this.Disposed_)
			{
				if (this.FSourceCitations != null)
				{
					this.FSourceCitations.Free();
				}
				base.Dispose();
				this.Disposed_ = true;
			}
		}

		public TGEDCOMSourceCitation AddSourceCitation(TGEDCOMSourceCitation ASourceCitation)
		{
			if (this.FSourceCitations == null)
			{
				this.FSourceCitations = new TGEDCOMList(this);
			}
			if (ASourceCitation != null)
			{
				this.FSourceCitations.Add(ASourceCitation);
			}
			return ASourceCitation;
		}

		public override TGEDCOMTag AddTag([In] string ATag, [In] string AValue, Type AClass)
		{
			TGEDCOMTag Result;
			if (BDSSystem.WStrCmp(ATag, "SOUR") == 0)
			{
				Result = this.AddSourceCitation(new TGEDCOMSourceCitation(base.Owner, this, ATag, AValue));
			}
			else
			{
				Result = base.AddTag(ATag, AValue, AClass);
			}
			return Result;
		}

		public override void Clear()
		{
			base.Clear();
			if (this.FSourceCitations != null)
			{
				this.FSourceCitations.Clear();
			}
		}

		public override bool IsEmpty()
		{
			return base.IsEmpty() && this.GetSourceCitationsCount() == 0;
		}

		public override void ReplaceXRefs(TXRefReplaceMap aMap)
		{
			base.ReplaceXRefs(aMap);
			if (this.FSourceCitations != null)
			{
				this.FSourceCitations.ReplaceXRefs(aMap);
			}
		}

		public override void ResetOwner(TGEDCOMObject AOwner)
		{
			base.ResetOwner(AOwner);
			if (this.FSourceCitations != null)
			{
				this.FSourceCitations.ResetOwner(AOwner);
			}
		}

		public TGEDCOMAssociation(TGEDCOMObject AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
