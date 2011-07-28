using GKSys;
using System;
using System.ComponentModel;
using System.IO;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GedCom551
{
	public class TGEDCOMEventDetail : TGEDCOMTagWithLists
	{
		[Browsable(false)]
		public string Classification
		{
			get
			{
				return this.GetStringTag(1);
			}
			set
			{
				this.SetStringTag(1, value);
			}
		}

		[Browsable(false)]
		public string Agency
		{
			get
			{
				return this.GetStringTag(2);
			}
			set
			{
				this.SetStringTag(2, value);
			}
		}

		[Browsable(false)]
		public string ReligiousAffilation
		{
			get
			{
				return this.GetStringTag(3);
			}
			set
			{
				this.SetStringTag(3, value);
			}
		}
		[Browsable(false)]
		public string Cause
		{
			get
			{
				return this.GetStringTag(4);
			}
			set
			{
				this.SetStringTag(4, value);
			}
		}
		[Browsable(false)]
		public TGEDCOMPlace Place
		{
			get
			{
				return this.GetPlace();
			}
		}
		[Browsable(false)]
		public TGEDCOMAddress Address
		{
			get
			{
				return this.GetAddress();
			}
		}
		[Browsable(false)]
		public TGEDCOMDateValue Date
		{
			get
			{
				return this.GetDate();
			}
		}
		[Browsable(false)]
		public TGEDCOMObject.TGEDCOMRestriction Restriction
		{
			get
			{
				return this.GetRestriction();
			}
			set
			{
				this.SetRestriction(value);
			}
		}

		/*[Browsable(false)]
		public new TGEDCOMNotes Notes
		{
			get
			{
				return base.GetNote(Index);
			}
		}
		[Browsable(false)]
		public new int NotesCount
		{
			get
			{
				return base.NotesCount;
			}
		}*/

		/*[Browsable(false)]
		public new TGEDCOMSourceCitation SourceCitations
		{
			get
			{
				return base.GetSourceCitation(Index);
			}
		}
		[Browsable(false)]
		public new int SourceCitationsCount
		{
			get
			{
				return base.SourceCitationsCount;
			}
		}*/

		/*[Browsable(false)]
		public new TGEDCOMMultimediaLink MultimediaLinks
		{
			get
			{
				return base.GetMultimediaLink(Index);
			}
		}
		[Browsable(false)]
		public new int MultimediaLinksCount
		{
			get
			{
				return base.MultimediaLinksCount;
			}
		}*/

		internal string GetStringTag(int Index)
		{
			string Result = "";
			if (Index != 1)
			{
				if (Index != 2)
				{
					if (Index != 3)
					{
						if (Index == 4)
						{
							Result = base.GetTagStringValue("CAUS");
						}
					}
					else
					{
						Result = base.GetTagStringValue("RELI");
					}
				}
				else
				{
					Result = base.GetTagStringValue("AGNC");
				}
			}
			else
			{
				Result = base.GetTagStringValue("TYPE");
			}
			return Result;
		}
		internal void SetStringTag(int Index, [In] string Value)
		{
			if (Index != 1)
			{
				if (Index != 2)
				{
					if (Index != 3)
					{
						if (Index == 4)
						{
							base.SetTagStringValue("CAUS", Value);
						}
					}
					else
					{
						base.SetTagStringValue("RELI", Value);
					}
				}
				else
				{
					base.SetTagStringValue("AGNC", Value);
				}
			}
			else
			{
				base.SetTagStringValue("TYPE", Value);
			}
		}
		internal TGEDCOMDateValue GetDate()
		{
			return base.TagClass("DATE", typeof(TGEDCOMDateValue)) as TGEDCOMDateValue;
		}
		internal TGEDCOMAddress GetAddress()
		{
			return base.TagClass("ADDR", typeof(TGEDCOMAddress)) as TGEDCOMAddress;
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
			string S;
			if (Value != TGEDCOMObject.TGEDCOMRestriction.rnConfidential)
			{
				if (Value != TGEDCOMObject.TGEDCOMRestriction.rnLocked)
				{
					if (Value != TGEDCOMObject.TGEDCOMRestriction.rnPrivacy)
					{
						S = "";
					}
					else
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
			base.SetTagStringValue("RESN", S);
		}
		internal TGEDCOMPlace GetPlace()
		{
			return base.TagClass("PLAC", typeof(TGEDCOMPlace)) as TGEDCOMPlace;
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
			this.FLevel = (AParent as TGEDCOMCustomTag).Level;
		}

		public override TGEDCOMTag AddTag([In] string ATag, [In] string AValue, Type AClass)
		{
			TGEDCOMTag Result;
			if (BDSSystem.WStrCmp(ATag, "DATE") == 0)
			{
				Result = base.AddTag(ATag, AValue, typeof(TGEDCOMDateValue));
			}
			else
			{
				if (BDSSystem.WStrCmp(ATag, "PLAC") == 0)
				{
					Result = base.AddTag(ATag, AValue, typeof(TGEDCOMPlace));
				}
				else
				{
					if (BDSSystem.WStrCmp(ATag, "ADDR") == 0)
					{
						Result = base.AddTag(ATag, AValue, typeof(TGEDCOMAddress));
					}
					else
					{
						if (BDSSystem.WStrCmp(ATag, "PHON") == 0 || BDSSystem.WStrCmp(ATag, "EMAIL") == 0 || BDSSystem.WStrCmp(ATag, "FAX") == 0 || BDSSystem.WStrCmp(ATag, "WWW") == 0)
						{
							Result = this.GetAddress().AddTag(ATag, AValue, AClass);
						}
						else
						{
							Result = base.AddTag(ATag, AValue, AClass);
						}
					}
				}
			}
			return Result;
		}
		public override void SaveToStream(StreamWriter AStream)
		{
			string[] aTagSorting = new string[0];
			this.SaveTagsToStream(AStream, aTagSorting);
			if (this.FNotes != null)
			{
				this.FNotes.SaveToStream(AStream);
			}
			if (this.FSourceCitations != null)
			{
				this.FSourceCitations.SaveToStream(AStream);
			}
			if (this.FMultimediaLinks != null)
			{
				this.FMultimediaLinks.SaveToStream(AStream);
			}
		}

		public TGEDCOMEventDetail(TGEDCOMObject AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
