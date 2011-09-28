using System;
using System.IO;
using System.Runtime.InteropServices;

using GKCore.Sys;

namespace GedCom551
{
	public sealed class TGEDCOMEventDetail : TGEDCOMTagWithLists
	{
		public string Classification
		{
			get { return this.GetStringTag(1); }
			set { this.SetStringTag(1, value); }
		}

		public string Agency
		{
			get { return this.GetStringTag(2); }
			set { this.SetStringTag(2, value); }
		}


		public string ReligiousAffilation
		{
			get { return this.GetStringTag(3); }
			set { this.SetStringTag(3, value); }
		}

		public string Cause
		{
			get { return this.GetStringTag(4); }
			set { this.SetStringTag(4, value); }
		}

		public TGEDCOMPlace Place
		{
			get { return base.TagClass("PLAC", typeof(TGEDCOMPlace)) as TGEDCOMPlace; }
		}

		public TGEDCOMAddress Address
		{
			get { return this.GetAddress(); }
		}

		public TGEDCOMDateValue Date
		{
			get { return base.TagClass("DATE", typeof(TGEDCOMDateValue)) as TGEDCOMDateValue; }
		}

		public TGEDCOMRestriction Restriction
		{
			get { return GetRestrictionVal(base.GetTagStringValue("RESN").Trim().ToUpper()); }
			set { base.SetTagStringValue("RESN", GetRestrictionStr(value)); }
		}

		/*
		public new TGEDCOMNotes Notes
		{
			get
			{
				return base.GetNote(Index);
			}
		}

		public new int NotesCount
		{
			get
			{
				return base.NotesCount;
			}
		}*/

		/*
		public new TGEDCOMSourceCitation SourceCitations
		{
			get
			{
				return base.GetSourceCitation(Index);
			}
		}

		public new int SourceCitationsCount
		{
			get
			{
				return base.SourceCitationsCount;
			}
		}*/

		/*
		public new TGEDCOMMultimediaLink MultimediaLinks
		{
			get
			{
				return base.GetMultimediaLink(Index);
			}
		}

		public new int MultimediaLinksCount
		{
			get
			{
				return base.MultimediaLinksCount;
			}
		}*/

		private string GetStringTag(int Index)
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

		private void SetStringTag(int Index, [In] string Value)
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

		private TGEDCOMAddress GetAddress()
		{
			return base.TagClass("ADDR", typeof(TGEDCOMAddress)) as TGEDCOMAddress;
		}

		protected override void CreateObj(TGEDCOMObject AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			base.SetLists(TEnumSet.Create(new Enum[]
			{
				TGEDCOMSubList.stNotes, 
				TGEDCOMSubList.stSource, 
				TGEDCOMSubList.stMultimedia
			}));
			this.FLevel = (AParent as TGEDCOMCustomTag).Level;
		}

		public override TGEDCOMTag AddTag([In] string ATag, [In] string AValue, Type AClass)
		{
			TGEDCOMTag Result;
			if (ATag == "DATE")
			{
				Result = base.AddTag(ATag, AValue, typeof(TGEDCOMDateValue));
			}
			else
			{
				if (ATag == "PLAC")
				{
					Result = base.AddTag(ATag, AValue, typeof(TGEDCOMPlace));
				}
				else
				{
					if (ATag == "ADDR")
					{
						Result = base.AddTag(ATag, AValue, typeof(TGEDCOMAddress));
					}
					else
					{
						if (ATag == "PHON" || ATag == "EMAIL" || ATag == "FAX" || ATag == "WWW")
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

			this._Notes.SaveToStream(AStream);
			this._SourceCitations.SaveToStream(AStream);
			this._MultimediaLinks.SaveToStream(AStream);
		}

		public TGEDCOMEventDetail(TGEDCOMObject AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
