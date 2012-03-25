using System;
using System.IO;
using System.Runtime.InteropServices;

using Ext.Utils;

namespace GedCom551
{
	public sealed class TGEDCOMEventDetail : TGEDCOMTagWithLists
	{
		public string Classification
		{
			get { return base.GetTagStringValue("TYPE"); }
			set { base.SetTagStringValue("TYPE", value); }
		}

		public string Agency
		{
			get { return base.GetTagStringValue("AGNC"); }
			set { base.SetTagStringValue("AGNC", value); }
		}


		public string ReligiousAffilation
		{
			get { return base.GetTagStringValue("RELI"); }
			set { base.SetTagStringValue("RELI", value); }
		}

		public string Cause
		{
			get { return base.GetTagStringValue("CAUS"); }
			set { base.SetTagStringValue("CAUS", value); }
		}

		public TGEDCOMPlace Place
		{
			get { return base.TagClass("PLAC", typeof(TGEDCOMPlace), TGEDCOMPlace.Create) as TGEDCOMPlace; }
		}

		public TGEDCOMAddress Address
		{
			get { return base.TagClass("ADDR", typeof(TGEDCOMAddress), TGEDCOMAddress.Create) as TGEDCOMAddress; }
		}

		public TGEDCOMDateValue Date
		{
			get { return base.TagClass("DATE", typeof(TGEDCOMDateValue), TGEDCOMDateValue.Create) as TGEDCOMDateValue; }
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

		protected override void CreateObj(TGEDCOMTree AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			base.SetLists(EnumSet.Create(new Enum[] { TGEDCOMSubList.stNotes, TGEDCOMSubList.stSource, TGEDCOMSubList.stMultimedia }));
			this.FLevel = (AParent as TGEDCOMCustomTag).Level;
		}

		public override TGEDCOMTag AddTag([In] string ATag, [In] string AValue, TagConstructor ATagConstructor)
		{
			TGEDCOMTag Result;
			if (ATag == "DATE")
			{
				Result = base.AddTag(ATag, AValue, TGEDCOMDateValue.Create);
			}
			else
			{
				if (ATag == "PHON" || ATag == "EMAIL" || ATag == "FAX" || ATag == "WWW")
				{
					Result = this.Address.AddTag(ATag, AValue, ATagConstructor);
				}
				else
				{
					// define "PLAC", "ADDR" by default
					Result = base.AddTag(ATag, AValue, ATagConstructor);
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

		public TGEDCOMEventDetail(TGEDCOMTree AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
