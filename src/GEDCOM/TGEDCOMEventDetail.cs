using System;
using System.IO;

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
			get { return base.TagClass("PLAC", TGEDCOMPlace.Create) as TGEDCOMPlace; }
		}

		public TGEDCOMAddress Address
		{
			get { return base.TagClass("ADDR", TGEDCOMAddress.Create) as TGEDCOMAddress; }
		}

		public TGEDCOMDateValue Date
		{
			get { return base.TagClass("DATE", TGEDCOMDateValue.Create) as TGEDCOMDateValue; }
		}

		public TGEDCOMRestriction Restriction
		{
			get { return GEDCOMUtils.GetRestrictionVal(base.GetTagStringValue("RESN")); }
			set { base.SetTagStringValue("RESN", GEDCOMUtils.GetRestrictionStr(value)); }
		}

		protected override void CreateObj(TGEDCOMTree owner, GEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.fLevel = (parent as TGEDCOMTag).Level;
		}

		public override TGEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
		{
			TGEDCOMTag result;

			if (tagName == "PHON" || tagName == "EMAIL" || tagName == "FAX" || tagName == "WWW")
			{
				result = this.Address.AddTag(tagName, tagValue, tagConstructor);
			}
			else
			{
				// define "PLAC", "ADDR", "DATE" by default
				result = base.AddTag(tagName, tagValue, tagConstructor);
			}

			return result;
		}

		public override void SaveToStream(StreamWriter stream)
		{
			this.SaveTagsToStream(stream);

			this.fNotes.SaveToStream(stream);
			this.fSourceCitations.SaveToStream(stream);
			this.fMultimediaLinks.SaveToStream(stream);
		}

		public TGEDCOMEventDetail(TGEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}
	}
}
