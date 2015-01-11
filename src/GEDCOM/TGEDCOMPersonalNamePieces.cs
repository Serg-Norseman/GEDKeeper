using System;
using System.IO;

/// <summary>
/// 
/// </summary>

namespace GedCom551
{
	public sealed class TGEDCOMPersonalNamePieces : TGEDCOMTagWithLists
	{
		public string Prefix
		{
			get { return base.GetTagStringValue("NPFX"); }
			set { base.SetTagStringValue("NPFX", value); }
		}

		public string Given
		{
			get { return base.GetTagStringValue("GIVN"); }
			set { base.SetTagStringValue("GIVN", value); }
		}

		public string Nickname
		{
			get { return base.GetTagStringValue("NICK"); }
			set { base.SetTagStringValue("NICK", value); }
		}

		public string SurnamePrefix
		{
			get { return base.GetTagStringValue("SPFX"); }
			set { base.SetTagStringValue("SPFX", value); }
		}

		public string Surname
		{
			get { return base.GetTagStringValue("SURN"); }
			set { base.SetTagStringValue("SURN", value); }
		}

		public string Suffix
		{
			get { return base.GetTagStringValue("NSFX"); }
			set { base.SetTagStringValue("NSFX", value); }
		}



		public string PatronymicName
		{
			get { return base.GetTagStringValue("_PATN"); }
			set { base.SetTagStringValue("_PATN", value); }
		}

		// as BKW6
		public string MarriedName
		{
			get { return base.GetTagStringValue("_MARN"); }
			set { base.SetTagStringValue("_MARN", value); }
		}

		// as BKW6
		public string ReligiousName
		{
			get { return base.GetTagStringValue("_RELN"); }
			set { base.SetTagStringValue("_RELN", value); }
		}

		// as BKW6
		public string CensusName
		{
			get { return base.GetTagStringValue("_CENN"); }
			set { base.SetTagStringValue("_CENN", value); }
		}


		public override void SaveToStream(StreamWriter stream)
		{
			this.SaveTagsToStream(stream);

			this.fNotes.SaveToStream(stream);
			this.fSourceCitations.SaveToStream(stream);
		}

		public TGEDCOMPersonalNamePieces(TGEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}
	}
}
