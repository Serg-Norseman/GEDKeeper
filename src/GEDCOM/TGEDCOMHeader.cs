using System;
using ExtUtils;

/// <summary>
/// 
/// </summary>

namespace GedCom551
{
	public sealed class TGEDCOMHeader : TGEDCOMCustomRecord
	{

		public TGEDCOMCharacterSet CharacterSet
		{
			get { return GEDCOMUtils.GetCharacterSetVal(base.GetTagStringValue("CHAR")); }
			set { base.SetTagStringValue("CHAR", GEDCOMUtils.GetCharacterSetStr(value)); }
		}

		public StringList Notes
		{
			get { return base.GetTagStrings(base.FindTag("NOTE", 0)); }
			set { base.SetTagStrings(base.TagClass("NOTE", TGEDCOMNotes.Create), value); }
		}

		public string Source
		{
			get { return base.GetTagStringValue("SOUR"); }
			set { base.SetTagStringValue("SOUR", value); }
		}

		public string SourceVersion
		{
			get { return base.GetTagStringValue("SOUR\\VERS"); }
			set { base.SetTagStringValue("SOUR\\VERS", value); }
		}

		public string SourceProductName
		{
			get { return base.GetTagStringValue("SOUR\\NAME"); }
			set { base.SetTagStringValue("SOUR\\NAME", value); }
		}

		public string SourceBusinessName
		{
			get { return base.GetTagStringValue("SOUR\\CORP"); }
			set { base.SetTagStringValue("SOUR\\CORP", value); }
		}

		public TGEDCOMAddress SourceBusinessAddress
		{
			get {
				TGEDCOMTag corpTag = base.TagClass("SOUR\\CORP", TGEDCOMTag.Create);
				return corpTag.TagClass("ADDR", TGEDCOMAddress.Create) as TGEDCOMAddress;
			}
		}

		public string ReceivingSystemName
		{
			get { return base.GetTagStringValue("DEST"); }
			set { base.SetTagStringValue("DEST", value); }
		}

		public string FileName
		{
			get { return base.GetTagStringValue("FILE"); }
			set { base.SetTagStringValue("FILE", value); }
		}

		public string Copyright
		{
			get { return base.GetTagStringValue("COPR"); }
			set { base.SetTagStringValue("COPR", value); }
		}

		public string GEDCOMVersion
		{
			get { return base.GetTagStringValue("GEDC\\VERS"); }
			set { base.SetTagStringValue("GEDC\\VERS", value); }
		}

		public string GEDCOMForm
		{
			get { return base.GetTagStringValue("GEDC\\FORM"); }
			set { base.SetTagStringValue("GEDC\\FORM", value); }
		}

		public string CharacterSetVersion
		{
			get { return base.GetTagStringValue("CHAR\\VERS"); }
			set { base.SetTagStringValue("CHAR\\VERS", value); }
		}

		public string Language
		{
			get { return base.GetTagStringValue("LANG"); }
			set { base.SetTagStringValue("LANG", value); }
		}

		public string PlaceHierarchy
		{
			get { return base.GetTagStringValue("PLAC\\FORM"); }
			set { base.SetTagStringValue("PLAC\\FORM", value); }
		}

		public TGEDCOMPointer Submission
		{
			get { return base.TagClass("SUBN", TGEDCOMPointer.Create) as TGEDCOMPointer; }
		}

		public TGEDCOMPointer Submitter
		{
			get { return base.TagClass("SUBM", TGEDCOMPointer.Create) as TGEDCOMPointer; }
		}

		public TGEDCOMDateExact TransmissionDate
		{
			get { return base.TagClass("DATE", TGEDCOMDateExact.Create) as TGEDCOMDateExact; }
		}

		public TGEDCOMTime TransmissionTime
		{
			get { return this.TransmissionDate.TagClass("TIME", TGEDCOMTime.Create) as TGEDCOMTime; }
		}

		public DateTime TransmissionDateTime
		{
			get {
				return this.TransmissionDate.Date.Add(this.TransmissionTime.Value);
			}
			set {
				this.TransmissionDate.Date = value.Date;
				this.TransmissionTime.Value = value.TimeOfDay;
			}
		}

		// new property (not standard)
		public int FileRevision
		{
			get { return base.GetTagIntegerValue("FILE\\_REV", 0); }
			set { base.SetTagIntegerValue("FILE\\_REV", value); }
		}

		protected override void CreateObj(TGEDCOMTree owner, GEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.fName = "HEAD";
		}

        protected override void Dispose(bool disposing)
		{
			if (disposing)
			{
			}
            base.Dispose(disposing);
		}

		public override void Clear()
		{
			base.Clear();
		}

		public override TGEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
		{
			TGEDCOMTag result;

			if (tagName == "DATE")
			{
				result = base.AddTag(tagName, tagValue, TGEDCOMDateExact.Create);
			}
			else if (tagName == "SUBM")
			{
				result = base.AddTag(tagName, tagValue, TGEDCOMPointer.Create);
			}
			else if (tagName == "SUBN")
			{
				result = base.AddTag(tagName, tagValue, TGEDCOMPointer.Create);
			}
			else
			{
				result = base.AddTag(tagName, tagValue, tagConstructor);
			}

			return result;
		}

		public TGEDCOMHeader(TGEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}
	}
}
