using System;
using Ext.Utils;

/// <summary>
/// Localization: clean
/// </summary>

namespace GedCom551
{
	public sealed class TGEDCOMHeader : TGEDCOMCustomRecord
	{
		private StringList FNotes;

		public TGEDCOMCharacterSet CharacterSet
		{
			get { return base.GetCharacterSetVal(base.GetTagStringValue("CHAR")); }
			set { base.SetTagStringValue("CHAR", base.GetCharacterSetStr(value)); }
		}

		public StringList Notes
		{
			get {
				return base.GetTagStrings(base.FindTag("NOTE", 0), ref this.FNotes);
			}
			set {
				base.SetTagStrings(base.TagClass("NOTE", typeof(TGEDCOMNotes), TGEDCOMNotes.Create), value);
			}
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
				TGEDCOMTag CorpTag = base.TagClass("SOUR\\CORP", typeof(TGEDCOMTag), TGEDCOMTag.Create);
				return CorpTag.TagClass("ADDR", typeof(TGEDCOMAddress), TGEDCOMAddress.Create) as TGEDCOMAddress;
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
			get { return base.TagClass("SUBN", typeof(TGEDCOMPointer), TGEDCOMPointer.Create) as TGEDCOMPointer; }
		}

		public TGEDCOMPointer Submitter
		{
			get { return base.TagClass("SUBM", typeof(TGEDCOMPointer), TGEDCOMPointer.Create) as TGEDCOMPointer; }
		}

		public TGEDCOMDateExact TransmissionDate
		{
			get { return base.TagClass("DATE", typeof(TGEDCOMDateExact), TGEDCOMDateExact.Create) as TGEDCOMDateExact; }
		}

		public TGEDCOMTime TransmissionTime
		{
			get { return this.TransmissionDate.TagClass("TIME", typeof(TGEDCOMTime), TGEDCOMTime.Create) as TGEDCOMTime; }
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

		protected override void CreateObj(TGEDCOMTree owner, TGEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.FName = "HEAD";
			this.FNotes = null;
		}

		public override void Dispose()
		{
			if (!this.Disposed_)
			{
				if (this.FNotes != null)
				{
					//this.FNotes.Dispose();
					this.FNotes = null;
				}
				base.Dispose();
				this.Disposed_ = true;
			}
		}

		public override void Clear()
		{
			base.Clear();
			if (this.FNotes != null)
			{
				//this.FNotes.Dispose();
				this.FNotes = null;
			}
		}

		public override TGEDCOMTag AddTag(string ATag, string AValue, TagConstructor ATagConstructor)
		{
			TGEDCOMTag Result;

			if (ATag == "DATE")
			{
				Result = base.AddTag(ATag, AValue, TGEDCOMDateExact.Create);
			}
			else if (ATag == "SUBM")
			{
				Result = base.AddTag(ATag, AValue, TGEDCOMPointer.Create);
			}
			else if (ATag == "SUBN")
			{
				Result = base.AddTag(ATag, AValue, TGEDCOMPointer.Create);
			}
			else
			{
				Result = base.AddTag(ATag, AValue, ATagConstructor);
			}

			return Result;
		}

		public TGEDCOMHeader(TGEDCOMTree owner, TGEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}
	}
}
