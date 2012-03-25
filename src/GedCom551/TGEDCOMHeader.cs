using System;
using System.Runtime.InteropServices;

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
			get { return this.GetNotes(); }
			set { this.SetNotes(value); }
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

		public TGEDCOMAddress SourceBusinessAddress
		{
			get { return this.GetSourceBusinessAddress(); }
		}

		public TGEDCOMPointer Submission
		{
			get { return this.GetSubmission(); }
		}

		public TGEDCOMPointer Submitter
		{
			get { return this.GetSubmittor(); }
		}

		public TGEDCOMDateExact TransmissionDate
		{
			get { return this.GetDate(); }
		}

		public TGEDCOMTime TransmissionTime
		{
			get { return this.GetTime(); }
		}

		public DateTime TransmissionDateTime
		{
			get { return this.GetTransmissionDateTime(); }
			set { this.SetTransmissionDateTime(value); }
		}

		// new property (not standard)
		public int FileRevision
		{
			get { return base.GetTagIntegerValue("FILE\\_REV", 0); }
			set { base.SetTagIntegerValue("FILE\\_REV", value); }
		}

		private TGEDCOMAddress GetSourceBusinessAddress()
		{
			TGEDCOMTag SourTag = base.FindTag("SOUR", 0);
			if (SourTag == null)
			{
				SourTag = this.AddTag("SOUR", "", null);
			}
			TGEDCOMTag CorpTag = SourTag.FindTag("CORP", 0);
			if (CorpTag == null)
			{
				CorpTag = SourTag.AddTag("CORP", "", null);
			}
			return CorpTag.TagClass("ADDR", typeof(TGEDCOMAddress), TGEDCOMAddress.Create) as TGEDCOMAddress;
		}

		private TGEDCOMDateExact GetDate()
		{
			return base.TagClass("DATE", typeof(TGEDCOMDateExact), TGEDCOMDateExact.Create) as TGEDCOMDateExact;
		}

		private TGEDCOMTime GetTime()
		{
			TGEDCOMTag DateTag = base.FindTag("DATE", 0);
			if (DateTag == null)
			{
				DateTag = this.AddTag("DATE", "", null);
			}
			return DateTag.TagClass("TIME", typeof(TGEDCOMTime), TGEDCOMTime.Create) as TGEDCOMTime;
		}

		private TGEDCOMPointer GetSubmittor()
		{
			return base.TagClass("SUBM", typeof(TGEDCOMPointer), TGEDCOMPointer.Create) as TGEDCOMPointer;
		}

		private TGEDCOMPointer GetSubmission()
		{
			return base.TagClass("SUBN", typeof(TGEDCOMPointer), TGEDCOMPointer.Create) as TGEDCOMPointer;
		}

		private StringList GetNotes()
		{
			return base.GetTagStrings(base.FindTag("NOTE", 0), ref this.FNotes);
		}

		private void SetNotes([In] StringList Value)
		{
			base.SetTagStrings(base.TagClass("NOTE", typeof(TGEDCOMNotes), TGEDCOMNotes.Create), Value);
		}

		private DateTime GetTransmissionDateTime()
		{
			return this.TransmissionDate.Date.Add(this.TransmissionTime.Time);
		}

		private void SetTransmissionDateTime([In] DateTime Value)
		{
			this.TransmissionDate.Date = Value.Date;
			this.TransmissionTime.Time = Value.TimeOfDay;
		}

		public override TGEDCOMTag AddSubTag(TGEDCOMCustomTag AParent, [In] string ATag, [In] string AValue, TagConstructor ATagConstructor)
		{
			TGEDCOMTag Result;
			if (object.Equals(AParent, base.FindTag("SOUR\\CORP", 0)) && (ATag == "PHON" || ATag == "EMAIL" || ATag == "FAX" || ATag == "WWW"))
			{
				if (base.FindTag("SOUR\\CORP\\ADDR", 0) == null)
				{
					base.SetTagStringValue("SOUR\\CORP\\ADDR", "");
				}
				Result = base.FindTag("SOUR\\CORP\\ADDR", 0).AddTag(ATag, AValue, ATagConstructor);
			}
			else
			{
				if (ATag == "ADDR")
				{
					Result = base.AddSubTag(AParent, ATag, AValue, TGEDCOMAddress.Create);
				}
				else
				{
					Result = base.AddSubTag(AParent, ATag, AValue, ATagConstructor);
				}
			}
			return Result;
		}

		protected override void CreateObj(TGEDCOMTree AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
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

		public override TGEDCOMTag AddTag([In] string ATag, [In] string AValue, TagConstructor ATagConstructor)
		{
			TGEDCOMTag Result;
			if (ATag == "DATE")
			{
				Result = base.AddTag(ATag, AValue, TGEDCOMDateExact.Create);
			}
			else
			{
				if (ATag == "SUBM")
				{
					Result = base.AddTag(ATag, AValue, TGEDCOMPointer.Create);
				}
				else
				{
					if (ATag == "SUBN")
					{
						Result = base.AddTag(ATag, AValue, TGEDCOMPointer.Create);
					}
					else
					{
						Result = base.AddTag(ATag, AValue, ATagConstructor);
					}
				}
			}
			return Result;
		}

		public TGEDCOMHeader(TGEDCOMTree AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
