using GKSys;
using System;
using System.ComponentModel;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GedCom551
{
	public class TGEDCOMHeader : TGEDCOMCustomRecord
	{
		internal TStrings FNotes;

		[Browsable(false)]
		public TGEDCOMObject.TGEDCOMCharacterSet CharacterSet
		{
			get
			{
				return this.GetCharacterSet();
			}
			set
			{
				this.SetCharacterSet(value);
			}
		}
		[Browsable(false)]
		public string CharacterSetVersion
		{
			get
			{
				return this.GetStringTag(10);
			}
			set
			{
				this.SetStringTag(10, value);
			}
		}
		[Browsable(false)]
		public string Copyright
		{
			get
			{
				return this.GetStringTag(7);
			}
			set
			{
				this.SetStringTag(7, value);
			}
		}
		[Browsable(false)]
		public string FileName
		{
			get
			{
				return this.GetStringTag(6);
			}
			set
			{
				this.SetStringTag(6, value);
			}
		}
		[Browsable(false)]
		public string GEDCOMVersion
		{
			get
			{
				return this.GetStringTag(8);
			}
			set
			{
				this.SetStringTag(8, value);
			}
		}
		[Browsable(false)]
		public string GEDCOMForm
		{
			get
			{
				return this.GetStringTag(9);
			}
			set
			{
				this.SetStringTag(9, value);
			}
		}
		[Browsable(false)]
		public string Language
		{
			get
			{
				return this.GetStringTag(11);
			}
			set
			{
				this.SetStringTag(11, value);
			}
		}
		[Browsable(false)]
		public TStrings Notes
		{
			get
			{
				return this.GetNotes();
			}
			set
			{
				this.SetNotes(value);
			}
		}
		[Browsable(false)]
		public string PlaceHierarchy
		{
			get
			{
				return this.GetStringTag(12);
			}
			set
			{
				this.SetStringTag(12, value);
			}
		}
		[Browsable(false)]
		public string ReceivingSystemName
		{
			get
			{
				return this.GetStringTag(5);
			}
			set
			{
				this.SetStringTag(5, value);
			}
		}
		[Browsable(false)]
		public string Source
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
		public string SourceVersion
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
		public string SourceProductName
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
		public string SourceBusinessName
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
		public TGEDCOMAddress SourceBusinessAddress
		{
			get
			{
				return this.GetSourceBusinessAddress();
			}
		}
		[Browsable(false)]
		public TGEDCOMPointer Submission
		{
			get
			{
				return this.GetSubmission();
			}
		}
		[Browsable(false)]
		public TGEDCOMPointer Submitter
		{
			get
			{
				return this.GetSubmittor();
			}
		}
		[Browsable(false)]
		public TGEDCOMDateExact TransmissionDate
		{
			get
			{
				return this.GetDate();
			}
		}
		[Browsable(false)]
		public TGEDCOMTime TransmissionTime
		{
			get
			{
				return this.GetTime();
			}
		}
		[Browsable(false)]
		public DateTime TransmissionDateTime
		{
			get
			{
				return this.GetTransmissionDateTime();
			}
			set
			{
				this.SetTransmissionDateTime(value);
			}
		}
		internal string GetStringTag(int Index)
		{
			string Result = "";
			switch (Index)
			{
				case 1:
				{
					Result = base.GetTagStringValue("SOUR");
					break;
				}
				case 2:
				{
					Result = base.GetTagStringValue("SOUR\\VERS");
					break;
				}
				case 3:
				{
					Result = base.GetTagStringValue("SOUR\\NAME");
					break;
				}
				case 4:
				{
					Result = base.GetTagStringValue("SOUR\\CORP");
					break;
				}
				case 5:
				{
					Result = base.GetTagStringValue("DEST");
					break;
				}
				case 6:
				{
					Result = base.GetTagStringValue("FILE");
					break;
				}
				case 7:
				{
					Result = base.GetTagStringValue("COPR");
					break;
				}
				case 8:
				{
					Result = base.GetTagStringValue("GEDC\\VERS");
					break;
				}
				case 9:
				{
					Result = base.GetTagStringValue("GEDC\\FORM");
					break;
				}
				case 10:
				{
					Result = base.GetTagStringValue("CHAR\\VERS");
					break;
				}
				case 11:
				{
					Result = base.GetTagStringValue("LANG");
					break;
				}
				case 12:
				{
					Result = base.GetTagStringValue("PLAC\\FORM");
					break;
				}
			}
			return Result;
		}
		internal void SetStringTag(int Index, [In] string Value)
		{
			switch (Index)
			{
				case 1:
				{
					base.SetTagStringValue("SOUR", Value);
					break;
				}
				case 2:
				{
					base.SetTagStringValue("SOUR\\VERS", Value);
					break;
				}
				case 3:
				{
					base.SetTagStringValue("SOUR\\NAME", Value);
					break;
				}
				case 4:
				{
					base.SetTagStringValue("SOUR\\CORP", Value);
					break;
				}
				case 5:
				{
					base.SetTagStringValue("DEST", Value);
					break;
				}
				case 6:
				{
					base.SetTagStringValue("FILE", Value);
					break;
				}
				case 7:
				{
					base.SetTagStringValue("COPR", Value);
					break;
				}
				case 8:
				{
					base.SetTagStringValue("GEDC\\VERS", Value);
					break;
				}
				case 9:
				{
					base.SetTagStringValue("GEDC\\FORM", Value);
					break;
				}
				case 10:
				{
					base.SetTagStringValue("CHAR\\VERS", Value);
					break;
				}
				case 11:
				{
					base.SetTagStringValue("LANG", Value);
					break;
				}
				case 12:
				{
					base.SetTagStringValue("PLAC\\FORM", Value);
					break;
				}
			}
		}
		internal TGEDCOMAddress GetSourceBusinessAddress()
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
			return CorpTag.TagClass("ADDR", typeof(TGEDCOMAddress)) as TGEDCOMAddress;
		}
		internal TGEDCOMDateExact GetDate()
		{
			return base.TagClass("DATE", typeof(TGEDCOMDateExact)) as TGEDCOMDateExact;
		}
		internal TGEDCOMTime GetTime()
		{
			TGEDCOMTag DateTag = base.FindTag("DATE", 0);
			if (DateTag == null)
			{
				DateTag = this.AddTag("DATE", "", null);
			}
			return DateTag.TagClass("TIME", typeof(TGEDCOMTime)) as TGEDCOMTime;
		}
		internal TGEDCOMPointer GetSubmittor()
		{
			return base.TagClass("SUBM", typeof(TGEDCOMPointer)) as TGEDCOMPointer;
		}
		internal TGEDCOMPointer GetSubmission()
		{
			return base.TagClass("SUBN", typeof(TGEDCOMPointer)) as TGEDCOMPointer;
		}
		internal TGEDCOMObject.TGEDCOMCharacterSet GetCharacterSet()
		{
			string S = base.GetTagStringValue("CHAR").ToUpper();
			TGEDCOMObject.TGEDCOMCharacterSet Result;
			if (BDSSystem.WStrCmp(S, "ASCII") == 0 || BDSSystem.WStrCmp(S, "ANSI") == 0 || BDSSystem.WStrCmp(S, "IBMPC") == 0)
			{
				Result = TGEDCOMObject.TGEDCOMCharacterSet.csASCII;
			}
			else
			{
				if (BDSSystem.WStrCmp(S, "ANSEL") == 0)
				{
					Result = TGEDCOMObject.TGEDCOMCharacterSet.csANSEL;
				}
				else
				{
					if (BDSSystem.WStrCmp(S, "UNICODE") == 0)
					{
						Result = TGEDCOMObject.TGEDCOMCharacterSet.csUNICODE;
					}
					else
					{
						if (BDSSystem.WStrCmp(S, "UTF8") == 0 || BDSSystem.WStrCmp(S, "UTF-8") == 0)
						{
							Result = TGEDCOMObject.TGEDCOMCharacterSet.csUTF8;
						}
						else
						{
							Result = TGEDCOMObject.TGEDCOMCharacterSet.csANSEL;
						}
					}
				}
			}
			return Result;
		}
		internal void SetCharacterSet([In] TGEDCOMObject.TGEDCOMCharacterSet Value)
		{
			string S = "";
			if (Value != TGEDCOMObject.TGEDCOMCharacterSet.csASCII)
			{
				if (Value != TGEDCOMObject.TGEDCOMCharacterSet.csANSEL)
				{
					if (Value != TGEDCOMObject.TGEDCOMCharacterSet.csUNICODE)
					{
						if (Value == TGEDCOMObject.TGEDCOMCharacterSet.csUTF8)
						{
							S = "UTF-8";
						}
					}
					else
					{
						S = "UNICODE";
					}
				}
				else
				{
					S = "ANSEL";
				}
			}
			else
			{
				S = "ASCII";
			}
			base.SetTagStringValue("CHAR", S);
		}
		internal TStrings GetNotes()
		{
			return base.GetTagStrings(base.FindTag("NOTE", 0), ref this.FNotes);
		}
		internal void SetNotes([In] TStrings Value)
		{
			base.SetTagStrings(base.TagClass("NOTE", typeof(TGEDCOMNotes)), Value);
		}
		internal DateTime GetTransmissionDateTime()
		{
			return this.TransmissionDate.Date.Add(this.TransmissionTime.Time);
		}
		internal void SetTransmissionDateTime([In] DateTime Value)
		{
			this.TransmissionDate.Date = Value.Date;
			this.TransmissionTime.Time = Value.TimeOfDay;
		}

		protected internal override TGEDCOMTag AddSubTag(TGEDCOMCustomTag AParent, [In] string ATag, [In] string AValue, Type AClass)
		{
			TGEDCOMTag Result;
			if (object.Equals(AParent, base.FindTag("SOUR\\CORP", 0)) && (BDSSystem.WStrCmp(ATag, "PHON") == 0 || BDSSystem.WStrCmp(ATag, "EMAIL") == 0 || BDSSystem.WStrCmp(ATag, "FAX") == 0 || BDSSystem.WStrCmp(ATag, "WWW") == 0))
			{
				if (base.FindTag("SOUR\\CORP\\ADDR", 0) == null)
				{
					base.SetTagStringValue("SOUR\\CORP\\ADDR", "");
				}
				Result = base.FindTag("SOUR\\CORP\\ADDR", 0).AddTag(ATag, AValue, AClass);
			}
			else
			{
				if (BDSSystem.WStrCmp(ATag, "ADDR") == 0)
				{
					Result = base.AddSubTag(AParent, ATag, AValue, typeof(TGEDCOMAddress));
				}
				else
				{
					Result = base.AddSubTag(AParent, ATag, AValue, AClass);
				}
			}
			return Result;
		}
		protected internal override void CreateObj(TGEDCOMObject AOwner, TGEDCOMObject AParent)
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
					object fNotes = this.FNotes;
					VCLUtils.FreeAndNil(ref fNotes);
					this.FNotes = (fNotes as TStrings);
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
				object fNotes = this.FNotes;
				VCLUtils.FreeAndNil(ref fNotes);
				this.FNotes = (fNotes as TStrings);
			}
		}

		public override TGEDCOMTag AddTag([In] string ATag, [In] string AValue, Type AClass)
		{
			TGEDCOMTag Result;
			if (ATag == "DATE")
			{
				Result = base.AddTag(ATag, AValue, typeof(TGEDCOMDateExact));
			}
			else
			{
				if (ATag == "SUBM")
				{
					Result = base.AddTag(ATag, AValue, typeof(TGEDCOMPointer));
				}
				else
				{
					if (ATag == "SUBN")
					{
						Result = base.AddTag(ATag, AValue, typeof(TGEDCOMPointer));
					}
					else
					{
						Result = base.AddTag(ATag, AValue, AClass);
					}
				}
			}
			return Result;
		}

		public TGEDCOMHeader(TGEDCOMObject AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
