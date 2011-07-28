using GKSys;
using System;
using System.ComponentModel;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GedCom551
{
	public class TGEDCOMSourceCitation : TGEDCOMPointer
	{
		internal TStrings FDescription;

		public bool IsPointer
		{
			get { return this.GetIsPointer(); }
		}

		public TStrings Description
		{
			get { return this.GetDescription(); }
			set { this.SetDescription(value); }
		}

		public string Page
		{
			get { return this.GetPage(); }
			set { this.SetPage(value); }
		}

		public int CertaintyAssessment
		{
			get { return this.GetCertaintyAssessment(); }
			set { this.SetCertaintyAssessment(value); }
		}

		internal bool GetIsPointer()
		{
			return BDSSystem.WStrCmp(base.XRef, "") > 0;
		}

		internal TStrings GetDescription()
		{
			if (this.FDescription == null)
			{
				this.FDescription = new TStringList();
			}
			else
			{
				this.FDescription.Clear();
			}
			if (!this.IsPointer)
			{
				base.GetTagStrings(this, ref this.FDescription);
			}
			else
			{
				TGEDCOMRecord SourceRecord = base.Value;
				if (SourceRecord != null && SourceRecord is TGEDCOMSourceRecord)
				{
					this.FDescription.Assign((SourceRecord as TGEDCOMSourceRecord).Title);
				}
			}
			return this.FDescription;
		}
		internal void SetDescription([In] TStrings Value)
		{
			this.Clear();
			base.SetTagStrings(this, Value);
		}
		internal string GetPage()
		{
			return base.GetTagStringValue("PAGE");
		}
		internal void SetPage([In] string Value)
		{
			base.SetTagStringValue("PAGE", Value);
		}
		internal int GetCertaintyAssessment()
		{
			return base.GetTagIntegerValue("QUAY", 0);
		}
		internal void SetCertaintyAssessment([In] int Value)
		{
			base.SetTagIntegerValue("QUAY", Value);
		}
		protected internal override void CreateObj(TGEDCOMObject AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			this.FName = "SOUR";
		}
		protected internal override string GetStringValue()
		{
			string Result;
			if (this.IsPointer)
			{
				Result = base.GetStringValue();
			}
			else
			{
				Result = this.FStringValue;
			}
			return Result;
		}

		public override void Dispose()
		{
			if (!this.Disposed_)
			{
				if (this.FDescription != null)
				{
					this.FDescription.Free();
				}
				base.Dispose();
				this.Disposed_ = true;
			}
		}

		public override void Clear()
		{
			base.Clear();
			if (this.FDescription != null)
			{
				object fDescription = this.FDescription;
				VCLUtils.FreeAndNil(ref fDescription);
				this.FDescription = (fDescription as TStrings);
			}
		}
		public override bool IsEmpty()
		{
			bool Result;
			if (this.IsPointer)
			{
				Result = base.IsEmpty();
			}
			else
			{
				Result = (BDSSystem.WStrCmp(this.FStringValue, "") == 0 && base.Count == 0);
			}
			return Result;
		}
		public override string ParseString([In] string AString)
		{
			this.FStringValue = "";
			base.XRef = "";
			string Result = AString;
			Result = base.ExtractDelimiter(Result, 0);
			Result = base.ParseString(Result);
			if (!this.IsPointer)
			{
				this.FStringValue = Result;
				Result = "";
			}
			return Result;
		}

		public TGEDCOMSourceCitation(TGEDCOMObject AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
