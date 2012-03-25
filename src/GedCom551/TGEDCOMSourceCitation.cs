using System;
using System.Runtime.InteropServices;

using Ext.Utils;

namespace GedCom551
{
	public sealed class TGEDCOMSourceCitation : TGEDCOMPointer
	{
		private StringList FDescription;

		public bool IsPointer
		{
			get { return (!string.IsNullOrEmpty(base.XRef)); }
		}

		public StringList Description
		{
			get { return this.GetDescription(); }
			set { this.SetDescription(value); }
		}

		public string Page
		{
			get { return base.GetTagStringValue("PAGE"); }
			set { base.SetTagStringValue("PAGE", value); }
		}

		public int CertaintyAssessment
		{
			get { return base.GetTagIntegerValue("QUAY", 0); }
			set { base.SetTagIntegerValue("QUAY", value); }
		}

		private StringList GetDescription()
		{
			if (this.FDescription == null)
			{
				this.FDescription = new StringList();
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

		private void SetDescription([In] StringList Value)
		{
			this.Clear();
			base.SetTagStrings(this, Value);
		}

		protected override void CreateObj(TGEDCOMTree AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			this.FName = "SOUR";
		}

		protected override string GetStringValue()
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
				//this.FDescription.Dispose();
				this.FDescription = null;
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
				Result = (this.FStringValue == "" && base.Count == 0);
			}
			return Result;
		}

		public override string ParseString([In] string AString)
		{
			this.FStringValue = "";
			base.XRef = "";
			string Result = AString;
			if (!string.IsNullOrEmpty(Result))
			{
				Result = base.ExtractDelimiter(Result, 0);
				Result = base.ParseString(Result);
				if (!this.IsPointer)
				{
					this.FStringValue = Result;
					Result = "";
				}
			}
			return Result;
		}

		public TGEDCOMSourceCitation(TGEDCOMTree AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
