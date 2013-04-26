using System;
using System.Runtime.InteropServices;

using Ext.Utils;

namespace GedCom551
{
	public sealed class TGEDCOMNotes : TGEDCOMPointer
	{
		private StringList FNotes;

		public bool IsPointer
		{
			get { return (!string.IsNullOrEmpty(base.XRef)); }
		}

		public StringList Notes
		{
			get { return this.GetNotes(); }
			set { this.SetNotes(value); }
		}

		private StringList GetNotes()
		{
			if (this.FNotes == null)
			{
				this.FNotes = new StringList();
			}
			else
			{
				this.FNotes.Clear();
			}
			if (!this.IsPointer)
			{
				base.GetTagStrings(this, ref this.FNotes);
			}
			else
			{
				TGEDCOMRecord NotesRecord = base.Value;
				if (NotesRecord != null && NotesRecord is TGEDCOMNoteRecord)
				{
					this.FNotes.Assign((NotesRecord as TGEDCOMNoteRecord).Note);
				}
			}
			return this.FNotes;
		}

		private void SetNotes([In] StringList Value)
		{
			this.Clear();
			base.SetTagStrings(this, Value);
		}

		protected override void CreateObj(TGEDCOMTree AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			this.FName = "NOTE";
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
				if (this.FNotes != null)
				{
					this.FNotes.Free();
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

		public TGEDCOMNotes(TGEDCOMTree AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}

        public new static TGEDCOMTag Create(TGEDCOMTree AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue)
		{
			return new TGEDCOMNotes(AOwner, AParent, AName, AValue);
		}
	}
}
