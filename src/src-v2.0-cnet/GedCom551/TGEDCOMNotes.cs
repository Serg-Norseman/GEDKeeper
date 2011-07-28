using GKSys;
using System;
using System.ComponentModel;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GedCom551
{
	public class TGEDCOMNotes : TGEDCOMPointer
	{

		internal TStrings FNotes;
		[Browsable(false)]
		public bool IsPointer
		{
			get { return this.GetIsPointer(); }
		}

		[Browsable(false)]
		public TStrings Notes
		{
			get { return this.GetNotes(); }
			set { this.SetNotes(value); }
		}

		internal bool GetIsPointer()
		{
			return BDSSystem.WStrCmp(base.XRef, "") > 0;
		}
		internal TStrings GetNotes()
		{
			if (this.FNotes == null)
			{
				this.FNotes = new TStringList();
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
					this.FNotes.Assign((NotesRecord as TGEDCOMNoteRecord).Notes);
				}
			}
			return this.FNotes;
		}
		internal void SetNotes([In] TStrings Value)
		{
			this.Clear();
			base.SetTagStrings(this, Value);
		}
		protected internal override void CreateObj(TGEDCOMObject AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			this.FName = "NOTE";
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
				object fNotes = this.FNotes;
				VCLUtils.FreeAndNil(ref fNotes);
				this.FNotes = (fNotes as TStrings);
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

		public TGEDCOMNotes(TGEDCOMObject AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
