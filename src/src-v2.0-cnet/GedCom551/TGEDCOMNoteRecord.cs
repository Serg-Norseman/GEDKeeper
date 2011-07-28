using GKSys;
using System;
using System.ComponentModel;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GedCom551
{
	public class TGEDCOMNoteRecord : TGEDCOMRecord
	{

		internal new TStrings FNotes;

		public string AutomatedRecordID
		{
			get { return this.GetStringTag(1); }
			set { this.SetStringTag(1, value); }
		}

		public new TStrings Notes
		{
			get { return this.GetNotes(); }
			set { this.SetNotes(value); }
		}

		internal TStrings GetNotes()
		{
			return base.GetTagStrings(this, ref this.FNotes);
		}
		internal void SetNotes(TStrings Value)
		{
			base.SetTagStrings(this, Value);
		}
		internal string GetStringTag(int Index)
		{
			string Result = "";
			if (Index == 1)
			{
				Result = base.GetTagStringValue("RIN");
			}
			return Result;
		}
		internal void SetStringTag(int Index, [In] string Value)
		{
			if (Index == 1)
			{
				base.SetTagStringValue("RIN", Value);
			}
		}
		protected internal override void CreateObj(TGEDCOMObject AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			base.SetLists(TEnumSet.Create(new Enum[]
			{
				TGEDCOMObject.TGEDCOMSubList.stSource
			}));
			this.FRecordType = TGEDCOMRecord.TGEDCOMRecordType.rtNote;
			this.FName = "NOTE";
			this.FNotes = null;
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

		public override TGEDCOMTag AddTag([In] string ATag, [In] string AValue, Type AClass)
		{
			TGEDCOMTag Result;
			if (BDSSystem.WStrCmp(ATag, "REFN") == 0)
			{
				Result = base.AddUserReference(new TGEDCOMUserReference(base.Owner, this, ATag, AValue));
			}
			else
			{
				Result = base.AddTag(ATag, AValue, AClass);
			}
			return Result;
		}
		public override void Clear()
		{
			base.Clear();
		}
		public override bool IsEmpty()
		{
			return base.IsEmpty();
		}
		public override void MoveTo(TGEDCOMRecord aToRecord, bool aClearDest)
		{
			TStringList cont = new TStringList();
			try
			{
				cont.Text = (aToRecord as TGEDCOMNoteRecord).Notes.Text;
				base.MoveTo(aToRecord, aClearDest);
				(aToRecord as TGEDCOMNoteRecord).Notes = cont;
			}
			finally
			{
				cont.Free();
			}
		}
		public override void ReplaceXRefs(TXRefReplaceMap aMap)
		{
			base.ReplaceXRefs(aMap);
		}
		public override void ResetOwner(TGEDCOMObject AOwner)
		{
			base.ResetOwner(AOwner);
		}

		public TGEDCOMNoteRecord(TGEDCOMObject AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
