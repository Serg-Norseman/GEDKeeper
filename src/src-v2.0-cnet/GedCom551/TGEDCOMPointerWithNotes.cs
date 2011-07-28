using GKSys;
using System;
using System.ComponentModel;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GedCom551
{
	public class TGEDCOMPointerWithNotes : TGEDCOMPointer
	{
		internal TGEDCOMList FNotes;

		/*[Browsable(false)]
		public TGEDCOMNotes Notes
		{
			get
			{
				return this.GetNotes(Index);
			}
		}
		[Browsable(false)]
		public int NotesCount
		{
			get
			{
				return this.GetNotesCount();
			}
		}*/

		internal TGEDCOMNotes GetNote(int Index)
		{
			TGEDCOMNotes Result;
			if (this.FNotes == null)
			{
				Result = null;
			}
			else
			{
				Result = (this.FNotes[Index] as TGEDCOMNotes);
			}
			return Result;
		}
		internal int GetNotesCount()
		{
			int Result;
			if (this.FNotes == null)
			{
				Result = 0;
			}
			else
			{
				Result = this.FNotes.Count;
			}
			return Result;
		}
		protected internal override void CreateObj(TGEDCOMObject AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
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

		public TGEDCOMNotes AddNotes(TGEDCOMNotes ANotes)
		{
			if (this.FNotes == null)
			{
				this.FNotes = new TGEDCOMList(this);
			}
			if (ANotes != null)
			{
				this.FNotes.Add(ANotes);
			}
			return ANotes;
		}

		public override TGEDCOMTag AddTag([In] string ATag, [In] string AValue, Type AClass)
		{
			TGEDCOMTag Result;
			if (BDSSystem.WStrCmp(ATag, "NOTE") == 0)
			{
				Result = this.AddNotes(new TGEDCOMNotes(base.Owner, this, ATag, AValue));
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
			if (this.FNotes != null)
			{
				this.FNotes.Clear();
			}
		}
		public override bool IsEmpty()
		{
			return base.IsEmpty() && base.Count == 0 && this.GetNotesCount() == 0;
		}
		public override void ReplaceXRefs(TXRefReplaceMap aMap)
		{
			base.ReplaceXRefs(aMap);
			if (this.FNotes != null)
			{
				this.FNotes.ReplaceXRefs(aMap);
			}
		}
		public override void ResetOwner(TGEDCOMObject AOwner)
		{
			base.ResetOwner(AOwner);
			if (this.FNotes != null)
			{
				this.FNotes.ResetOwner(AOwner);
			}
		}

		public TGEDCOMPointerWithNotes(TGEDCOMObject AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
