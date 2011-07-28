using GKSys;
using System;
using System.ComponentModel;
using System.IO;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GedCom551
{
	public class TGEDCOMMultimediaRecord : TGEDCOMRecord
	{
		internal TGEDCOMList FFileReferences;

		[Browsable(false)]
		public string AutomatedRecordID
		{
			get { return this.GetStringTag(1); }
			set { this.SetStringTag(1, value); }
		}

		/*public TGEDCOMFileReferenceWithTitle FileReferences
		{
			get { return this.GetFileReference(Index); }
		}
		public int FileReferencesCount
		{
			get { return this.GetFileReferencesCount(); }
		}*/

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
		internal TGEDCOMFileReferenceWithTitle GetFileReference(int Index)
		{
			TGEDCOMFileReferenceWithTitle Result;
			if (this.FFileReferences == null)
			{
				Result = null;
			}
			else
			{
				Result = (this.FFileReferences[Index] as TGEDCOMFileReferenceWithTitle);
			}
			return Result;
		}
		internal int GetFileReferencesCount()
		{
			int Result;
			if (this.FFileReferences == null)
			{
				Result = 0;
			}
			else
			{
				Result = this.FFileReferences.Count;
			}
			return Result;
		}
		protected internal override void CreateObj(TGEDCOMObject AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			base.SetLists(TEnumSet.Create(new Enum[]
			{
				TGEDCOMObject.TGEDCOMSubList.stNotes, 
				TGEDCOMObject.TGEDCOMSubList.stSource
			}));
			this.FRecordType = TGEDCOMRecord.TGEDCOMRecordType.rtMultimedia;
			this.FName = "OBJE";
			this.FFileReferences = null;
		}

		public override void Dispose()
		{
			if (!this.Disposed_)
			{
				if (this.FFileReferences != null)
				{
					this.FFileReferences.Free();
				}
				base.Dispose();
				this.Disposed_ = true;
			}
		}

		public override TGEDCOMTag AddTag([In] string ATag, [In] string AValue, Type AClass)
		{
			TGEDCOMTag Result;
			if (ATag.Equals("FILE"))
			{
				Result = this.AddFileReference(new TGEDCOMFileReferenceWithTitle(base.Owner, this, ATag, AValue));
			}
			else
			{
				if (ATag.Equals("REFN"))
				{
					Result = base.AddUserReference(new TGEDCOMUserReference(base.Owner, this, ATag, AValue));
				}
				else
				{
					Result = base.AddTag(ATag, AValue, AClass);
				}
			}
			return Result;
		}

		public TGEDCOMFileReferenceWithTitle AddFileReference(TGEDCOMFileReferenceWithTitle Value)
		{
			if (this.FFileReferences == null)
			{
				this.FFileReferences = new TGEDCOMList(this);
			}
			if (Value != null)
			{
				this.FFileReferences.Add(Value);
			}
			return Value;
		}

		public override void Clear()
		{
			base.Clear();
			if (this.FFileReferences != null)
			{
				this.FFileReferences.Clear();
			}
			if (this.FNotes != null)
			{
				this.FNotes.Clear();
			}
			if (this.FSourceCitations != null)
			{
				this.FSourceCitations.Clear();
			}
		}

		public override bool IsEmpty()
		{
			return base.IsEmpty() && this.GetFileReferencesCount() == 0 && base.GetNotesCount() == 0 && base.GetSourceCitationsCount() == 0;
		}

		public override void Pack()
		{
			base.Pack();
			if (this.FFileReferences != null)
			{
				this.FFileReferences.Pack();
			}
		}
		public override void ReplaceXRefs(TXRefReplaceMap aMap)
		{
			base.ReplaceXRefs(aMap);
			if (this.FFileReferences != null)
			{
				this.FFileReferences.ReplaceXRefs(aMap);
			}
		}
		public override void ResetOwner(TGEDCOMObject AOwner)
		{
			base.ResetOwner(AOwner);
			if (this.FFileReferences != null)
			{
				this.FFileReferences.ResetOwner(AOwner);
			}
		}
		public override void SaveToStream(StreamWriter AStream)
		{
			base.SaveToStream(AStream);
			if (this.FFileReferences != null)
			{
				this.FFileReferences.SaveToStream(AStream);
			}
		}

		public TGEDCOMMultimediaRecord(TGEDCOMObject AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
