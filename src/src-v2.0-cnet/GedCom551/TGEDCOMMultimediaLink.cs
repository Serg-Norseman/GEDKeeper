using GKSys;
using System;
using System.ComponentModel;
using System.IO;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GedCom551
{
	public class TGEDCOMMultimediaLink : TGEDCOMPointer
	{
		internal TGEDCOMList FFileReferences;

		/*public TGEDCOMFileReference FileReferences
		{
			get
			{
				return this.GetFileReferences(Index);
			}
		}

		[Browsable(false)]
		public int FileReferencesCount
		{
			get
			{
				return this.GetFileReferencesCount();
			}
		}*/

		public bool IsPointer
		{
			get { return this.GetIsPointer(); }
		}

		public string Title
		{
			get { return this.GetStringTag(1); }
			set { this.SetStringTag(1, value); }
		}

		public bool IsPrimary
		{
			get { return this.GetIsPrimary(); }
			set { this.SetIsPrimary(value); }
		}

		internal bool GetIsPointer()
		{
			return BDSSystem.WStrCmp(base.XRef, "") > 0;
		}

		internal string GetStringTag(int Index)
		{
			string Result = "";
			if (Index == 1)
			{
				Result = base.GetTagStringValue("TITL");
			}
			return Result;
		}

		internal void SetStringTag(int Index, [In] string Value)
		{
			if (Index == 1)
			{
				base.SetTagStringValue("TITL", Value);
			}
		}

		internal TGEDCOMFileReference GetFileReference(int Index)
		{
			TGEDCOMFileReference Result;
			if (this.FFileReferences == null)
			{
				Result = null;
			}
			else
			{
				Result = (this.FFileReferences[Index] as TGEDCOMFileReference);
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

		internal bool GetIsPrimary()
		{
			TGEDCOMTag tag = base.FindTag("_PRIM", 0);
			return tag != null && tag.StringValue.Equals("Y");
		}

		internal void SetIsPrimary([In] bool Value)
		{
			if (Value)
			{
				TGEDCOMTag tag = base.FindTag("_PRIM", 0);
				if (tag == null)
				{
					tag = this.AddTag("_PRIM", "", null);
				}
				tag.StringValue = "Y";
			}
			else
			{
				base.DeleteTag("_PRIM");
			}
		}

		protected internal override void CreateObj(TGEDCOMObject AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			this.FName = "OBJE";
			this.FFileReferences = null;
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
				if (this.FFileReferences != null)
				{
					this.FFileReferences.Free();
				}
				base.Dispose();
				this.Disposed_ = true;
			}
		}

		public TGEDCOMFileReference AddFileReference(TGEDCOMFileReference AFileReference)
		{
			if (this.FFileReferences == null)
			{
				this.FFileReferences = new TGEDCOMList(this);
			}
			if (AFileReference != null)
			{
				this.FFileReferences.Add(AFileReference);
			}
			return AFileReference;
		}

		public override TGEDCOMTag AddTag([In] string ATag, [In] string AValue, Type AClass)
		{
			TGEDCOMTag Result;
			if (ATag.Equals("FILE"))
			{
				Result = this.AddFileReference(new TGEDCOMFileReference(base.Owner, this, ATag, AValue));
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
			if (this.FFileReferences != null)
			{
				this.FFileReferences.Clear();
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
				Result = (base.Count == 0 && (this.FFileReferences == null || this.FFileReferences.Count == 0));
			}
			return Result;
		}

		public override string ParseString([In] string AString)
		{
			this.FStringValue = "";
			return base.ParseString(AString);
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

		public TGEDCOMMultimediaLink(TGEDCOMObject AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
