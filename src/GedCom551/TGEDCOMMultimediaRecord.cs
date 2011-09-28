using System;
using System.IO;
using System.Runtime.InteropServices;

using GKCore.Sys;

namespace GedCom551
{
	public sealed class TGEDCOMMultimediaRecord : TGEDCOMRecord
	{
		private TGEDCOMListEx<TGEDCOMFileReferenceWithTitle> _FileReferences;

		public TGEDCOMListEx<TGEDCOMFileReferenceWithTitle> FileReferences
		{
			get { return this._FileReferences; }
		}

		protected override void CreateObj(TGEDCOMObject AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			base.SetLists(TEnumSet.Create(new Enum[]
			{
				TGEDCOMSubList.stNotes, 
				TGEDCOMSubList.stSource
			}));
			this.FRecordType = TGEDCOMRecordType.rtMultimedia;
			this.FName = "OBJE";

			this._FileReferences = new TGEDCOMListEx<TGEDCOMFileReferenceWithTitle>(this);
		}

		public override void Dispose()
		{
			if (!this.Disposed_)
			{
				this._FileReferences.Dispose();

				base.Dispose();
				this.Disposed_ = true;
			}
		}

		public override TGEDCOMTag AddTag([In] string ATag, [In] string AValue, Type AClass)
		{
			TGEDCOMTag Result;
			if (ATag == "FILE")
			{
				Result = this._FileReferences.Add(new TGEDCOMFileReferenceWithTitle(base.Owner, this, ATag, AValue));
			}
			else
			{
				if (ATag == "REFN")
				{
					Result = base.UserReferences.Add(new TGEDCOMUserReference(base.Owner, this, ATag, AValue));
				}
				else
				{
					Result = base.AddTag(ATag, AValue, AClass);
				}
			}
			return Result;
		}

		public override void Clear()
		{
			base.Clear();
			this._FileReferences.Clear();
		}

		public override bool IsEmpty()
		{
			return base.IsEmpty() && this._FileReferences.Count == 0;
		}

		public override void Pack()
		{
			base.Pack();
			this._FileReferences.Pack();
		}

		public override void ReplaceXRefs(TXRefReplaceMap aMap)
		{
			base.ReplaceXRefs(aMap);
			this._FileReferences.ReplaceXRefs(aMap);
		}

		public override void ResetOwner(TGEDCOMObject AOwner)
		{
			base.ResetOwner(AOwner);
			this._FileReferences.ResetOwner(AOwner);
		}

		public override void SaveToStream(StreamWriter AStream)
		{
			base.SaveToStream(AStream);
			this._FileReferences.SaveToStream(AStream);
		}

		public TGEDCOMMultimediaRecord(TGEDCOMObject AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
