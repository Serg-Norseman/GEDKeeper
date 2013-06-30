using System;
using System.IO;

namespace GedCom551
{
	public sealed class TGEDCOMMultimediaRecord : TGEDCOMRecord
	{
		private GEDCOMList<TGEDCOMFileReferenceWithTitle> _FileReferences;

		public GEDCOMList<TGEDCOMFileReferenceWithTitle> FileReferences
		{
			get { return this._FileReferences; }
		}

		protected override void CreateObj(TGEDCOMTree owner, TGEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.FRecordType = TGEDCOMRecordType.rtMultimedia;
			this.FName = "OBJE";

			this._FileReferences = new GEDCOMList<TGEDCOMFileReferenceWithTitle>(this);
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

		public override TGEDCOMTag AddTag(string ATag, string AValue, TagConstructor ATagConstructor)
		{
			TGEDCOMTag Result;
			if (ATag == "FILE")
			{
				Result = this._FileReferences.Add(new TGEDCOMFileReferenceWithTitle(base.Owner, this, ATag, AValue));
			}
			else
			{
				Result = base.AddTag(ATag, AValue, ATagConstructor);
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

		public override void ResetOwner(TGEDCOMTree AOwner)
		{
			base.ResetOwner(AOwner);
			this._FileReferences.ResetOwner(AOwner);
		}

		public override void SaveToStream(StreamWriter AStream)
		{
			base.SaveToStream(AStream);
			this._FileReferences.SaveToStream(AStream);
		}

		public TGEDCOMMultimediaRecord(TGEDCOMTree owner, TGEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static TGEDCOMTag Create(TGEDCOMTree owner, TGEDCOMObject parent, string tagName, string tagValue)
		{
			return new TGEDCOMMultimediaRecord(owner, parent, tagName, tagValue);
		}
	}
}
