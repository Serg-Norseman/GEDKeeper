using System;
using System.IO;

namespace GedCom551
{
	public sealed class TGEDCOMMultimediaRecord : TGEDCOMRecord
	{
		private GEDCOMList<TGEDCOMFileReferenceWithTitle> fFileReferences;

		public GEDCOMList<TGEDCOMFileReferenceWithTitle> FileReferences
		{
			get { return this.fFileReferences; }
		}

		protected override void CreateObj(TGEDCOMTree owner, GEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.fRecordType = TGEDCOMRecordType.rtMultimedia;
			this.fName = "OBJE";

			this.fFileReferences = new GEDCOMList<TGEDCOMFileReferenceWithTitle>(this);
		}

		protected override void Dispose(bool disposing)
		{
			if (disposing)
			{
				this.fFileReferences.Dispose();
			}
			base.Dispose(disposing);
		}

		public override TGEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
		{
			TGEDCOMTag result;

			if (tagName == "FILE")
			{
				result = this.fFileReferences.Add(new TGEDCOMFileReferenceWithTitle(base.Owner, this, tagName, tagValue));
			}
			else
			{
				result = base.AddTag(tagName, tagValue, tagConstructor);
			}

			return result;
		}

		public override void Clear()
		{
			base.Clear();
			this.fFileReferences.Clear();
		}

		public override bool IsEmpty()
		{
			return base.IsEmpty() && this.fFileReferences.Count == 0;
		}

		public override void Pack()
		{
			base.Pack();
			this.fFileReferences.Pack();
		}

        public override void ReplaceXRefs(XRefReplacer map)
		{
            base.ReplaceXRefs(map);
            this.fFileReferences.ReplaceXRefs(map);
		}

		public override void ResetOwner(TGEDCOMTree newOwner)
		{
			base.ResetOwner(newOwner);
			this.fFileReferences.ResetOwner(newOwner);
		}

		public override void SaveToStream(StreamWriter stream)
		{
			base.SaveToStream(stream);
			this.fFileReferences.SaveToStream(stream);
		}

		public TGEDCOMMultimediaRecord(TGEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static TGEDCOMTag Create(TGEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
		{
			return new TGEDCOMMultimediaRecord(owner, parent, tagName, tagValue);
		}

        #region Auxiliary

        public string aux_GetTitle()
        {
        	if (this.fFileReferences.Count > 0) {
        		TGEDCOMFileReferenceWithTitle fileRef = this.fFileReferences[0];
        		return fileRef.Title;
        	}
        	
        	return string.Empty;
        }
        
        #endregion
	}
}
