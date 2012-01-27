using System;
using System.IO;
using System.Runtime.InteropServices;

using GKSys;

namespace GedCom551
{
	public class TGEDCOMTagWithLists : TGEDCOMTag
	{
		private EnumSet FLists;
		protected TGEDCOMListEx<TGEDCOMNotes> _Notes;
		protected TGEDCOMListEx<TGEDCOMSourceCitation> _SourceCitations;
		protected TGEDCOMListEx<TGEDCOMMultimediaLink> _MultimediaLinks;

		public TGEDCOMListEx<TGEDCOMNotes> Notes
		{
			get { return this._Notes; }
		}

		public TGEDCOMListEx<TGEDCOMSourceCitation> SourceCitations
		{
			get { return this._SourceCitations; }
		}

		public TGEDCOMListEx<TGEDCOMMultimediaLink> MultimediaLinks
		{
			get { return this._MultimediaLinks; }
		}

		protected override void CreateObj(TGEDCOMTree AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			this.FLists = EnumSet.Create();

			this._Notes = new TGEDCOMListEx<TGEDCOMNotes>(this);
			this._SourceCitations = new TGEDCOMListEx<TGEDCOMSourceCitation>(this);
			this._MultimediaLinks = new TGEDCOMListEx<TGEDCOMMultimediaLink>(this);
		}

		protected void SetLists(EnumSet ALists)
		{
			this.FLists = ALists;
		}

		public override void Dispose()
		{
			if (!this.Disposed_)
			{
				this._Notes.Dispose();
				this._SourceCitations.Dispose();
				this._MultimediaLinks.Dispose();

				base.Dispose();
				this.Disposed_ = true;
			}
		}

		public override void Pack()
		{
			base.Pack();

			this._Notes.Pack();
			this._SourceCitations.Pack();
			this._MultimediaLinks.Pack();
		}

		public override void ReplaceXRefs(TXRefReplaceMap aMap)
		{
			base.ReplaceXRefs(aMap);

			this._Notes.ReplaceXRefs(aMap);
			this._SourceCitations.ReplaceXRefs(aMap);
			this._MultimediaLinks.ReplaceXRefs(aMap);
		}

		public override void ResetOwner(TGEDCOMTree AOwner)
		{
			base.ResetOwner(AOwner);

			this._Notes.ResetOwner(AOwner);
			this._SourceCitations.ResetOwner(AOwner);
			this._MultimediaLinks.ResetOwner(AOwner);
		}

		public override void SaveToStream(StreamWriter AStream)
		{
			base.SaveToStream(AStream);

			this._Notes.SaveToStream(AStream);
			this._SourceCitations.SaveToStream(AStream);
			this._MultimediaLinks.SaveToStream(AStream);
		}

		public override TGEDCOMTag AddTag([In] string ATag, [In] string AValue, TagConstructor ATagConstructor)
		{
			TGEDCOMTag Result;
			if (ATag == "NOTE" && this.FLists.InSet(TGEDCOMSubList.stNotes))
			{
				Result = this.Notes.Add(new TGEDCOMNotes(base.Owner, this, ATag, AValue));
			}
			else
			{
				if (ATag == "SOUR" && this.FLists.InSet(TGEDCOMSubList.stSource))
				{
					Result = this.SourceCitations.Add(new TGEDCOMSourceCitation(base.Owner, this, ATag, AValue));
				}
				else
				{
					if (ATag == "OBJE" && this.FLists.InSet(TGEDCOMSubList.stMultimedia))
					{
						Result = this.MultimediaLinks.Add(new TGEDCOMMultimediaLink(base.Owner, this, ATag, AValue));
					}
					else
					{
						Result = base.AddTag(ATag, AValue, ATagConstructor);
					}
				}
			}
			return Result;
		}

		public override void Clear()
		{
			base.Clear();

			this._Notes.Clear();
			this._SourceCitations.Clear();
			this._MultimediaLinks.Clear();
		}

		public override bool IsEmpty()
		{
			return base.IsEmpty() && this._Notes.Count == 0 && this._SourceCitations.Count == 0 && this._MultimediaLinks.Count == 0;
		}

		public TGEDCOMTagWithLists(TGEDCOMTree AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
