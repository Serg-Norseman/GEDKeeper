using System;
using System.IO;
using System.Runtime.InteropServices;

using GKCore.Sys;

namespace GedCom551
{
	public sealed class TGEDCOMSourceRecord : TGEDCOMRecord
	{
		private TStrings FTitle;
		private TStrings FOriginator;
		private TStrings FPublication;
		private TStrings FText;

		private TGEDCOMListEx<TGEDCOMRepositoryCitation> _RepositoryCitations;

		public TGEDCOMData Data
		{
			get { return this.GetData(); }
		}

		public TStrings Originator
		{
			get { return this.GetOriginator(); }
			set { this.SetOriginator(value); }
		}

		public TStrings Title
		{
			get { return this.GetTitle(); }
			set { this.SetTitle(value); }
		}

		public string FiledByEntry
		{
			get { return base.GetTagStringValue("ABBR"); }
			set { base.SetTagStringValue("ABBR", value); }
		}

		public TStrings Publication
		{
			get { return this.GetPublication(); }
			set { this.SetPublication(value); }
		}

		public TStrings Text
		{
			get { return this.GetText(); }
			set { this.SetText(value); }
		}

		public TGEDCOMListEx<TGEDCOMRepositoryCitation> RepositoryCitations
		{
			get { return this._RepositoryCitations; }
		}

		/*public new TGEDCOMMultimediaLink MultimediaLinks
		{
			get { return base.GetMultimediaLink(Index); }
		}
		public new int MultimediaLinksCount
		{
			get { return base.MultimediaLinksCount; }
		}*/

		/*public new TGEDCOMNotes Notes
		{
			get	{ return base.GetNote(Index); }
		}
		public new int NotesCount
		{
			get { return base.NotesCount; }
		}*/

		private TStrings GetTitle()
		{
			return base.GetTagStrings(base.TagClass("TITL", typeof(TGEDCOMTag)), ref this.FTitle);
		}

		private void SetTitle(TStrings Value)
		{
			base.SetTagStrings(base.TagClass("TITL", typeof(TGEDCOMTag)), Value);
		}

		private TStrings GetOriginator()
		{
			return base.GetTagStrings(base.TagClass("AUTH", typeof(TGEDCOMTag)), ref this.FOriginator);
		}

		private void SetOriginator([In] TStrings Value)
		{
			base.SetTagStrings(base.TagClass("AUTH", typeof(TGEDCOMTag)), Value);
		}

		private TStrings GetPublication()
		{
			return base.GetTagStrings(base.TagClass("PUBL", typeof(TGEDCOMTag)), ref this.FPublication);
		}

		private TStrings GetText()
		{
			return base.GetTagStrings(base.TagClass("TEXT", typeof(TGEDCOMTag)), ref this.FText);
		}

		private void SetPublication([In] TStrings Value)
		{
			base.SetTagStrings(base.TagClass("PUBL", typeof(TGEDCOMTag)), Value);
		}

		private void SetText([In] TStrings Value)
		{
			base.SetTagStrings(base.TagClass("TEXT", typeof(TGEDCOMTag)), Value);
		}

		private TGEDCOMData GetData()
		{
			return base.TagClass("DATA", typeof(TGEDCOMData)) as TGEDCOMData;
		}

		protected override void CreateObj(TGEDCOMObject AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			base.SetLists(TEnumSet.Create(new Enum[]
			{
				TGEDCOMSubList.stNotes, 
				TGEDCOMSubList.stMultimedia
			}));
			this.FRecordType = TGEDCOMRecordType.rtSource;
			this.FName = "SOUR";

			this.FTitle = null;
			this.FOriginator = null;
			this.FPublication = null;
			this.FText = null;

			this._RepositoryCitations = new TGEDCOMListEx<TGEDCOMRepositoryCitation>(this);
		}

		public override void Dispose()
		{
			if (!this.Disposed_)
			{
				if (this.FTitle != null)
				{
					this.FTitle.Free();
				}
				if (this.FOriginator != null)
				{
					this.FOriginator.Free();
				}
				if (this.FPublication != null)
				{
					this.FPublication.Free();
				}
				if (this.FText != null)
				{
					this.FText.Free();
				}

				this._RepositoryCitations.Dispose();

				base.Dispose();
				this.Disposed_ = true;
			}
		}

		public override TGEDCOMTag AddTag([In] string ATag, [In] string AValue, Type AClass)
		{
			TGEDCOMTag Result;

			if (ATag == "REPO")
			{
				Result = this._RepositoryCitations.Add(new TGEDCOMRepositoryCitation(base.Owner, this, ATag, AValue));
			}
			else
			{
				if (ATag == "DATA")
				{
					Result = base.AddTag(ATag, AValue, typeof(TGEDCOMData));
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
			}

			return Result;
		}

		public override void Clear()
		{
			base.Clear();
			this._RepositoryCitations.Clear();
		}

		public override bool IsEmpty()
		{
			return base.IsEmpty() && this._RepositoryCitations.Count == 0;
		}

		public override void MoveTo(TGEDCOMRecord aToRecord, bool aClearDest)
		{
			TGEDCOMSourceRecord toSource = aToRecord as TGEDCOMSourceRecord;
			TStringList titl = new TStringList();
			TStringList orig = new TStringList();
			TStringList publ = new TStringList();
			TStringList text = new TStringList();
			try
			{
				titl.Text = (toSource.Title.Text + "\n" + this.Title.Text).Trim();
				orig.Text = (toSource.Originator.Text + "\n" + this.Originator.Text).Trim();
				publ.Text = (toSource.Publication.Text + "\n" + this.Publication.Text).Trim();
				text.Text = (toSource.Text.Text + "\n" + this.Text.Text).Trim();
				base.DeleteTag("TITL");
				base.DeleteTag("TEXT");
				base.DeleteTag("ABBR");
				base.DeleteTag("PUBL");
				base.DeleteTag("AUTH");
				base.MoveTo(aToRecord, aClearDest);
				toSource.Title = titl;
				toSource.Originator = orig;
				toSource.Publication = publ;
				toSource.Text = text;

				while (this._RepositoryCitations.Count > 0)
				{
					TGEDCOMObject obj = this._RepositoryCitations.Extract(0);
					(obj as TGEDCOMCustomTag).ResetParent(toSource);
					toSource.RepositoryCitations.Add(obj as TGEDCOMRepositoryCitation);
				}
			}
			finally
			{
				titl.Free();
				orig.Free();
				publ.Free();
				text.Free();
			}
		}

		public override void Pack()
		{
			base.Pack();
			this._RepositoryCitations.Pack();
		}

		public override void ReplaceXRefs(TXRefReplaceMap aMap)
		{
			base.ReplaceXRefs(aMap);
			this._RepositoryCitations.ReplaceXRefs(aMap);
		}

		public override void ResetOwner(TGEDCOMObject AOwner)
		{
			base.ResetOwner(AOwner);
			this._RepositoryCitations.ResetOwner(AOwner);
		}

		public override void SaveToStream(StreamWriter AStream)
		{
			base.SaveToStream(AStream);
			this._RepositoryCitations.SaveToStream(AStream);
		}

		public void SetOriginatorArray(params string[] Value)
		{
			base.SetTagStrings(base.TagClass("AUTH", typeof(TGEDCOMTag)), Value);
		}

		public void SetTitleArray(params string[] Value)
		{
			base.SetTagStrings(base.TagClass("TITL", typeof(TGEDCOMTag)), Value);
		}

		public void SetPublicationArray(params string[] Value)
		{
			base.SetTagStrings(base.TagClass("PUBL", typeof(TGEDCOMTag)), Value);
		}

		public void SetTextArray(params string[] Value)
		{
			base.SetTagStrings(base.TagClass("TEXT", typeof(TGEDCOMTag)), Value);
		}

		public TGEDCOMSourceRecord(TGEDCOMObject AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
