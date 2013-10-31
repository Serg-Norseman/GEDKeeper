using System;
using System.IO;

using Ext.Utils;

namespace GedCom551
{
	public sealed class TGEDCOMSourceRecord : TGEDCOMRecord
	{
		private StringList FTitle;
		private StringList FOriginator;
		private StringList FPublication;
		private StringList FText;

		private GEDCOMList<TGEDCOMRepositoryCitation> _RepositoryCitations;

		public TGEDCOMData Data
		{
			get { return base.TagClass("DATA", TGEDCOMData.Create) as TGEDCOMData; }
		}

		public StringList Originator
		{
			get { return base.GetTagStrings(base.TagClass("AUTH", TGEDCOMTag.Create), ref this.FOriginator); }
			set { base.SetTagStrings(base.TagClass("AUTH", TGEDCOMTag.Create), value); }
		}

		public StringList Title
		{
			get { return base.GetTagStrings(base.TagClass("TITL", TGEDCOMTag.Create), ref this.FTitle); }
			set { base.SetTagStrings(base.TagClass("TITL", TGEDCOMTag.Create), value); }
		}

		public string FiledByEntry
		{
			get { return base.GetTagStringValue("ABBR"); }
			set { base.SetTagStringValue("ABBR", value); }
		}

		public StringList Publication
		{
			get { return base.GetTagStrings(base.TagClass("PUBL", TGEDCOMTag.Create), ref this.FPublication); }
			set { base.SetTagStrings(base.TagClass("PUBL", TGEDCOMTag.Create), value); }
		}

		public StringList Text
		{
			get { return base.GetTagStrings(base.TagClass("TEXT", TGEDCOMTag.Create), ref this.FText); }
			set { base.SetTagStrings(base.TagClass("TEXT", TGEDCOMTag.Create), value); }
		}

		public GEDCOMList<TGEDCOMRepositoryCitation> RepositoryCitations
		{
			get { return this._RepositoryCitations; }
		}

		protected override void CreateObj(TGEDCOMTree owner, TGEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.FRecordType = TGEDCOMRecordType.rtSource;
			this.FName = "SOUR";

			this.FTitle = null;
			this.FOriginator = null;
			this.FPublication = null;
			this.FText = null;

			this._RepositoryCitations = new GEDCOMList<TGEDCOMRepositoryCitation>(this);
		}

		public override void Dispose()
		{
			if (!this.Disposed_)
			{
				if (this.FTitle != null) this.FTitle.Free();
				if (this.FOriginator != null) this.FOriginator.Free();
				if (this.FPublication != null) this.FPublication.Free();
				if (this.FText != null) this.FText.Free();

				this._RepositoryCitations.Dispose();

				base.Dispose();
				this.Disposed_ = true;
			}
		}

		public override TGEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
		{
			TGEDCOMTag result;

			if (tagName == "REPO")
			{
				result = this._RepositoryCitations.Add(new TGEDCOMRepositoryCitation(base.Owner, this, tagName, tagValue));
			}
			else if (tagName == "DATA")
			{
				result = base.AddTag(tagName, tagValue, TGEDCOMData.Create);
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
			this._RepositoryCitations.Clear();
		}

		public override bool IsEmpty()
		{
			return base.IsEmpty() && this._RepositoryCitations.Count == 0;
		}

		public override void MoveTo(TGEDCOMRecord aToRecord, bool aClearDest)
		{
			TGEDCOMSourceRecord toSource = aToRecord as TGEDCOMSourceRecord;
			StringList titl = new StringList();
			StringList orig = new StringList();
			StringList publ = new StringList();
			StringList text = new StringList();
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
                    (obj as TGEDCOMTag).ResetParent(toSource);
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

		public override void ReplaceXRefs(XRefReplacer aMap)
		{
			base.ReplaceXRefs(aMap);
			this._RepositoryCitations.ReplaceXRefs(aMap);
		}

		public override void ResetOwner(TGEDCOMTree AOwner)
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
			base.SetTagStrings(base.TagClass("AUTH", TGEDCOMTag.Create), Value);
		}

		public void SetTitleArray(params string[] Value)
		{
			base.SetTagStrings(base.TagClass("TITL", TGEDCOMTag.Create), Value);
		}

		public void SetPublicationArray(params string[] Value)
		{
			base.SetTagStrings(base.TagClass("PUBL", TGEDCOMTag.Create), Value);
		}

		public void SetTextArray(params string[] Value)
		{
			base.SetTagStrings(base.TagClass("TEXT", TGEDCOMTag.Create), Value);
		}

		public TGEDCOMSourceRecord(TGEDCOMTree owner, TGEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static TGEDCOMTag Create(TGEDCOMTree owner, TGEDCOMObject parent, string tagName, string tagValue)
		{
			return new TGEDCOMSourceRecord(owner, parent, tagName, tagValue);
		}

        #region Auxiliary

        public override bool aux_IsMatch(TGEDCOMRecord record, float matchThreshold, MatchParams matchParams)
		{
			bool match = false;

			if (record != null) {
				TGEDCOMSourceRecord source = record as TGEDCOMSourceRecord;

				string title1 = this.FiledByEntry;
				string title2 = source.FiledByEntry;

				match = (string.Compare(title1, title2, true) == 0);
			}

			return match;
		}

		public void aux_AddRepository(TGEDCOMRepositoryRecord aRepRec)
		{
			if (aRepRec != null) {
				TGEDCOMRepositoryCitation cit = new TGEDCOMRepositoryCitation(this.Owner, this, "", "");
				cit.Value = aRepRec;
				this.RepositoryCitations.Add(cit);
			}
		}

		#endregion
	}
}
