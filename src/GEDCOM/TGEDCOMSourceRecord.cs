using System;
using System.IO;

using ExtUtils;

namespace GedCom551
{
	public sealed class TGEDCOMSourceRecord : TGEDCOMRecord
	{
		private GEDCOMList<TGEDCOMRepositoryCitation> fRepositoryCitations;

		public TGEDCOMData Data
		{
			get { return base.TagClass("DATA", TGEDCOMData.Create) as TGEDCOMData; }
		}

		public StringList Originator
		{
			get { return base.GetTagStrings(base.TagClass("AUTH", TGEDCOMTag.Create)); }
			set { base.SetTagStrings(base.TagClass("AUTH", TGEDCOMTag.Create), value); }
		}

		public StringList Title
		{
			get { return base.GetTagStrings(base.TagClass("TITL", TGEDCOMTag.Create)); }
			set { base.SetTagStrings(base.TagClass("TITL", TGEDCOMTag.Create), value); }
		}

		public string FiledByEntry
		{
			get { return base.GetTagStringValue("ABBR"); }
			set { base.SetTagStringValue("ABBR", value); }
		}

		public StringList Publication
		{
			get { return base.GetTagStrings(base.TagClass("PUBL", TGEDCOMTag.Create)); }
			set { base.SetTagStrings(base.TagClass("PUBL", TGEDCOMTag.Create), value); }
		}

		public StringList Text
		{
			get { return base.GetTagStrings(base.TagClass("TEXT", TGEDCOMTag.Create)); }
			set { base.SetTagStrings(base.TagClass("TEXT", TGEDCOMTag.Create), value); }
		}

		public GEDCOMList<TGEDCOMRepositoryCitation> RepositoryCitations
		{
			get { return this.fRepositoryCitations; }
		}

		protected override void CreateObj(TGEDCOMTree owner, GEDCOMObject parent)
		{
			base.CreateObj(owner, parent);
			this.fRecordType = TGEDCOMRecordType.rtSource;
			this.fName = "SOUR";

			this.fRepositoryCitations = new GEDCOMList<TGEDCOMRepositoryCitation>(this);
		}

		protected override void Dispose(bool disposing)
		{
			if (disposing)
			{
				this.fRepositoryCitations.Dispose();
			}
			base.Dispose(disposing);
		}

		public override TGEDCOMTag AddTag(string tagName, string tagValue, TagConstructor tagConstructor)
		{
			TGEDCOMTag result;

			if (tagName == "REPO")
			{
				result = this.fRepositoryCitations.Add(new TGEDCOMRepositoryCitation(base.Owner, this, tagName, tagValue));
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
			this.fRepositoryCitations.Clear();
		}

		public override bool IsEmpty()
		{
			return base.IsEmpty() && this.fRepositoryCitations.Count == 0;
		}

        public override void MoveTo(TGEDCOMRecord targetRecord, bool clearDest)
		{
            if (targetRecord == null) return;

			TGEDCOMSourceRecord targetSource = targetRecord as TGEDCOMSourceRecord;

            StringList titl = new StringList();
			StringList orig = new StringList();
			StringList publ = new StringList();
			StringList text = new StringList();
			try
			{
				titl.Text = (targetSource.Title.Text + "\n" + this.Title.Text).Trim();
				orig.Text = (targetSource.Originator.Text + "\n" + this.Originator.Text).Trim();
				publ.Text = (targetSource.Publication.Text + "\n" + this.Publication.Text).Trim();
				text.Text = (targetSource.Text.Text + "\n" + this.Text.Text).Trim();
				
                base.DeleteTag("TITL");
				base.DeleteTag("TEXT");
				base.DeleteTag("ABBR");
				base.DeleteTag("PUBL");
				base.DeleteTag("AUTH");

                base.MoveTo(targetRecord, clearDest);
				
                targetSource.Title = titl;
				targetSource.Originator = orig;
				targetSource.Publication = publ;
				targetSource.Text = text;

				while (this.fRepositoryCitations.Count > 0)
				{
                    TGEDCOMRepositoryCitation obj = this.fRepositoryCitations.Extract(0);
                    obj.ResetParent(targetSource);
					targetSource.RepositoryCitations.Add(obj);
				}
			}
			finally
			{
                titl.Dispose();
                orig.Dispose();
                publ.Dispose();
                text.Dispose();
			}
		}

		public override void Pack()
		{
			base.Pack();
			this.fRepositoryCitations.Pack();
		}

        public override void ReplaceXRefs(XRefReplacer map)
		{
            base.ReplaceXRefs(map);
            this.fRepositoryCitations.ReplaceXRefs(map);
		}

		public override void ResetOwner(TGEDCOMTree newOwner)
		{
			base.ResetOwner(newOwner);
			this.fRepositoryCitations.ResetOwner(newOwner);
		}

		public override void SaveToStream(StreamWriter stream)
		{
			base.SaveToStream(stream);
			this.fRepositoryCitations.SaveToStream(stream);
		}

		public void SetOriginatorArray(params string[] value)
		{
			base.SetTagStrings(base.TagClass("AUTH", TGEDCOMTag.Create), value);
		}

		public void SetTitleArray(params string[] value)
		{
			base.SetTagStrings(base.TagClass("TITL", TGEDCOMTag.Create), value);
		}

		public void SetPublicationArray(params string[] value)
		{
			base.SetTagStrings(base.TagClass("PUBL", TGEDCOMTag.Create), value);
		}

		public void SetTextArray(params string[] value)
		{
			base.SetTagStrings(base.TagClass("TEXT", TGEDCOMTag.Create), value);
		}

		public TGEDCOMSourceRecord(TGEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue) : base(owner, parent, tagName, tagValue)
		{
		}

        public new static TGEDCOMTag Create(TGEDCOMTree owner, GEDCOMObject parent, string tagName, string tagValue)
		{
			return new TGEDCOMSourceRecord(owner, parent, tagName, tagValue);
		}

        public override float IsMatch(TGEDCOMTag tag, MatchParams matchParams)
		{
        	if (tag == null) return 0.0f;
        	float match = 0.0f;

        	TGEDCOMSourceRecord source = tag as TGEDCOMSourceRecord;
        	if (string.Compare(this.FiledByEntry, source.FiledByEntry, true) == 0) {
        		match = 100.0f;
        	}

        	return match;
        }

        #region Auxiliary

		public TGEDCOMRepositoryCitation aux_AddRepository(TGEDCOMRepositoryRecord aRepRec)
		{
			TGEDCOMRepositoryCitation cit = null;
			
			if (aRepRec != null) {
				cit = new TGEDCOMRepositoryCitation(this.Owner, this, "", "");
				cit.Value = aRepRec;
				this.RepositoryCitations.Add(cit);
			}
			
			return cit;
		}

		#endregion
	}
}
