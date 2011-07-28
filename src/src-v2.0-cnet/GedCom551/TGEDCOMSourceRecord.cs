using GKSys;
using System;
using System.ComponentModel;
using System.IO;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GedCom551
{
	public class TGEDCOMSourceRecord : TGEDCOMRecord
	{

		internal TStrings FTitle;
		internal TStrings FOriginator;
		internal TStrings FPublication;
		internal TStrings FText;
		internal TGEDCOMList FRepositoryCitations;

		public string AutomatedRecordID
		{
			get { return this.GetStringTag(2); }
			set { this.SetStringTag(2, value); }
		}

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
			get { return this.GetStringTag(1); }
			set { this.SetStringTag(1, value); }
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

		/*public TGEDCOMRepositoryCitation RepositoryCitations
		{
			get { return this.GetRepositoryCitations(Index); }
		}
		public int RepositoryCitationsCount
		{
			get { return this.GetRepositoryCitationsCount(); }
		}*/

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

		internal TStrings GetTitle()
		{
			return base.GetTagStrings(base.TagClass("TITL", typeof(TGEDCOMTag)), ref this.FTitle);
		}
		internal void SetTitle(TStrings Value)
		{
			base.SetTagStrings(base.TagClass("TITL", typeof(TGEDCOMTag)), Value);
		}
		internal TStrings GetOriginator()
		{
			return base.GetTagStrings(base.TagClass("AUTH", typeof(TGEDCOMTag)), ref this.FOriginator);
		}
		internal void SetOriginator([In] TStrings Value)
		{
			base.SetTagStrings(base.TagClass("AUTH", typeof(TGEDCOMTag)), Value);
		}
		internal TStrings GetPublication()
		{
			return base.GetTagStrings(base.TagClass("PUBL", typeof(TGEDCOMTag)), ref this.FPublication);
		}
		internal TStrings GetText()
		{
			return base.GetTagStrings(base.TagClass("TEXT", typeof(TGEDCOMTag)), ref this.FText);
		}
		internal void SetPublication([In] TStrings Value)
		{
			base.SetTagStrings(base.TagClass("PUBL", typeof(TGEDCOMTag)), Value);
		}
		internal void SetText([In] TStrings Value)
		{
			base.SetTagStrings(base.TagClass("TEXT", typeof(TGEDCOMTag)), Value);
		}
		internal TGEDCOMData GetData()
		{
			return base.TagClass("DATA", typeof(TGEDCOMData)) as TGEDCOMData;
		}
		internal string GetStringTag(int Index)
		{
			string Result = "";
			if (Index != 1)
			{
				if (Index == 2)
				{
					Result = base.GetTagStringValue("RIN");
				}
			}
			else
			{
				Result = base.GetTagStringValue("ABBR");
			}
			return Result;
		}
		internal void SetStringTag(int Index, [In] string Value)
		{
			if (Index != 1)
			{
				if (Index == 2)
				{
					base.SetTagStringValue("RIN", Value);
				}
			}
			else
			{
				base.SetTagStringValue("ABBR", Value);
			}
		}

		public TGEDCOMRepositoryCitation GetRepositoryCitation(int Index)
		{
			TGEDCOMRepositoryCitation Result;
			if (this.FRepositoryCitations == null)
			{
				Result = null;
			}
			else
			{
				Result = (this.FRepositoryCitations[Index] as TGEDCOMRepositoryCitation);
			}
			return Result;
		}

		internal int GetRepositoryCitationsCount()
		{
			int Result;
			if (this.FRepositoryCitations == null)
			{
				Result = 0;
			}
			else
			{
				Result = this.FRepositoryCitations.Count;
			}
			return Result;
		}
		protected internal override void CreateObj(TGEDCOMObject AOwner, TGEDCOMObject AParent)
		{
			base.CreateObj(AOwner, AParent);
			base.SetLists(TEnumSet.Create(new Enum[]
			{
				TGEDCOMObject.TGEDCOMSubList.stNotes, 
				TGEDCOMObject.TGEDCOMSubList.stMultimedia
			}));
			this.FRecordType = TGEDCOMRecord.TGEDCOMRecordType.rtSource;
			this.FName = "SOUR";
			this.FTitle = null;
			this.FOriginator = null;
			this.FPublication = null;
			this.FText = null;
			this.FRepositoryCitations = null;
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
				if (this.FRepositoryCitations != null)
				{
					this.FRepositoryCitations.Free();
				}
				base.Dispose();
				this.Disposed_ = true;
			}
		}

		public TGEDCOMRepositoryCitation AddRepositoryCitation(TGEDCOMRepositoryCitation Value)
		{
			if (this.FRepositoryCitations == null)
			{
				this.FRepositoryCitations = new TGEDCOMList(this);
			}
			if (Value != null)
			{
				this.FRepositoryCitations.Add(Value);
			}
			return Value;
		}
		public void DeleteRepositoryCitation(TGEDCOMRepositoryCitation Value)
		{
			if (this.FRepositoryCitations != null)
			{
				this.FRepositoryCitations.DeleteObject(Value);
			}
		}

		public override TGEDCOMTag AddTag([In] string ATag, [In] string AValue, Type AClass)
		{
			TGEDCOMTag Result;
			if (BDSSystem.WStrCmp(ATag, "REPO") == 0)
			{
				Result = this.AddRepositoryCitation(new TGEDCOMRepositoryCitation(base.Owner, this, ATag, AValue));
			}
			else
			{
				if (BDSSystem.WStrCmp(ATag, "DATA") == 0)
				{
					Result = base.AddTag(ATag, AValue, typeof(TGEDCOMData));
				}
				else
				{
					if (BDSSystem.WStrCmp(ATag, "REFN") == 0)
					{
						Result = base.AddUserReference(new TGEDCOMUserReference(base.Owner, this, ATag, AValue));
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
			if (this.FRepositoryCitations != null)
			{
				this.FRepositoryCitations.Clear();
			}
		}
		public override bool IsEmpty()
		{
			return base.IsEmpty() && this.GetRepositoryCitationsCount() == 0;
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
				if (this.FRepositoryCitations != null)
				{
					while (this.FRepositoryCitations.Count > 0)
					{
						TGEDCOMObject obj = this.FRepositoryCitations.Extract(0);
						(obj as TGEDCOMCustomTag).ResetParent(toSource);
						toSource.AddRepositoryCitation(obj as TGEDCOMRepositoryCitation);
					}
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
			if (this.FRepositoryCitations != null)
			{
				this.FRepositoryCitations.Pack();
			}
		}
		public override void ReplaceXRefs(TXRefReplaceMap aMap)
		{
			base.ReplaceXRefs(aMap);
			if (this.FRepositoryCitations != null)
			{
				this.FRepositoryCitations.ReplaceXRefs(aMap);
			}
		}
		public override void ResetOwner(TGEDCOMObject AOwner)
		{
			base.ResetOwner(AOwner);
			if (this.FRepositoryCitations != null)
			{
				this.FRepositoryCitations.ResetOwner(AOwner);
			}
		}
		public override void SaveToStream(StreamWriter AStream)
		{
			base.SaveToStream(AStream);
			if (this.FRepositoryCitations != null)
			{
				this.FRepositoryCitations.SaveToStream(AStream);
			}
		}
		public void SetOriginatorArray(params string[] Value)
		{
			Value = (string[])Value.Clone();
			base.SetTagStrings(base.TagClass("AUTH", typeof(TGEDCOMTag)), Value);
		}
		public void SetTitleArray(params string[] Value)
		{
			Value = (string[])Value.Clone();
			base.SetTagStrings(base.TagClass("TITL", typeof(TGEDCOMTag)), Value);
		}
		public void SetPublicationArray(params string[] Value)
		{
			Value = (string[])Value.Clone();
			base.SetTagStrings(base.TagClass("PUBL", typeof(TGEDCOMTag)), Value);
		}
		public void SetTextArray(params string[] Value)
		{
			Value = (string[])Value.Clone();
			base.SetTagStrings(base.TagClass("TEXT", typeof(TGEDCOMTag)), Value);
		}

		public TGEDCOMSourceRecord(TGEDCOMObject AOwner, TGEDCOMObject AParent, [In] string AName, [In] string AValue) : base(AOwner, AParent, AName, AValue)
		{
		}
	}
}
