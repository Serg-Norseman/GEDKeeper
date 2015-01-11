using System;
using System.Windows.Forms;

using GedCom551;
using GKCore;
using GKCore.Interfaces;
using GKUI.Controls;
using GKUI.Sheets;

/// <summary>
/// 
/// </summary>

namespace GKUI.Dialogs
{
	public partial class TfmSourceEdit : Form, IBaseEditor
	{
		private readonly IBase fBase;
		private readonly GKNotesSheet fNotesList;
        private readonly GKMediaSheet fMediaList;
        private readonly GKRepositoriesSheet fRepositoriesList;

        private TGEDCOMSourceRecord fSourceRecord;

		public TGEDCOMSourceRecord SourceRecord
		{
			get { return this.fSourceRecord; }
			set { this.SetSourceRecord(value); }
		}

        public IBase Base
		{
			get { return this.fBase; }
		}

		private void AcceptChanges()
		{
			this.fSourceRecord.FiledByEntry = this.EditShortTitle.Text;
			this.fSourceRecord.Originator.Clear();
			this.fSourceRecord.SetOriginatorArray(this.EditAuthor.Lines);
			this.fSourceRecord.Title.Clear();
			this.fSourceRecord.SetTitleArray(this.EditTitle.Lines);
			this.fSourceRecord.Publication.Clear();
			this.fSourceRecord.SetPublicationArray(this.EditPublication.Lines);
			this.fSourceRecord.Text.Clear();
			this.fSourceRecord.SetTextArray(this.EditText.Lines);

			this.Base.ChangeRecord(this.fSourceRecord);
		}

		private void ListModify(object sender, ModifyEventArgs eArgs)
		{
            if (sender == this.fRepositoriesList && eArgs.Action == RecordAction.raJump)
            {
            	TGEDCOMRepositoryCitation cit = eArgs.ItemData as TGEDCOMRepositoryCitation;
            	if (cit != null)
            	{
            		this.AcceptChanges();
            		this.Base.SelectRecordByXRef((cit.Value as TGEDCOMRepositoryRecord).XRef);
            		base.Close();
            	}
            }
		}

        private void SetSourceRecord(TGEDCOMSourceRecord value)
		{
			this.fSourceRecord = value;
			
            this.EditShortTitle.Text = this.fSourceRecord.FiledByEntry;
			this.EditAuthor.Text = this.fSourceRecord.Originator.Text.Trim();
			this.EditTitle.Text = this.fSourceRecord.Title.Text.Trim();
			this.EditPublication.Text = this.fSourceRecord.Publication.Text.Trim();
			this.EditText.Text = this.fSourceRecord.Text.Text.Trim();

            this.fNotesList.DataList = this.fSourceRecord.Notes.GetEnumerator();
            this.fMediaList.DataList = this.fSourceRecord.MultimediaLinks.GetEnumerator();
            this.fRepositoriesList.DataList = this.fSourceRecord.RepositoryCitations.GetEnumerator();
			
            this.ActiveControl = this.EditShortTitle;
		}

		private void btnAccept_Click(object sender, EventArgs e)
		{
			try
			{
				this.AcceptChanges();
				base.DialogResult = DialogResult.OK;
			}
			catch (Exception ex)
			{
				this.fBase.Host.LogWrite("TfmSourceEdit.Accept(): " + ex.Message);
				base.DialogResult = DialogResult.None;
			}
		}

		private void EditShortTitle_TextChanged(object sender, EventArgs e)
		{
			this.Text = LangMan.LS(LSID.LSID_Source) + " \"" + this.EditShortTitle.Text + "\"";
		}

		public TfmSourceEdit(IBase aBase)
		{
			this.InitializeComponent();
			this.fBase = aBase;

			this.fNotesList = new GKNotesSheet(this, this.SheetNotes);
            this.fMediaList = new GKMediaSheet(this, this.SheetMultimedia);

            this.fRepositoriesList = new GKRepositoriesSheet(this, this.SheetRepositories);
			this.fRepositoriesList.OnModify += this.ListModify;

            this.btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
            this.btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
            this.Label1.Text = LangMan.LS(LSID.LSID_ShortTitle);
            this.Label3.Text = LangMan.LS(LSID.LSID_Author);
            this.Label2.Text = LangMan.LS(LSID.LSID_Title);
            this.Label4.Text = LangMan.LS(LSID.LSID_Publication);
            this.SheetCommon.Text = LangMan.LS(LSID.LSID_Common);
            this.SheetText.Text = LangMan.LS(LSID.LSID_Text);
            this.SheetRepositories.Text = LangMan.LS(LSID.LSID_RPRepositories);
            this.SheetNotes.Text = LangMan.LS(LSID.LSID_RPNotes);
            this.SheetMultimedia.Text = LangMan.LS(LSID.LSID_RPMultimedia);
		}
	}
}
