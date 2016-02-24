using System;
using System.Windows.Forms;

using BSLib;
using GKCommon;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKCore.Types;
using GKUI.Controls;
using GKUI.Sheets;

namespace GKUI.Dialogs
{
    /// <summary>
    /// 
    /// </summary>
    public partial class TfmSourceEdit : Form, IBaseEditor
	{
		private readonly IBaseWindow fBase;
		private readonly GKNotesSheet fNotesList;
        private readonly GKMediaSheet fMediaList;
        private readonly GKSheetList fRepositoriesList;

        private GEDCOMSourceRecord fSourceRecord;

		public GEDCOMSourceRecord SourceRecord
		{
			get { return this.fSourceRecord; }
			set { this.SetSourceRecord(value); }
		}

        public IBaseWindow Base
		{
			get { return this.fBase; }
		}

        private void SetSourceRecord(GEDCOMSourceRecord value)
		{
			this.fSourceRecord = value;
			
            this.EditShortTitle.Text = this.fSourceRecord.FiledByEntry;
			this.EditAuthor.Text = this.fSourceRecord.Originator.Text.Trim();
			this.EditTitle.Text = this.fSourceRecord.Title.Text.Trim();
			this.EditPublication.Text = this.fSourceRecord.Publication.Text.Trim();
			this.EditText.Text = this.fSourceRecord.Text.Text.Trim();

            this.fNotesList.DataList = this.fSourceRecord.Notes.GetEnumerator();
            this.fMediaList.DataList = this.fSourceRecord.MultimediaLinks.GetEnumerator();
            this.UpdateReposSheet();
			
            this.ActiveControl = this.EditShortTitle;
		}

		public TfmSourceEdit(IBaseWindow aBase)
		{
			this.InitializeComponent();
			this.fBase = aBase;

			this.fNotesList = new GKNotesSheet(this, this.SheetNotes);
            this.fMediaList = new GKMediaSheet(this, this.SheetMultimedia);
            this.fRepositoriesList = this.CreateReposSheet(this.SheetRepositories);

            // SetLang()
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

		private GKSheetList CreateReposSheet(Control owner)
		{
			GKSheetList sheet = new GKSheetList(owner);
			
            sheet.Columns_BeginUpdate();
            sheet.AddColumn(LangMan.LS(LSID.LSID_Repository), 300, false);
            sheet.Columns_EndUpdate();

            sheet.Buttons = EnumSet<SheetButton>.Create(SheetButton.lbAdd, SheetButton.lbDelete, SheetButton.lbJump);
            sheet.OnModify += this.ModifyReposSheet;

            return sheet;
		}
		
		private void UpdateReposSheet()
		{
            try
            {
                this.fRepositoriesList.ClearItems();

                foreach (GEDCOMRepositoryCitation repCit in this.fSourceRecord.RepositoryCitations) {
                    GEDCOMRepositoryRecord rep = repCit.Value as GEDCOMRepositoryRecord;
                    if (rep == null) continue;

                    this.fRepositoriesList.AddItem(rep.RepositoryName, repCit);
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("TfmSourceEdit.UpdateReposSheet(): " + ex.Message);
            }
		}
		
		private void ModifyReposSheet(object sender, ModifyEventArgs eArgs)
		{
            bool result = false;

            GEDCOMRepositoryCitation cit = eArgs.ItemData as GEDCOMRepositoryCitation;

            switch (eArgs.Action)
            {
            	case RecordAction.raAdd:
            		GEDCOMRepositoryRecord rep = this.fBase.SelectRecord(GEDCOMRecordType.rtRepository, null) as GEDCOMRepositoryRecord;
            		if (rep != null) {
            			this.fSourceRecord.AddRepository(rep);
            			result = true;
            		}
            		break;

            	case RecordAction.raDelete:
            		if (cit != null && GKUtils.ShowQuestion(LangMan.LS(LSID.LSID_DetachRepositoryQuery)) != DialogResult.No) {
            			this.fSourceRecord.RepositoryCitations.Delete(cit);
            			result = true;
            		}
            		break;

            	case RecordAction.raJump:
            		if (cit != null) {
            			this.AcceptChanges();
                        this.Base.SelectRecordByXRef(cit.Value.XRef);
            			base.Close();
            		}
            		break;
            }

            if (result) this.UpdateReposSheet();
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

		private void btnAccept_Click(object sender, EventArgs e)
		{
			try
			{
				this.AcceptChanges();
				base.DialogResult = DialogResult.OK;
			}
			catch (Exception ex)
			{
				this.fBase.Host.LogWrite("TfmSourceEdit.btnAccept_Click(): " + ex.Message);
				base.DialogResult = DialogResult.None;
			}
		}

		private void EditShortTitle_TextChanged(object sender, EventArgs e)
		{
            this.Text = string.Format("{0} \"{1}\"", LangMan.LS(LSID.LSID_Source), this.EditShortTitle.Text);
		}
	}
}
