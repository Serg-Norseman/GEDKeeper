using System;
using System.Windows.Forms;

using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKUI.Sheets;

namespace GKUI.Dialogs
{
	/// <summary>
	/// 
	/// </summary>
	public partial class TfmRepositoryEdit : Form, IBaseEditor
	{
		private readonly IBaseWindow fBase;
        private readonly GKNotesSheet fNotesList;

        private GEDCOMRepositoryRecord fRepository;
        
		public GEDCOMRepositoryRecord Repository
		{
			get { return this.fRepository; }
			set { this.SetRepository(value); }
		}

		public IBaseWindow Base
		{
			get { return this.fBase; }
		}

		private void SetRepository(GEDCOMRepositoryRecord value)
		{
			this.fRepository = value;
			this.edName.Text = this.fRepository.RepositoryName;

			this.fNotesList.DataList = this.fRepository.Notes.GetEnumerator();
		}

		private void btnAddress_Click(object sender, EventArgs e)
		{
			this.fBase.ModifyAddress(this.fRepository.Address);
		}

		private void btnAccept_Click(object sender, EventArgs e)
		{
			try
			{
				this.fRepository.RepositoryName = this.edName.Text;
				this.fBase.ChangeRecord(this.fRepository);
				base.DialogResult = DialogResult.OK;
			}
			catch (Exception ex)
			{
				this.fBase.Host.LogWrite("TfmRepositoryEdit.btnAccept_Click(): " + ex.Message);
				base.DialogResult = DialogResult.None;
			}
		}

		public TfmRepositoryEdit(IBaseWindow aBase)
		{
			this.InitializeComponent();
			this.fBase = aBase;

            this.fNotesList = new GKNotesSheet(this, this.SheetNotes);

            // SetLang()
            this.Text = LangMan.LS(LSID.LSID_Repository);
			this.btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
			this.btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
			this.Label1.Text = LangMan.LS(LSID.LSID_Title);
			this.SheetNotes.Text = LangMan.LS(LSID.LSID_RPNotes);
			this.btnAddress.Text = LangMan.LS(LSID.LSID_Address) + @"...";
		}
	}
}
