using System;
using System.Windows.Forms;

using GedCom551;
using GKCore;
using GKCore.Interfaces;
using GKUI.Sheets;

/// <summary>
/// 
/// </summary>

namespace GKUI.Dialogs
{
	public partial class TfmRepositoryEdit : Form, IBaseEditor
	{
		private readonly IBase fBase;
        private readonly GKNotesSheet fNotesList;

        private TGEDCOMRepositoryRecord fRepository;
        
		public TGEDCOMRepositoryRecord Repository
		{
			get { return this.fRepository; }
			set { this.SetRepository(value); }
		}

		public IBase Base
		{
			get { return this.fBase; }
		}

		private void SetRepository(TGEDCOMRepositoryRecord value)
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
				this.fBase.Host.LogWrite("TfmRepositoryEdit.Accept(): " + ex.Message);
				base.DialogResult = DialogResult.None;
			}
		}

		public TfmRepositoryEdit(IBase aBase)
		{
			this.InitializeComponent();
			this.fBase = aBase;

            this.fNotesList = new GKNotesSheet(this, this.SheetNotes);

            this.Text = LangMan.LS(LSID.LSID_Repository);
			this.btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
			this.btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
			this.Label1.Text = LangMan.LS(LSID.LSID_Title);
			this.SheetNotes.Text = LangMan.LS(LSID.LSID_RPNotes);
			this.btnAddress.Text = LangMan.LS(LSID.LSID_Address) + "...";
		}
	}
}
