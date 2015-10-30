using System;
using System.Windows.Forms;

using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;

namespace GKUI.Dialogs
{
    /// <summary>
    /// 
    /// </summary>
    public partial class TfmNoteEdit : Form, IBaseEditor
	{
		private readonly IBaseWindow fBase;
		private GEDCOMNoteRecord fNoteRecord;

		public GEDCOMNoteRecord NoteRecord
		{
			get { return this.fNoteRecord; }
			set { this.SetNoteRecord(value); }
		}

		public IBaseWindow Base
		{
			get { return this.fBase; }
		}

		private void SetNoteRecord(GEDCOMNoteRecord value)
		{
			this.fNoteRecord = value;
			this.mmNote.Text = this.fNoteRecord.Note.Text.Trim();
		}

		private void btnAccept_Click(object sender, EventArgs e)
		{
			try
			{
				this.fNoteRecord.SetNotesArray(this.mmNote.Lines);
				this.fBase.ChangeRecord(this.fNoteRecord);
				base.DialogResult = DialogResult.OK;
			}
			catch (Exception ex)
			{
				this.fBase.Host.LogWrite("TfmNoteEdit.btnAccept_Click(): " + ex.Message);
				base.DialogResult = DialogResult.None;
			}
		}

		public TfmNoteEdit(IBaseWindow aBase)
		{
			this.InitializeComponent();
			this.fBase = aBase;

			// SetLang()
			this.btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
			this.btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
			this.Text = LangMan.LS(LSID.LSID_Note);
		}
	}
}
