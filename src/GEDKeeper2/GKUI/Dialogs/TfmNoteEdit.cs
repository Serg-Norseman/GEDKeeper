using System;
using System.Windows.Forms;

using GedCom551;
using GKCore;
using GKCore.Interfaces;

/// <summary>
/// 
/// </summary>

namespace GKUI.Dialogs
{
	public partial class TfmNoteEdit : Form, IBaseEditor
	{
		private readonly IBase fBase;
		private TGEDCOMNoteRecord fNoteRecord;

		public TGEDCOMNoteRecord NoteRecord
		{
			get { return this.fNoteRecord; }
			set { this.SetNoteRecord(value); }
		}

		public IBase Base
		{
			get { return this.fBase; }
		}

		private void SetNoteRecord(TGEDCOMNoteRecord value)
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
				this.fBase.Host.LogWrite("TfmNoteEdit.Accept(): " + ex.Message);
				base.DialogResult = DialogResult.None;
			}
		}

		public TfmNoteEdit(IBase aBase)
		{
			this.InitializeComponent();
			this.fBase = aBase;

			this.btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
			this.btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
			this.Text = LangMan.LS(LSID.LSID_Note);
		}
	}
}
