using System;
using System.Runtime.InteropServices;
using System.Windows.Forms;

using GedCom551;
using GKCore;
using GKCore.Sys;

namespace GKUI
{
	public partial class TfmNoteEdit : Form
	{
		private TfmBase FBase;
		private TGEDCOMNoteRecord FNoteRecord;

		public TfmBase Base
		{
			get { return this.FBase; }
		}

		public TGEDCOMNoteRecord NoteRecord
		{
			get { return this.FNoteRecord; }
			set { this.SetNoteRecord(value); }
		}

		private void SetNoteRecord([In] TGEDCOMNoteRecord Value)
		{
			this.FNoteRecord = Value;
			this.mmNote.Text = this.FNoteRecord.Note.Text.Trim();
		}

		private void btnAccept_Click(object sender, EventArgs e)
		{
			try
			{
				this.FNoteRecord.SetNotesArray(this.mmNote.Lines);
				this.Base.ChangeRecord(this.FNoteRecord);
				base.DialogResult = DialogResult.OK;
			}
			catch (Exception E)
			{
				SysUtils.LogWrite("TfmNoteEdit.Accept(): " + E.Message);
				base.DialogResult = DialogResult.None;
			}
		}

		public TfmNoteEdit(TfmBase aBase)
		{
			this.InitializeComponent();
			this.FBase = aBase;
			this.btnAccept.Text = GKL.LSList[97];
			this.btnCancel.Text = GKL.LSList[98];
			this.Text = GKL.LSList[108];
		}
	}
}
