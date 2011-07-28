using GedCom551;
using GKCore;
using GKSys;
using System;
using System.ComponentModel;
using System.Drawing;
using System.Resources;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Windows.Forms;

namespace GKUI
{
	public class TfmNoteEdit : Form
	{
		private Button btnAccept;
		private Button btnCancel;
		private TextBox mmNote;
		private TfmBase FBase;
		private TGEDCOMNoteRecord FNoteRecord;

		[Browsable(false)]
		public TfmBase Base
		{
			get
			{
				return this.FBase;
			}
		}

		[Browsable(false)]
		public TGEDCOMNoteRecord NoteRecord
		{
			get
			{
				return this.FNoteRecord;
			}
			set
			{
				this.SetNoteRecord(value);
			}
		}

		private void SetNoteRecord([In] TGEDCOMNoteRecord Value)
		{
			this.FNoteRecord = Value;
			this.mmNote.Text = this.FNoteRecord.Notes.Text.Trim();
		}
		private void InitializeComponent()
		{
			ResourceManager resources = new ResourceManager(typeof(TfmNoteEdit));
			this.btnAccept = new Button();
			this.btnCancel = new Button();
			this.mmNote = new TextBox();
			base.SuspendLayout();
			this.btnAccept.Image = (resources.GetObject("btnAccept.Image") as Image);
			this.btnAccept.ImageAlign = ContentAlignment.MiddleLeft;
			this.btnAccept.Location = new Point(224, 216);
			this.btnAccept.Name = "btnAccept";
			this.btnAccept.Size = new Size(81, 25);
			this.btnAccept.TabIndex = 1;
			this.btnAccept.Text = "Accept";
			this.btnAccept.TextAlign = ContentAlignment.MiddleRight;
			this.btnAccept.Click += new EventHandler(this.btnAccept_Click);
			this.btnCancel.DialogResult = DialogResult.Cancel;
			this.btnCancel.Image = (resources.GetObject("btnCancel.Image") as Image);
			this.btnCancel.ImageAlign = ContentAlignment.MiddleLeft;
			this.btnCancel.Location = new Point(312, 216);
			this.btnCancel.Name = "btnCancel";
			this.btnCancel.Size = new Size(81, 25);
			this.btnCancel.TabIndex = 2;
			this.btnCancel.Text = "Cancel";
			this.btnCancel.TextAlign = ContentAlignment.MiddleRight;
			this.mmNote.Location = new Point(8, 8);
			this.mmNote.Multiline = true;
			this.mmNote.Name = "mmNote";
			this.mmNote.ScrollBars = ScrollBars.Both;
			this.mmNote.Size = new Size(385, 193);
			this.mmNote.TabIndex = 0;
			this.mmNote.Text = "";
			base.AcceptButton = this.btnAccept;
			this.AutoScaleBaseSize = new Size(5, 14);
			base.CancelButton = this.btnCancel;
			base.ClientSize = new Size(402, 249);
			base.Controls.Add(this.btnAccept);
			base.Controls.Add(this.btnCancel);
			base.Controls.Add(this.mmNote);
			this.Font = new Font("Tahoma", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 204);
			base.FormBorderStyle = FormBorderStyle.FixedDialog;
			base.MaximizeBox = false;
			base.MinimizeBox = false;
			base.Name = "TfmNoteEdit";
			base.ShowInTaskbar = false;
			base.StartPosition = FormStartPosition.CenterScreen;
			this.Text = "TfmNoteEdit";
			base.ResumeLayout(false);
		}
		private void btnAccept_Click(object sender, EventArgs e)
		{
			TStrings strs = null;
			try
			{
				strs = VCLUtils.StrArrayToStrings(this.mmNote.Lines);
				this.FNoteRecord.Notes = strs;
			}
			finally
			{
				strs.Free();
			}
			this.Base.ChangeRecord(this.FNoteRecord);
			base.DialogResult = DialogResult.OK;
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
