using GedCom551;
using GKCore;
using GKSys;
using GKUI.Lists;
using System;
using System.ComponentModel;
using System.Drawing;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Windows.Forms;

namespace GKUI
{
	public class TfmRepositoryEdit : Form
	{
		private Button btnAccept;
		private Button btnCancel;
		private GroupBox GroupBox1;
		private Label Label1;
		private TextBox edName;
		private TabControl PagesData;
		private TabPage SheetNotes;
		private Button btnAddress;
		private TfmBase FBase;
		private TGEDCOMRepositoryRecord FRepository;
		private TSheetList FNotesList;

		[Browsable(false)]
		public TfmBase Base
		{
			get
			{
				return this.FBase;
			}
		}
		[Browsable(false)]
		public TGEDCOMRepositoryRecord Repository
		{
			get
			{
				return this.FRepository;
			}
			set
			{
				this.SetRepository(value);
			}
		}
		private void ControlsRefresh()
		{
			this.Base.RecListNotesRefresh(this.FRepository, this.FNotesList.List, null);
		}
		private void ListModify(object Sender, object ItemData, TGenEngine.TRecAction Action)
		{
			if (object.Equals(Sender, this.FNotesList) && this.Base.ModifyRecNote(this, this.FRepository, ItemData as TGEDCOMNotes, Action))
			{
				this.ControlsRefresh();
			}
		}
		private void SetRepository([In] TGEDCOMRepositoryRecord Value)
		{
			this.FRepository = Value;
			this.edName.Text = this.FRepository.RepositoryName;
			this.ControlsRefresh();
		}
		private void InitializeComponent()
		{
			this.btnAccept = new Button();
			this.btnCancel = new Button();
			this.GroupBox1 = new GroupBox();
			this.Label1 = new Label();
			this.edName = new TextBox();
			this.PagesData = new TabControl();
			this.SheetNotes = new TabPage();
			this.btnAddress = new Button();
			this.GroupBox1.SuspendLayout();
			this.PagesData.SuspendLayout();
			base.SuspendLayout();
			this.btnAccept.ImageAlign = ContentAlignment.MiddleLeft;
			this.btnAccept.Location = new Point(240, 336);
			this.btnAccept.Name = "btnAccept";
			this.btnAccept.Size = new Size(81, 25);
			this.btnAccept.TabIndex = 3;
			this.btnAccept.Text = "Принять";
			this.btnAccept.TextAlign = ContentAlignment.MiddleRight;
			this.btnAccept.Click += new EventHandler(this.btnAccept_Click);
			this.btnCancel.DialogResult = DialogResult.Cancel;
			this.btnCancel.ImageAlign = ContentAlignment.MiddleLeft;
			this.btnCancel.Location = new Point(328, 336);
			this.btnCancel.Name = "btnCancel";
			this.btnCancel.Size = new Size(81, 25);
			this.btnCancel.TabIndex = 4;
			this.btnCancel.Text = "Отменить";
			this.btnCancel.TextAlign = ContentAlignment.MiddleRight;
			this.GroupBox1.Controls.Add(this.Label1);
			this.GroupBox1.Controls.Add(this.edName);
			this.GroupBox1.Location = new Point(0, 0);
			this.GroupBox1.Name = "GroupBox1";
			this.GroupBox1.Size = new Size(417, 41);
			this.GroupBox1.TabIndex = 0;
			this.GroupBox1.TabStop = false;
			this.Label1.Location = new Point(8, 20);
			this.Label1.Name = "Label1";
			this.Label1.Size = new Size(54, 13);
			this.Label1.TabIndex = 0;
			this.Label1.Text = "Название";
			this.edName.Location = new Point(72, 12);
			this.edName.Name = "edName";
			this.edName.Size = new Size(337, 21);
			this.edName.TabIndex = 0;
			this.edName.Text = "";
			this.PagesData.Controls.Add(this.SheetNotes);
			this.PagesData.Location = new Point(0, 41);
			this.PagesData.Name = "PagesData";
			this.PagesData.SelectedIndex = 0;
			this.PagesData.Size = new Size(417, 280);
			this.PagesData.TabIndex = 1;
			this.SheetNotes.Location = new Point(4, 22);
			this.SheetNotes.Name = "SheetNotes";
			this.SheetNotes.Size = new Size(409, 254);
			this.SheetNotes.TabIndex = 0;
			this.SheetNotes.Text = "Заметки";
			this.btnAddress.Location = new Point(8, 336);
			this.btnAddress.Name = "btnAddress";
			this.btnAddress.Size = new Size(81, 25);
			this.btnAddress.TabIndex = 2;
			this.btnAddress.Text = "Адрес...";
			this.btnAddress.Click += new EventHandler(this.btnAddress_Click);
			base.AcceptButton = this.btnAccept;
			this.AutoScaleBaseSize = new Size(5, 14);
			base.CancelButton = this.btnCancel;
			base.ClientSize = new Size(417, 369);
			base.Controls.Add(this.btnAccept);
			base.Controls.Add(this.btnCancel);
			base.Controls.Add(this.GroupBox1);
			base.Controls.Add(this.PagesData);
			base.Controls.Add(this.btnAddress);
			this.Font = new Font("Tahoma", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 204);
			base.FormBorderStyle = FormBorderStyle.FixedDialog;
			base.MaximizeBox = false;
			base.MinimizeBox = false;
			base.Name = "TfmRepositoryEdit";
			base.ShowInTaskbar = false;
			base.StartPosition = FormStartPosition.CenterScreen;
			this.Text = "Архив";
			this.GroupBox1.ResumeLayout(false);
			this.PagesData.ResumeLayout(false);
			base.ResumeLayout(false);
		}
		private void btnAddress_Click(object sender, EventArgs e)
		{
			this.Base.ModifyAddress(this, this.FRepository.Address);
		}
		private void btnAccept_Click(object sender, EventArgs e)
		{
			this.FRepository.RepositoryName = this.edName.Text;
			this.Base.ChangeRecord(this.FRepository);
			base.DialogResult = DialogResult.OK;
		}

		public TfmRepositoryEdit(TfmBase aBase)
		{
			this.InitializeComponent();
			this.FBase = aBase;
			this.FNotesList = new TSheetList(this.SheetNotes);
			this.FNotesList.set_OnModify(new TSheetList.TModifyEvent(this.ListModify));
			this.Base.SetupRecNotesList(this.FNotesList);
			this.Text = GKL.LSList[134];
			this.btnAccept.Text = GKL.LSList[97];
			this.btnCancel.Text = GKL.LSList[98];
			this.Label1.Text = GKL.LSList[125];
			this.SheetNotes.Text = GKL.LSList[54];
			this.btnAddress.Text = GKL.LSList[82] + "...";
		}

	}
}
