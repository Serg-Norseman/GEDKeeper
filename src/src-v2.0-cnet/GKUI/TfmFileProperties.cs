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
	public class TfmFileProperties : Form
	{
		private Button btnAccept;
		private Button btnCancel;
		private TabPage SheetAuthor;
		private Label Label1;
		private Label Label2;
		private Label Label3;
		private TextBox EditName;
		private TextBox EditTel;
		private TextBox MemoAddress;
		private TabPage SheetAdvanced;
		private CheckBox CheckAdvanced;
		private Label Label4;
		private TextBox edExtName;
		private TfmBase FBase;
		public TabControl PageControl1;

		[Browsable(false)]
		public TfmBase Base
		{
			get	{ return this.FBase; }
		}

		private void InitializeComponent()
		{
			ResourceManager resources = new ResourceManager(typeof(TfmFileProperties));
			this.btnAccept = new Button();
			this.btnCancel = new Button();
			this.PageControl1 = new TabControl();
			this.SheetAuthor = new TabPage();
			this.Label1 = new Label();
			this.Label2 = new Label();
			this.Label3 = new Label();
			this.EditName = new TextBox();
			this.EditTel = new TextBox();
			this.MemoAddress = new TextBox();
			this.SheetAdvanced = new TabPage();
			this.Label4 = new Label();
			this.CheckAdvanced = new CheckBox();
			this.edExtName = new TextBox();
			this.PageControl1.SuspendLayout();
			this.SheetAuthor.SuspendLayout();
			this.SheetAdvanced.SuspendLayout();
			base.SuspendLayout();
			this.btnAccept.Image = (resources.GetObject("btnAccept.Image") as Image);
			this.btnAccept.ImageAlign = ContentAlignment.MiddleLeft;
			this.btnAccept.Location = new Point(272, 296);
			this.btnAccept.Name = "btnAccept";
			this.btnAccept.Size = new Size(81, 25);
			this.btnAccept.TabIndex = 0;
			this.btnAccept.Text = "Принять";
			this.btnAccept.TextAlign = ContentAlignment.MiddleRight;
			this.btnAccept.Click += new EventHandler(this.btnAccept_Click);
			this.btnCancel.DialogResult = DialogResult.Cancel;
			this.btnCancel.Image = (resources.GetObject("btnCancel.Image") as Image);
			this.btnCancel.ImageAlign = ContentAlignment.MiddleLeft;
			this.btnCancel.Location = new Point(360, 296);
			this.btnCancel.Name = "btnCancel";
			this.btnCancel.Size = new Size(81, 25);
			this.btnCancel.TabIndex = 1;
			this.btnCancel.Text = "Отменить";
			this.btnCancel.TextAlign = ContentAlignment.MiddleRight;
			this.PageControl1.Controls.Add(this.SheetAuthor);
			this.PageControl1.Controls.Add(this.SheetAdvanced);
			this.PageControl1.Location = new Point(8, 8);
			this.PageControl1.Name = "PageControl1";
			this.PageControl1.SelectedIndex = 0;
			this.PageControl1.Size = new Size(433, 273);
			this.PageControl1.TabIndex = 2;
			this.SheetAuthor.Controls.Add(this.Label1);
			this.SheetAuthor.Controls.Add(this.Label2);
			this.SheetAuthor.Controls.Add(this.Label3);
			this.SheetAuthor.Controls.Add(this.EditName);
			this.SheetAuthor.Controls.Add(this.EditTel);
			this.SheetAuthor.Controls.Add(this.MemoAddress);
			this.SheetAuthor.Location = new Point(4, 22);
			this.SheetAuthor.Name = "SheetAuthor";
			this.SheetAuthor.Size = new Size(425, 247);
			this.SheetAuthor.TabIndex = 0;
			this.SheetAuthor.Text = "Автор";
			this.Label1.Location = new Point(8, 8);
			this.Label1.Name = "Label1";
			this.Label1.Size = new Size(30, 13);
			this.Label1.TabIndex = 0;
			this.Label1.Text = "Имя";
			this.Label2.Location = new Point(8, 32);
			this.Label2.Name = "Label2";
			this.Label2.Size = new Size(40, 13);
			this.Label2.TabIndex = 1;
			this.Label2.Text = "Адрес";
			this.Label3.Location = new Point(8, 152);
			this.Label3.Name = "Label3";
			this.Label3.Size = new Size(50, 13);
			this.Label3.TabIndex = 2;
			this.Label3.Text = "Телефон";
			this.EditName.Location = new Point(64, 8);
			this.EditName.Name = "EditName";
			this.EditName.Size = new Size(353, 21);
			this.EditName.TabIndex = 0;
			this.EditName.Text = "";
			this.EditTel.Location = new Point(64, 152);
			this.EditTel.Name = "EditTel";
			this.EditTel.Size = new Size(353, 21);
			this.EditTel.TabIndex = 1;
			this.EditTel.Text = "";
			this.MemoAddress.Location = new Point(64, 32);
			this.MemoAddress.Multiline = true;
			this.MemoAddress.Name = "MemoAddress";
			this.MemoAddress.Size = new Size(353, 113);
			this.MemoAddress.TabIndex = 2;
			this.MemoAddress.Text = "";
			this.SheetAdvanced.Controls.Add(this.Label4);
			this.SheetAdvanced.Controls.Add(this.CheckAdvanced);
			this.SheetAdvanced.Controls.Add(this.edExtName);
			this.SheetAdvanced.Location = new Point(4, 22);
			this.SheetAdvanced.Name = "SheetAdvanced";
			this.SheetAdvanced.Size = new Size(425, 247);
			this.SheetAdvanced.TabIndex = 1;
			this.SheetAdvanced.Text = "Расширение проекта";
			this.Label4.Location = new Point(8, 40);
			this.Label4.Name = "Label4";
			this.Label4.Size = new Size(200, 13);
			this.Label4.TabIndex = 0;
			this.Label4.Text = "Название архива и папки хранилища";
			this.CheckAdvanced.Location = new Point(8, 8);
			this.CheckAdvanced.Name = "CheckAdvanced";
			this.CheckAdvanced.Size = new Size(409, 17);
			this.CheckAdvanced.TabIndex = 0;
			this.CheckAdvanced.Text = "Поддержка расширения (архив, хранилище файлов)";
			this.edExtName.Location = new Point(8, 56);
			this.edExtName.Name = "edExtName";
			this.edExtName.Size = new Size(225, 21);
			this.edExtName.TabIndex = 1;
			this.edExtName.Text = "";
			base.AcceptButton = this.btnAccept;
			this.AutoScaleBaseSize = new Size(5, 14);
			base.CancelButton = this.btnCancel;
			base.ClientSize = new Size(449, 329);
			base.Controls.Add(this.btnAccept);
			base.Controls.Add(this.btnCancel);
			base.Controls.Add(this.PageControl1);
			this.Font = new Font("Tahoma", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 204);
			base.FormBorderStyle = FormBorderStyle.FixedDialog;
			base.MaximizeBox = false;
			base.MinimizeBox = false;
			base.Name = "TfmFileProperties";
			base.ShowInTaskbar = false;
			base.StartPosition = FormStartPosition.CenterScreen;
			this.Text = "Свойства файла";
			this.PageControl1.ResumeLayout(false);
			this.SheetAuthor.ResumeLayout(false);
			this.SheetAdvanced.ResumeLayout(false);
			base.ResumeLayout(false);
		}

		private void UpdateControls()
		{
			TGEDCOMSubmitterRecord submitter = this.Base.Engine.GetSubmitter();
			this.EditName.Text = submitter.Name.FullName;
			this.MemoAddress.Text = submitter.Address.Address.Text;
			this.EditTel.Text = submitter.Address.GetPhoneNumber(0);
			this.CheckAdvanced.Checked = this.Base.Engine.IsAdvanced;
			this.edExtName.Text = this.Base.Engine.GetSpecExtName();
		}

		private void btnAccept_Click(object sender, EventArgs e)
		{
			TGEDCOMSubmitterRecord submitter = this.Base.Engine.GetSubmitter();
			submitter.Name.StringValue = this.EditName.Text;
			TStrings strs = null;
			try
			{
				strs = VCLUtils.StrArrayToStrings(this.MemoAddress.Lines);
				submitter.Address.Address = strs;
			}
			finally
			{
				strs.Free();
			}
			submitter.Address.SetPhoneNumber(0, this.EditTel.Text);
			submitter.ChangeDate.ChangeDateTime = DateTime.Now;
			this.Base.Engine.IsAdvanced = this.CheckAdvanced.Checked;
			this.Base.Engine.ExtName = this.edExtName.Text;
			this.Base.Modified = true;
			base.DialogResult = DialogResult.OK;
		}

		public TfmFileProperties(TfmBase aBase)
		{
			this.InitializeComponent();
			this.FBase = aBase;
			this.UpdateControls();
			this.btnAccept.Text = GKL.LSList[97];
			this.btnCancel.Text = GKL.LSList[98];
			this.SheetAuthor.Text = GKL.LSList[142];
			this.Label1.Text = GKL.LSList[85];
			this.Label2.Text = GKL.LSList[82];
			this.Label3.Text = GKL.LSList[131];
			this.SheetAdvanced.Text = GKL.LSList[167];
			this.CheckAdvanced.Text = GKL.LSList[168];
			this.Label4.Text = GKL.LSList[169];
		}
	}
}
