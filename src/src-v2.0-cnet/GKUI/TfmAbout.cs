using GKCore;
using GKSys;
using System;
using System.Drawing;
using System.Resources;
using System.Runtime.CompilerServices;
using System.Windows.Forms;

namespace GKUI
{
	public class TfmAbout : Form
	{
		private Label LabelProduct;
		private Label LabelVersion;
		private Label LabelCopyright;
		private Label LabelCite;
		private Label LabelMail;
		private Button btnClose;

		private void LabelMail_Click(object sender, EventArgs e)
		{
			TGKSys.LoadExtFile(this.LabelMail.Text);
		}

		private void InitializeComponent()
		{
			ResourceManager resources = new ResourceManager(typeof(TfmAbout));
			this.LabelProduct = new Label();
			this.LabelVersion = new Label();
			this.btnClose = new Button();
			this.LabelCopyright = new Label();
			this.LabelMail = new Label();
			this.LabelCite = new Label();
			base.SuspendLayout();
			this.LabelProduct.AutoSize = true;
			this.LabelProduct.Font = new Font("Times New Roman", 20.25f, FontStyle.Bold, GraphicsUnit.Point, 204);
			this.LabelProduct.Location = new Point(8, 8);
			this.LabelProduct.Name = "LabelProduct";
			this.LabelProduct.Size = new Size(23, 35);
			this.LabelProduct.TabIndex = 0;
			this.LabelProduct.Text = "?";
			this.LabelVersion.AutoSize = true;
			this.LabelVersion.Font = new Font("Times New Roman", 11.25f, FontStyle.Bold, GraphicsUnit.Point, 204);
			this.LabelVersion.Location = new Point(8, 56);
			this.LabelVersion.Name = "LabelVersion";
			this.LabelVersion.Size = new Size(57, 21);
			this.LabelVersion.TabIndex = 1;
			this.LabelVersion.Text = "Version";
			this.btnClose.DialogResult = DialogResult.Cancel;
			this.btnClose.Image = (resources.GetObject("btnClose.Image") as Image);
			this.btnClose.ImageAlign = ContentAlignment.MiddleLeft;
			this.btnClose.Location = new Point(288, 272);
			this.btnClose.Name = "btnClose";
			this.btnClose.Size = new Size(81, 25);
			this.btnClose.TabIndex = 0;
			this.btnClose.Text = "Закрыть";
			this.btnClose.TextAlign = ContentAlignment.MiddleRight;
			this.LabelCopyright.AutoSize = true;
			this.LabelCopyright.Font = new Font("Times New Roman", 11.25f, FontStyle.Bold, GraphicsUnit.Point, 204);
			this.LabelCopyright.Location = new Point(8, 88);
			this.LabelCopyright.Name = "LabelCopyright";
			this.LabelCopyright.Size = new Size(232, 21);
			this.LabelCopyright.TabIndex = 1;
			this.LabelCopyright.Text = "Copyright © Serg V. Zhdanovskih";
			this.LabelMail.AutoSize = true;
			this.LabelMail.Font = new Font("Times New Roman", 8.25f, FontStyle.Bold | FontStyle.Underline, GraphicsUnit.Point, 204);
			this.LabelMail.ForeColor = Color.Blue;
			this.LabelMail.Location = new Point(8, 256);
			this.LabelMail.Name = "LabelMail";
			this.LabelMail.Size = new Size(126, 16);
			this.LabelMail.TabIndex = 2;
			this.LabelMail.Text = "http://gedkeeper.ucoz.ru/";
			this.LabelMail.Click += new EventHandler(this.LabelMail_Click);
			this.LabelCite.Font = new Font("Times New Roman", 12f, FontStyle.Bold, GraphicsUnit.Point, 204);
			this.LabelCite.Location = new Point(8, 144);
			this.LabelCite.Name = "LabelCite";
			this.LabelCite.Size = new Size(361, 97);
			this.LabelCite.TabIndex = 3;
			this.LabelCite.Text = "«История рода - это есть история Отечества» «Неуважение к предкам - есть первый признак дикости и безнравственности» (Александр Сергеевич Пушкин)";
			this.LabelCite.TextAlign = ContentAlignment.TopRight;
			this.AutoScaleBaseSize = new Size(5, 14);
			base.CancelButton = this.btnClose;
			base.ClientSize = new Size(378, 305);
			base.Controls.Add(this.LabelProduct);
			base.Controls.Add(this.LabelVersion);
			base.Controls.Add(this.LabelCopyright);
			base.Controls.Add(this.LabelMail);
			base.Controls.Add(this.btnClose);
			base.Controls.Add(this.LabelCite);
			this.Font = new Font("Tahoma", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 204);
			base.FormBorderStyle = FormBorderStyle.FixedDialog;
			base.MaximizeBox = false;
			base.MinimizeBox = false;
			base.Name = "TfmAbout";
			base.ShowInTaskbar = false;
			base.StartPosition = FormStartPosition.CenterScreen;
			this.Text = "О программе";
			base.ResumeLayout(false);
		}

		public TfmAbout()
		{
			this.InitializeComponent();
			this.LabelCite.Text = "«История рода - это есть история Отечества»\r\n«Неуважение к предкам - есть первый признак дикости и безнравственности»\r\n(Александр Сергеевич Пушкин)";
			this.Text = GKL.LSList[49];
			this.btnClose.Text = GKL.LSList[99];
		}


		public static void ShowAbout(string AppName, string AppVersion)
		{
			TfmAbout dlg = new TfmAbout();
			try
			{
				dlg.LabelProduct.Text = AppName;
				dlg.LabelVersion.Text = "Version " + AppVersion;
				dlg.ShowDialog();
			}
			finally
			{
				TObjectHelper.Free(dlg);
			}
		}

	}
}
