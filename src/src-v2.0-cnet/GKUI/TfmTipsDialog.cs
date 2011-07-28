using GKCore;
using GKSys;
using System;
using System.Drawing;
using System.Resources;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Windows.Forms;

namespace GKUI
{
	public class TfmTipsDialog : Form
	{
		private Panel Shape1;
		private CheckBox ShowCheck;
		private Button NextTipBtn;
		private Button btnClose;
		private Panel Shape2;
		private Panel Shape3;
		private Label TitleLabel;
		private PictureBox Image1;
		private TextBox TipWindow;
		private TStringList FTips;

		private void GetTips()
		{
			if (this.FTips.Count > 0)
			{
				this.TipWindow.Text = this.FTips[0];
				this.FTips.Delete(0);
			}
			this.NextTipBtn.Enabled = (this.FTips.Count > 0);
		}
		private void InitializeComponent()
		{
			ResourceManager resources = new ResourceManager(typeof(TfmTipsDialog));
			this.Shape1 = new Panel();
			this.ShowCheck = new CheckBox();
			this.NextTipBtn = new Button();
			this.btnClose = new Button();
			this.Shape2 = new Panel();
			this.Shape3 = new Panel();
			this.TitleLabel = new Label();
			this.Image1 = new PictureBox();
			this.TipWindow = new TextBox();
			base.SuspendLayout();
			this.Shape1.BackColor = Color.White;
			this.Shape1.BorderStyle = BorderStyle.FixedSingle;
			this.Shape1.ForeColor = Color.Black;
			this.Shape1.Location = new Point(88, 8);
			this.Shape1.Name = "Shape1";
			this.Shape1.Size = new Size(289, 40);
			this.Shape1.TabIndex = 0;
			this.ShowCheck.Checked = true;
			this.ShowCheck.CheckState = CheckState.Checked;
			this.ShowCheck.Location = new Point(18, 220);
			this.ShowCheck.Name = "ShowCheck";
			this.ShowCheck.Size = new Size(167, 17);
			this.ShowCheck.TabIndex = 0;
			this.ShowCheck.Text = "Показывать при старте";
			this.NextTipBtn.Location = new Point(216, 216);
			this.NextTipBtn.Name = "NextTipBtn";
			this.NextTipBtn.Size = new Size(75, 25);
			this.NextTipBtn.TabIndex = 1;
			this.NextTipBtn.Text = "Далее";
			this.NextTipBtn.Click += new EventHandler(this.NextTipBtn_Click);
			this.btnClose.DialogResult = DialogResult.Cancel;
			this.btnClose.Location = new Point(296, 216);
			this.btnClose.Name = "btnClose";
			this.btnClose.Size = new Size(75, 25);
			this.btnClose.TabIndex = 2;
			this.btnClose.Text = "Закрыть";
			this.Shape2.BackColor = Color.Gray;
			this.Shape2.BorderStyle = BorderStyle.FixedSingle;
			this.Shape2.Location = new Point(16, 8);
			this.Shape2.Name = "Shape2";
			this.Shape2.Size = new Size(73, 185);
			this.Shape2.TabIndex = 1;
			this.Shape3.BackColor = Color.White;
			this.Shape3.BorderStyle = BorderStyle.FixedSingle;
			this.Shape3.ForeColor = Color.Black;
			this.Shape3.Location = new Point(88, 47);
			this.Shape3.Name = "Shape3";
			this.Shape3.Size = new Size(289, 146);
			this.Shape3.TabIndex = 2;
			this.TitleLabel.BackColor = Color.White;
			this.TitleLabel.Font = new Font("Arial", 16f, FontStyle.Bold, GraphicsUnit.Point, 204);
			this.TitleLabel.Location = new Point(96, 16);
			this.TitleLabel.Name = "TitleLabel";
			this.TitleLabel.Size = new Size(270, 22);
			this.TitleLabel.TabIndex = 3;
			this.TitleLabel.Text = "Вы знаете что...";
			this.Image1.Image = (resources.GetObject("Image1.Image") as Image);
			this.Image1.Location = new Point(32, 24);
			this.Image1.Name = "Image1";
			this.Image1.Size = new Size(41, 43);
			this.Image1.TabIndex = 4;
			this.Image1.TabStop = false;
			this.TipWindow.BorderStyle = BorderStyle.None;
			this.TipWindow.Location = new Point(98, 57);
			this.TipWindow.Multiline = true;
			this.TipWindow.Name = "TipWindow";
			this.TipWindow.Size = new Size(265, 127);
			this.TipWindow.TabIndex = 3;
			this.TipWindow.Text = "";
			this.AutoScaleBaseSize = new Size(5, 14);
			base.CancelButton = this.btnClose;
			base.ClientSize = new Size(389, 252);
			base.Controls.Add(this.TitleLabel);
			base.Controls.Add(this.Image1);
			base.Controls.Add(this.ShowCheck);
			base.Controls.Add(this.TipWindow);
			base.Controls.Add(this.NextTipBtn);
			base.Controls.Add(this.btnClose);
			base.Controls.Add(this.Shape1);
			base.Controls.Add(this.Shape2);
			base.Controls.Add(this.Shape3);
			this.Font = new Font("Tahoma", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 204);
			base.FormBorderStyle = FormBorderStyle.FixedToolWindow;
			base.MaximizeBox = false;
			base.MinimizeBox = false;
			base.Name = "TfmTipsDialog";
			base.ShowInTaskbar = false;
			base.StartPosition = FormStartPosition.CenterScreen;
			this.Text = " ";
			base.ResumeLayout(false);
		}
		private void NextTipBtn_Click(object sender, EventArgs e)
		{
			this.GetTips();
		}
		protected override void Dispose(bool Disposing)
		{
			if (Disposing)
			{
				this.FTips.Free();
			}
			base.Dispose(Disposing);
		}
		public TfmTipsDialog()
		{
			this.InitializeComponent();
			this.FTips = new TStringList();
			this.btnClose.Text = GKL.LSList[99];
			this.ShowCheck.Text = GKL.LSList[263];
			this.NextTipBtn.Text = GKL.LSList[384];
			this.TitleLabel.Text = GKL.LSList[385];
		}

		public static bool ShowTipsEx([In] string ACaption, bool ShowTipsChecked, TStrings Tips)
		{
			TfmTipsDialog dlg = new TfmTipsDialog();
			bool Result;
			try
			{
				dlg.ShowCheck.Checked = ShowTipsChecked;
				dlg.Text = ACaption;
				dlg.TitleLabel.Text = ACaption;
				dlg.FTips.Assign(Tips);
				dlg.GetTips();
				dlg.ShowDialog();
				Result = dlg.ShowCheck.Checked;
			}
			finally
			{
				TObjectHelper.Free(dlg);
			}
			return Result;
		}
	}
}
