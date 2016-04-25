using System;

namespace GKUI.Dialogs
{
	partial class OrganizerWin
	{
		private System.Windows.Forms.TabControl PageControl1;
		private System.Windows.Forms.TabPage SheetAddresses;
		private System.Windows.Forms.TabPage SheetTelephones;
		private System.Windows.Forms.TabPage SheetEMails;
		private System.Windows.Forms.TabPage SheetWebs;

		private void InitializeComponent()
		{
			this.PageControl1 = new System.Windows.Forms.TabControl();
			this.SheetAddresses = new System.Windows.Forms.TabPage();
			this.SheetTelephones = new System.Windows.Forms.TabPage();
			this.SheetEMails = new System.Windows.Forms.TabPage();
			this.SheetWebs = new System.Windows.Forms.TabPage();
			this.PageControl1.SuspendLayout();
			this.SuspendLayout();
			this.PageControl1.Controls.Add(this.SheetAddresses);
			this.PageControl1.Controls.Add(this.SheetTelephones);
			this.PageControl1.Controls.Add(this.SheetEMails);
			this.PageControl1.Controls.Add(this.SheetWebs);
			this.PageControl1.Dock = System.Windows.Forms.DockStyle.Fill;
			this.PageControl1.Location = new System.Drawing.Point(0, 0);
			this.PageControl1.Name = "PageControl1";
			this.PageControl1.SelectedIndex = 0;
			this.PageControl1.Size = new System.Drawing.Size(736, 476);
			this.PageControl1.TabIndex = 0;
			this.SheetAddresses.Location = new System.Drawing.Point(4, 22);
			this.SheetAddresses.Name = "SheetAddresses";
			this.SheetAddresses.Size = new System.Drawing.Size(728, 450);
			this.SheetAddresses.TabIndex = 0;
			this.SheetAddresses.Text = "Адреса";
			this.SheetTelephones.Location = new System.Drawing.Point(4, 22);
			this.SheetTelephones.Name = "SheetTelephones";
			this.SheetTelephones.Size = new System.Drawing.Size(728, 496);
			this.SheetTelephones.TabIndex = 1;
			this.SheetTelephones.Text = "Телефоны";
			this.SheetEMails.Location = new System.Drawing.Point(4, 22);
			this.SheetEMails.Name = "SheetEMails";
			this.SheetEMails.Size = new System.Drawing.Size(728, 496);
			this.SheetEMails.TabIndex = 2;
			this.SheetEMails.Text = "Почта";
			this.SheetWebs.Location = new System.Drawing.Point(4, 22);
			this.SheetWebs.Name = "SheetWebs";
			this.SheetWebs.Size = new System.Drawing.Size(728, 496);
			this.SheetWebs.TabIndex = 3;
			this.SheetWebs.Text = "Сайты";
			this.AutoScaleBaseSize = new System.Drawing.Size(5, 14);
			this.ClientSize = new System.Drawing.Size(736, 476);
			this.Controls.Add(this.PageControl1);
			this.Font = new System.Drawing.Font("Tahoma", 8.25f, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, 204);
			this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
			this.MaximizeBox = false;
			this.MinimizeBox = false;
			this.Name = "TfmOrganizer";
			this.ShowInTaskbar = false;
			this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
			this.Text = "Органайзер";
			this.Load += new System.EventHandler(this.TfmOrganizer_Load);
			this.PageControl1.ResumeLayout(false);
			this.ResumeLayout(false);
		}
	}
}