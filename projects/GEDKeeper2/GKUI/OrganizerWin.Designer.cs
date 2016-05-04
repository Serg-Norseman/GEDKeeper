namespace GKUI
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
			// 
			// PageControl1
			// 
			this.PageControl1.Controls.Add(this.SheetAddresses);
			this.PageControl1.Controls.Add(this.SheetTelephones);
			this.PageControl1.Controls.Add(this.SheetEMails);
			this.PageControl1.Controls.Add(this.SheetWebs);
			this.PageControl1.Dock = System.Windows.Forms.DockStyle.Fill;
			this.PageControl1.Location = new System.Drawing.Point(0, 0);
			this.PageControl1.Name = "PageControl1";
			this.PageControl1.SelectedIndex = 0;
			this.PageControl1.Size = new System.Drawing.Size(785, 539);
			this.PageControl1.TabIndex = 0;
			// 
			// SheetAddresses
			// 
			this.SheetAddresses.Location = new System.Drawing.Point(4, 26);
			this.SheetAddresses.Name = "SheetAddresses";
			this.SheetAddresses.Size = new System.Drawing.Size(777, 509);
			this.SheetAddresses.TabIndex = 0;
			this.SheetAddresses.Text = "Адреса";
			// 
			// SheetTelephones
			// 
			this.SheetTelephones.Location = new System.Drawing.Point(4, 26);
			this.SheetTelephones.Name = "SheetTelephones";
			this.SheetTelephones.Size = new System.Drawing.Size(777, 509);
			this.SheetTelephones.TabIndex = 1;
			this.SheetTelephones.Text = "Телефоны";
			// 
			// SheetEMails
			// 
			this.SheetEMails.Location = new System.Drawing.Point(4, 26);
			this.SheetEMails.Name = "SheetEMails";
			this.SheetEMails.Size = new System.Drawing.Size(777, 509);
			this.SheetEMails.TabIndex = 2;
			this.SheetEMails.Text = "Почта";
			// 
			// SheetWebs
			// 
			this.SheetWebs.Location = new System.Drawing.Point(4, 26);
			this.SheetWebs.Name = "SheetWebs";
			this.SheetWebs.Size = new System.Drawing.Size(777, 509);
			this.SheetWebs.TabIndex = 3;
			this.SheetWebs.Text = "Сайты";
			// 
			// OrganizerWin
			// 
			this.AutoScaleDimensions = new System.Drawing.SizeF(120F, 120F);
			this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
			this.ClientSize = new System.Drawing.Size(785, 539);
			this.Controls.Add(this.PageControl1);
			this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
			this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
			this.MaximizeBox = false;
			this.MinimizeBox = false;
			this.Name = "OrganizerWin";
			this.ShowInTaskbar = false;
			this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
			this.Text = "Органайзер";
			this.Load += new System.EventHandler(this.TfmOrganizer_Load);
			this.PageControl1.ResumeLayout(false);
			this.ResumeLayout(false);
		}
	}
}