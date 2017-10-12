namespace GKUI.Forms
{
	partial class OrganizerWin
	{
		private System.Windows.Forms.TabControl tabsData;
		private System.Windows.Forms.TabPage pageAddresses;
		private System.Windows.Forms.TabPage pageTelephones;
		private System.Windows.Forms.TabPage pageMails;
		private System.Windows.Forms.TabPage pageWebs;

		private void InitializeComponent()
		{
		    this.tabsData = new System.Windows.Forms.TabControl();
		    this.pageAddresses = new System.Windows.Forms.TabPage();
		    this.pageTelephones = new System.Windows.Forms.TabPage();
		    this.pageMails = new System.Windows.Forms.TabPage();
		    this.pageWebs = new System.Windows.Forms.TabPage();
		    this.tabsData.SuspendLayout();
		    this.SuspendLayout();
		    // 
		    // tabsData
		    // 
		    this.tabsData.Controls.Add(this.pageAddresses);
		    this.tabsData.Controls.Add(this.pageTelephones);
		    this.tabsData.Controls.Add(this.pageMails);
		    this.tabsData.Controls.Add(this.pageWebs);
		    this.tabsData.Dock = System.Windows.Forms.DockStyle.Fill;
		    this.tabsData.Location = new System.Drawing.Point(0, 0);
		    this.tabsData.Name = "tabsData";
		    this.tabsData.SelectedIndex = 0;
		    this.tabsData.Size = new System.Drawing.Size(785, 539);
		    this.tabsData.TabIndex = 0;
		    // 
		    // pageAddresses
		    // 
		    this.pageAddresses.Location = new System.Drawing.Point(4, 26);
		    this.pageAddresses.Name = "pageAddresses";
		    this.pageAddresses.Size = new System.Drawing.Size(777, 509);
		    this.pageAddresses.TabIndex = 0;
		    this.pageAddresses.Text = "pageAddresses";
		    // 
		    // pageTelephones
		    // 
		    this.pageTelephones.Location = new System.Drawing.Point(4, 26);
		    this.pageTelephones.Name = "pageTelephones";
		    this.pageTelephones.Size = new System.Drawing.Size(777, 509);
		    this.pageTelephones.TabIndex = 1;
		    this.pageTelephones.Text = "pageTelephones";
		    // 
		    // pageMails
		    // 
		    this.pageMails.Location = new System.Drawing.Point(4, 26);
		    this.pageMails.Name = "pageMails";
		    this.pageMails.Size = new System.Drawing.Size(777, 509);
		    this.pageMails.TabIndex = 2;
		    this.pageMails.Text = "pageMails";
		    // 
		    // pageWebs
		    // 
		    this.pageWebs.Location = new System.Drawing.Point(4, 26);
		    this.pageWebs.Name = "pageWebs";
		    this.pageWebs.Size = new System.Drawing.Size(777, 509);
		    this.pageWebs.TabIndex = 3;
		    this.pageWebs.Text = "pageWebs";
		    // 
		    // OrganizerWin
		    // 
		    this.AutoScaleDimensions = new System.Drawing.SizeF(120F, 120F);
		    this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
		    this.ClientSize = new System.Drawing.Size(785, 539);
		    this.Controls.Add(this.tabsData);
		    this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
		    this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
		    this.KeyPreview = true;
		    this.MaximizeBox = false;
		    this.MinimizeBox = false;
		    this.Name = "OrganizerWin";
		    this.ShowInTaskbar = false;
		    this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
		    this.Text = "OrganizerWin";
		    this.Load += new System.EventHandler(this.OrganizerWin_Load);
		    this.KeyDown += new System.Windows.Forms.KeyEventHandler(this.OrganizerWin_KeyDown);
		    this.tabsData.ResumeLayout(false);
		    this.ResumeLayout(false);
		}
	}
}
