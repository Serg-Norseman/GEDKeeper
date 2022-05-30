namespace GKUI.Forms
{
	partial class CommonFilterDlg
	{
        protected System.Windows.Forms.Button btnAccept;
        protected System.Windows.Forms.Button btnCancel;
		protected System.Windows.Forms.TabControl tabsFilters;
        protected System.Windows.Forms.TabPage pageFieldsFilter;
        protected System.Windows.Forms.Button btnReset;
        private System.Windows.Forms.Panel pnlClient;
        private System.Windows.Forms.Panel pnlButtons;

		private void InitializeComponent()
		{
		    this.pnlButtons = new System.Windows.Forms.Panel();
		    this.btnReset = new System.Windows.Forms.Button();
		    this.btnAccept = new System.Windows.Forms.Button();
		    this.btnCancel = new System.Windows.Forms.Button();
		    this.pnlClient = new System.Windows.Forms.Panel();
		    this.tabsFilters = new System.Windows.Forms.TabControl();
		    this.pageFieldsFilter = new System.Windows.Forms.TabPage();
		    this.pnlButtons.SuspendLayout();
		    this.pnlClient.SuspendLayout();
		    this.tabsFilters.SuspendLayout();
		    this.pageFieldsFilter.SuspendLayout();
		    this.SuspendLayout();
		    // 
		    // pnlButtons
		    // 
		    this.pnlButtons.Controls.Add(this.btnReset);
		    this.pnlButtons.Controls.Add(this.btnAccept);
		    this.pnlButtons.Controls.Add(this.btnCancel);
		    this.pnlButtons.Dock = System.Windows.Forms.DockStyle.Bottom;
		    this.pnlButtons.Location = new System.Drawing.Point(0, 526);
		    this.pnlButtons.Margin = new System.Windows.Forms.Padding(0);
		    this.pnlButtons.Name = "pnlButtons";
		    this.pnlButtons.Size = new System.Drawing.Size(859, 62);
		    this.pnlButtons.TabIndex = 5;
		    // 
		    // btnReset
		    // 
		    this.btnReset.Location = new System.Drawing.Point(16, 14);
		    this.btnReset.Margin = new System.Windows.Forms.Padding(16, 16, 0, 0);
		    this.btnReset.Name = "btnReset";
		    this.btnReset.Size = new System.Drawing.Size(114, 30);
		    this.btnReset.TabIndex = 6;
		    this.btnReset.Text = "btnReset";
		    this.btnReset.Click += new System.EventHandler(this.btnReset_Click);
		    // 
		    // btnAccept
		    // 
		    this.btnAccept.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
		    this.btnAccept.Location = new System.Drawing.Point(600, 16);
		    this.btnAccept.Margin = new System.Windows.Forms.Padding(0, 16, 16, 0);
		    this.btnAccept.Name = "btnAccept";
		    this.btnAccept.Size = new System.Drawing.Size(112, 30);
		    this.btnAccept.TabIndex = 4;
		    this.btnAccept.Text = "btnAccept";
		    this.btnAccept.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
		    this.btnAccept.Click += new System.EventHandler(this.AcceptClickHandler);
		    // 
		    // btnCancel
		    // 
		    this.btnCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
		    this.btnCancel.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
		    this.btnCancel.Location = new System.Drawing.Point(729, 16);
		    this.btnCancel.Margin = new System.Windows.Forms.Padding(0, 16, 16, 0);
		    this.btnCancel.Name = "btnCancel";
		    this.btnCancel.Size = new System.Drawing.Size(114, 30);
		    this.btnCancel.TabIndex = 5;
		    this.btnCancel.Text = "btnCancel";
		    this.btnCancel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
		    // 
		    // pnlClient
		    // 
		    this.pnlClient.Controls.Add(this.tabsFilters);
		    this.pnlClient.Dock = System.Windows.Forms.DockStyle.Fill;
		    this.pnlClient.Location = new System.Drawing.Point(0, 0);
		    this.pnlClient.Margin = new System.Windows.Forms.Padding(2);
		    this.pnlClient.Name = "pnlClient";
		    this.pnlClient.Padding = new System.Windows.Forms.Padding(16, 16, 16, 16);
		    this.pnlClient.Size = new System.Drawing.Size(859, 526);
		    this.pnlClient.TabIndex = 6;
		    // 
		    // tabsFilters
		    // 
		    this.tabsFilters.Controls.Add(this.pageFieldsFilter);
		    this.tabsFilters.Dock = System.Windows.Forms.DockStyle.Fill;
		    this.tabsFilters.Location = new System.Drawing.Point(16, 16);
		    this.tabsFilters.Margin = new System.Windows.Forms.Padding(0, 0, 0, 16);
		    this.tabsFilters.Name = "tabsFilters";
		    this.tabsFilters.SelectedIndex = 0;
		    this.tabsFilters.Size = new System.Drawing.Size(827, 494);
		    this.tabsFilters.TabIndex = 1;
            // 
            // pageFieldsFilter
            // 
            this.pageFieldsFilter.Location = new System.Drawing.Point(4, 26);
		    this.pageFieldsFilter.Margin = new System.Windows.Forms.Padding(2);
		    this.pageFieldsFilter.Name = "tsFieldsFilter";
		    this.pageFieldsFilter.Size = new System.Drawing.Size(819, 464);
		    this.pageFieldsFilter.TabIndex = 1;
		    this.pageFieldsFilter.Text = "tsFieldsFilter";
		    // 
		    // CommonFilterDlg
		    // 
		    this.AcceptButton = this.btnAccept;
		    this.AutoScaleDimensions = new System.Drawing.SizeF(120F, 120F);
		    this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
		    this.CancelButton = this.btnCancel;
		    this.ClientSize = new System.Drawing.Size(859, 588);
		    this.Controls.Add(this.pnlClient);
		    this.Controls.Add(this.pnlButtons);
		    this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
		    this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
		    this.Margin = new System.Windows.Forms.Padding(2);
		    this.MaximizeBox = false;
		    this.MinimizeBox = false;
		    this.Name = "CommonFilterDlg";
		    this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
		    this.Text = "CommonFilterDlg";
		    this.pnlButtons.ResumeLayout(false);
		    this.pnlClient.ResumeLayout(false);
		    this.tabsFilters.ResumeLayout(false);
		    this.pageFieldsFilter.ResumeLayout(false);
		    this.ResumeLayout(false);
		}
	}
}
