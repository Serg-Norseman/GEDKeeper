namespace GKUI.Forms
{
	partial class StatisticsWin
	{
		private System.Windows.Forms.GroupBox grpSummary;
		private System.Windows.Forms.Panel Panel1;
		private System.Windows.Forms.ToolStrip ToolBar1;
		private System.Windows.Forms.ToolStripComboBox cbType;
		private GKUI.Components.GKListView lvSummary;
		private System.Windows.Forms.ToolStripSeparator TBS1;
		private System.Windows.Forms.ToolStripSeparator TBS2;
		private System.Windows.Forms.ToolStripButton tbExcelExport;

		private void InitializeComponent()
		{
			this.grpSummary = new System.Windows.Forms.GroupBox();
			this.lvSummary = new GKUI.Components.GKListView();
			this.Panel1 = new System.Windows.Forms.Panel();
			this.ToolBar1 = new System.Windows.Forms.ToolStrip();
			this.TBS1 = new System.Windows.Forms.ToolStripSeparator();
			this.cbType = new System.Windows.Forms.ToolStripComboBox();
			this.TBS2 = new System.Windows.Forms.ToolStripSeparator();
			this.tbExcelExport = new System.Windows.Forms.ToolStripButton();
			this.grpSummary.SuspendLayout();
			this.Panel1.SuspendLayout();
			this.ToolBar1.SuspendLayout();
			this.SuspendLayout();
			// 
			// grpSummary
			// 
			this.grpSummary.Controls.Add(this.lvSummary);
			this.grpSummary.Dock = System.Windows.Forms.DockStyle.Top;
			this.grpSummary.Location = new System.Drawing.Point(0, 0);
			this.grpSummary.Name = "grpSummary";
			this.grpSummary.Size = new System.Drawing.Size(893, 280);
			this.grpSummary.TabIndex = 0;
			this.grpSummary.TabStop = false;
			this.grpSummary.Text = "grpSummary";
			// 
			// lvSummary
			// 
			this.lvSummary.Dock = System.Windows.Forms.DockStyle.Fill;
			this.lvSummary.FullRowSelect = true;
			this.lvSummary.Location = new System.Drawing.Point(3, 20);
			this.lvSummary.Name = "lvSummary";
			this.lvSummary.Size = new System.Drawing.Size(887, 244);
			this.lvSummary.TabIndex = 0;
			this.lvSummary.UseCompatibleStateImageBehavior = false;
			this.lvSummary.View = System.Windows.Forms.View.Details;
			// 
			// Panel1
			// 
			this.Panel1.Controls.Add(this.ToolBar1);
			this.Panel1.Dock = System.Windows.Forms.DockStyle.Fill;
			this.Panel1.Location = new System.Drawing.Point(0, 267);
			this.Panel1.Name = "Panel1";
			this.Panel1.Size = new System.Drawing.Size(893, 307);
			this.Panel1.TabIndex = 2;
			// 
			// ToolBar1
			// 
			this.ToolBar1.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
									this.TBS1,
									this.cbType,
									this.TBS2,
									this.tbExcelExport});
			this.ToolBar1.Location = new System.Drawing.Point(0, 0);
			this.ToolBar1.Name = "ToolBar1";
			this.ToolBar1.Size = new System.Drawing.Size(893, 28);
			this.ToolBar1.TabIndex = 0;
			// 
			// TBS1
			// 
			this.TBS1.Name = "TBS1";
			this.TBS1.Size = new System.Drawing.Size(6, 28);
			// 
			// cbType
			// 
			this.cbType.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
			this.cbType.Name = "cbType";
			this.cbType.Size = new System.Drawing.Size(326, 28);
			this.cbType.SelectedIndexChanged += new System.EventHandler(this.cbType_SelectedIndexChanged);
			// 
			// TBS2
			// 
			this.TBS2.Name = "TBS2";
			this.TBS2.Size = new System.Drawing.Size(6, 28);
			// 
			// tbExcelExport
			// 
			this.tbExcelExport.Name = "tbExcelExport";
			this.tbExcelExport.Size = new System.Drawing.Size(23, 25);
			this.tbExcelExport.Click += new System.EventHandler(this.tbExcelExport_Click);
			// 
			// StatisticsWin
			// 
			this.AutoScaleDimensions = new System.Drawing.SizeF(120F, 120F);
			this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
			this.ClientSize = new System.Drawing.Size(893, 574);
			this.Controls.Add(this.Panel1);
			this.Controls.Add(this.grpSummary);
			this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
			this.KeyPreview = true;
			this.Name = "StatisticsWin";
			this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
			this.Text = "StatisticsWin";
			this.Load += new System.EventHandler(this.StatisticsWin_Load);
			this.KeyDown += new System.Windows.Forms.KeyEventHandler(this.StatisticsWin_KeyDown);
			this.grpSummary.ResumeLayout(false);
			this.Panel1.ResumeLayout(false);
			this.Panel1.PerformLayout();
			this.ToolBar1.ResumeLayout(false);
			this.ToolBar1.PerformLayout();
			this.ResumeLayout(false);
		}
	}
}