namespace GKHistoryDataPlugin
{
	partial class HistoryDataWin
	{

		private void InitializeComponent()
		{
		    System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(HistoryDataWin));
		    this.toolStrip1 = new System.Windows.Forms.ToolStrip();
		    this.toolStripButton1 = new System.Windows.Forms.ToolStripButton();
		    this.lvData = new GKUI.Components.GKListView();
		    this.toolStripButton2 = new System.Windows.Forms.ToolStripButton();
		    this.toolStrip1.SuspendLayout();
		    this.SuspendLayout();
		    // 
		    // toolStrip1
		    // 
		    this.toolStrip1.ImageScalingSize = new System.Drawing.Size(20, 20);
		    this.toolStrip1.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.toolStripButton1,
            this.toolStripButton2});
		    this.toolStrip1.Location = new System.Drawing.Point(0, 0);
		    this.toolStrip1.Name = "toolStrip1";
		    this.toolStrip1.Size = new System.Drawing.Size(694, 25);
		    this.toolStrip1.TabIndex = 0;
		    this.toolStrip1.Text = "toolStrip1";
		    // 
		    // toolStripButton1
		    // 
		    this.toolStripButton1.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text;
		    this.toolStripButton1.Enabled = false;
		    this.toolStripButton1.Image = ((System.Drawing.Image)(resources.GetObject("toolStripButton1.Image")));
		    this.toolStripButton1.ImageTransparentColor = System.Drawing.Color.Magenta;
		    this.toolStripButton1.Name = "toolStripButton1";
		    this.toolStripButton1.Size = new System.Drawing.Size(44, 22);
		    this.toolStripButton1.Text = "Check";
		    // 
		    // lvData
		    // 
		    this.lvData.Dock = System.Windows.Forms.DockStyle.Fill;
		    this.lvData.FullRowSelect = true;
		    this.lvData.HideSelection = false;
		    this.lvData.ListMan = null;
		    this.lvData.Location = new System.Drawing.Point(0, 25);
		    this.lvData.Name = "lvData";
		    this.lvData.Order = System.Windows.Forms.SortOrder.None;
		    this.lvData.OwnerDraw = true;
		    this.lvData.Size = new System.Drawing.Size(694, 372);
		    this.lvData.SortColumn = 0;
		    this.lvData.TabIndex = 1;
		    this.lvData.UseCompatibleStateImageBehavior = false;
		    this.lvData.View = System.Windows.Forms.View.Details;
		    this.lvData.MouseDoubleClick += new System.Windows.Forms.MouseEventHandler(this.lvData_MouseDoubleClick);
		    // 
		    // toolStripButton2
		    // 
		    this.toolStripButton2.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text;
		    this.toolStripButton2.Enabled = false;
		    this.toolStripButton2.Image = ((System.Drawing.Image)(resources.GetObject("toolStripButton2.Image")));
		    this.toolStripButton2.ImageTransparentColor = System.Drawing.Color.Magenta;
		    this.toolStripButton2.Name = "toolStripButton2";
		    this.toolStripButton2.Size = new System.Drawing.Size(56, 22);
		    this.toolStripButton2.Text = "Load file";
		    // 
		    // HistoryDataWin
		    // 
		    this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
		    this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
		    this.ClientSize = new System.Drawing.Size(694, 397);
		    this.Controls.Add(this.lvData);
		    this.Controls.Add(this.toolStrip1);
		    this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
		    this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedToolWindow;
		    this.Name = "HistoryDataWin";
		    this.ShowInTaskbar = false;
		    this.StartPosition = System.Windows.Forms.FormStartPosition.Manual;
		    this.Text = "HistoryData";
		    this.TopMost = true;
		    this.Closed += new System.EventHandler(this.HistoryDataWin_Closed);
		    this.Load += new System.EventHandler(this.HistoryDataWin_Load);
		    this.toolStrip1.ResumeLayout(false);
		    this.toolStrip1.PerformLayout();
		    this.ResumeLayout(false);
		    this.PerformLayout();

		}
		private System.Windows.Forms.ToolStrip toolStrip1;
		private System.Windows.Forms.ToolStripButton toolStripButton1;
		private GKUI.Components.GKListView lvData;
		private System.Windows.Forms.ToolStripButton toolStripButton2;
	}
}