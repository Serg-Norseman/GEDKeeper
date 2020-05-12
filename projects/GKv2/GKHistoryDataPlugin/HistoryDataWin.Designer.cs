namespace GKHistoryDataPlugin
{
	partial class HistoryDataWin
	{

		private void InitializeComponent()
		{
		    this.toolStrip1 = new System.Windows.Forms.ToolStrip();
		    this.btnCheck = new System.Windows.Forms.ToolStripButton();
		    this.btnLoadFile = new System.Windows.Forms.ToolStripButton();
		    this.toolStripSeparator1 = new System.Windows.Forms.ToolStripSeparator();
		    this.cbDataFiles = new System.Windows.Forms.ToolStripComboBox();
		    this.lvData = new GKUI.Components.GKListView();
		    this.toolStrip1.SuspendLayout();
		    this.SuspendLayout();
		    // 
		    // toolStrip1
		    // 
		    this.toolStrip1.ImageScalingSize = new System.Drawing.Size(20, 20);
		    this.toolStrip1.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
		    		    		    this.btnCheck,
		    		    		    this.btnLoadFile,
		    		    		    this.toolStripSeparator1,
		    		    		    this.cbDataFiles});
		    this.toolStrip1.Location = new System.Drawing.Point(0, 0);
		    this.toolStrip1.Name = "toolStrip1";
		    this.toolStrip1.Size = new System.Drawing.Size(694, 28);
		    this.toolStrip1.TabIndex = 0;
		    this.toolStrip1.Text = "toolStrip1";
		    // 
		    // btnCheck
		    // 
		    this.btnCheck.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text;
		    this.btnCheck.ImageTransparentColor = System.Drawing.Color.Magenta;
		    this.btnCheck.Name = "btnCheck";
		    this.btnCheck.Size = new System.Drawing.Size(52, 25);
		    this.btnCheck.Text = "Check";
		    this.btnCheck.Click += new System.EventHandler(this.btnCheck_Click);
		    // 
		    // btnLoadFile
		    // 
		    this.btnLoadFile.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text;
		    this.btnLoadFile.ImageTransparentColor = System.Drawing.Color.Magenta;
		    this.btnLoadFile.Name = "btnLoadFile";
		    this.btnLoadFile.Size = new System.Drawing.Size(71, 25);
		    this.btnLoadFile.Text = "Load file";
		    this.btnLoadFile.Click += new System.EventHandler(this.btnLoadFile_Click);
		    // 
		    // toolStripSeparator1
		    // 
		    this.toolStripSeparator1.Name = "toolStripSeparator1";
		    this.toolStripSeparator1.Size = new System.Drawing.Size(6, 28);
		    // 
		    // cbDataFiles
		    // 
		    this.cbDataFiles.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
		    this.cbDataFiles.Name = "cbDataFiles";
		    this.cbDataFiles.Size = new System.Drawing.Size(200, 28);
		    // 
		    // lvData
		    // 
		    this.lvData.Dock = System.Windows.Forms.DockStyle.Fill;
		    this.lvData.FullRowSelect = true;
		    this.lvData.HideSelection = false;
		    this.lvData.ListMan = null;
		    this.lvData.Location = new System.Drawing.Point(0, 28);
		    this.lvData.Name = "lvData";
		    this.lvData.Order = BSLib.Design.BSDTypes.SortOrder.None;
		    this.lvData.OwnerDraw = true;
		    this.lvData.Size = new System.Drawing.Size(694, 369);
		    this.lvData.SortColumn = 0;
		    this.lvData.TabIndex = 1;
		    this.lvData.UseCompatibleStateImageBehavior = false;
		    this.lvData.View = System.Windows.Forms.View.Details;
		    this.lvData.MouseDoubleClick += new System.Windows.Forms.MouseEventHandler(this.lvData_MouseDoubleClick);
		    // 
		    // HistoryDataWin
		    // 
		    this.AutoScaleDimensions = new System.Drawing.SizeF(8F, 17F);
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
		private System.Windows.Forms.ToolStripButton btnCheck;
		private GKUI.Components.GKListView lvData;
		private System.Windows.Forms.ToolStripButton btnLoadFile;
		private System.Windows.Forms.ToolStripSeparator toolStripSeparator1;
		private System.Windows.Forms.ToolStripComboBox cbDataFiles;
	}
}