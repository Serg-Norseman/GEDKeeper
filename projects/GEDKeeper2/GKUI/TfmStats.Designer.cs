using System;

namespace GKUI
{
	partial class TfmStats
	{
		private System.Windows.Forms.GroupBox GroupBox1;
		private System.Windows.Forms.Panel Panel1;
		private System.Windows.Forms.ToolStrip ToolBar1;
		private System.Windows.Forms.ToolStripComboBox cbType;
		private System.Windows.Forms.ListView ListCommon;
		private System.Windows.Forms.ColumnHeader ColumnHeader1;
		private System.Windows.Forms.ColumnHeader ColumnHeader2;
		private System.Windows.Forms.ColumnHeader ColumnHeader3;
		private System.Windows.Forms.ColumnHeader ColumnHeader4;
		private System.Windows.Forms.ToolStripSeparator TBS1;
		private System.Windows.Forms.ToolStripSeparator TBS2;
		private System.Windows.Forms.ToolStripButton tbExcelExport;

		private void InitializeComponent()
		{
			this.GroupBox1 = new System.Windows.Forms.GroupBox();
			this.ListCommon = new System.Windows.Forms.ListView();
			this.ColumnHeader1 = new System.Windows.Forms.ColumnHeader();
			this.ColumnHeader2 = new System.Windows.Forms.ColumnHeader();
			this.ColumnHeader3 = new System.Windows.Forms.ColumnHeader();
			this.ColumnHeader4 = new System.Windows.Forms.ColumnHeader();
			this.Panel1 = new System.Windows.Forms.Panel();
			this.ToolBar1 = new System.Windows.Forms.ToolStrip();
			this.TBS1 = new System.Windows.Forms.ToolStripSeparator();
			this.cbType = new System.Windows.Forms.ToolStripComboBox();
			this.TBS2 = new System.Windows.Forms.ToolStripSeparator();
			this.tbExcelExport = new System.Windows.Forms.ToolStripButton();
			this.GroupBox1.SuspendLayout();
			this.Panel1.SuspendLayout();
			this.ToolBar1.SuspendLayout();
			this.SuspendLayout();
			// 
			// GroupBox1
			// 
			this.GroupBox1.Controls.Add(this.ListCommon);
			this.GroupBox1.Dock = System.Windows.Forms.DockStyle.Top;
			this.GroupBox1.Location = new System.Drawing.Point(0, 0);
			this.GroupBox1.Name = "GroupBox1";
			this.GroupBox1.Size = new System.Drawing.Size(893, 267);
			this.GroupBox1.TabIndex = 0;
			this.GroupBox1.TabStop = false;
			this.GroupBox1.Text = "Сводка";
			// 
			// ListCommon
			// 
			this.ListCommon.Columns.AddRange(new System.Windows.Forms.ColumnHeader[] {
									this.ColumnHeader1,
									this.ColumnHeader2,
									this.ColumnHeader3,
									this.ColumnHeader4});
			this.ListCommon.Dock = System.Windows.Forms.DockStyle.Fill;
			this.ListCommon.FullRowSelect = true;
			this.ListCommon.Location = new System.Drawing.Point(3, 20);
			this.ListCommon.Name = "ListCommon";
			this.ListCommon.Size = new System.Drawing.Size(887, 244);
			this.ListCommon.TabIndex = 0;
			this.ListCommon.UseCompatibleStateImageBehavior = false;
			this.ListCommon.View = System.Windows.Forms.View.Details;
			// 
			// ColumnHeader1
			// 
			this.ColumnHeader1.Text = "Параметр";
			this.ColumnHeader1.Width = 300;
			// 
			// ColumnHeader2
			// 
			this.ColumnHeader2.Text = "Всего";
			this.ColumnHeader2.Width = 100;
			// 
			// ColumnHeader3
			// 
			this.ColumnHeader3.Text = "Мужчины";
			this.ColumnHeader3.Width = 100;
			// 
			// ColumnHeader4
			// 
			this.ColumnHeader4.Text = "Женщины";
			this.ColumnHeader4.Width = 100;
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
			this.tbExcelExport.Image = global::GKResources.iExcel;
			this.tbExcelExport.Name = "tbExcelExport";
			this.tbExcelExport.Size = new System.Drawing.Size(23, 25);
			this.tbExcelExport.Click += new System.EventHandler(this.tbExcelExport_Click);
			// 
			// TfmStats
			// 
			this.AutoScaleBaseSize = new System.Drawing.Size(7, 17);
			this.ClientSize = new System.Drawing.Size(893, 574);
			this.Controls.Add(this.Panel1);
			this.Controls.Add(this.GroupBox1);
			this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
			this.KeyPreview = true;
			this.Name = "TfmStats";
			this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
			this.Text = "Статистика";
			this.Load += new System.EventHandler(this.TfmStats_Load);
			this.KeyDown += new System.Windows.Forms.KeyEventHandler(this.TfmStats_KeyDown);
			this.GroupBox1.ResumeLayout(false);
			this.Panel1.ResumeLayout(false);
			this.Panel1.PerformLayout();
			this.ToolBar1.ResumeLayout(false);
			this.ToolBar1.PerformLayout();
			this.ResumeLayout(false);
		}
	}
}