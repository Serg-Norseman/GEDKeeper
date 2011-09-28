using System;

namespace GKUI
{
	partial class TfmStats
	{
		private System.Windows.Forms.GroupBox GroupBox1;
		private System.Windows.Forms.Panel Panel1;
		private System.Windows.Forms.Panel ToolBar1;
		private System.Windows.Forms.ComboBox cbType;
		private System.Windows.Forms.ListView ListCommon;
		private System.Windows.Forms.ColumnHeader ColumnHeader1;
		private System.Windows.Forms.ColumnHeader ColumnHeader2;
		private System.Windows.Forms.ColumnHeader ColumnHeader3;
		private System.Windows.Forms.ColumnHeader ColumnHeader4;

		private void InitializeComponent()
		{
			this.GroupBox1 = new System.Windows.Forms.GroupBox();
			this.ListCommon = new System.Windows.Forms.ListView();
			this.ColumnHeader1 = new System.Windows.Forms.ColumnHeader();
			this.ColumnHeader2 = new System.Windows.Forms.ColumnHeader();
			this.ColumnHeader3 = new System.Windows.Forms.ColumnHeader();
			this.ColumnHeader4 = new System.Windows.Forms.ColumnHeader();
			this.Panel1 = new System.Windows.Forms.Panel();
			this.ToolBar1 = new System.Windows.Forms.Panel();
			this.cbType = new System.Windows.Forms.ComboBox();
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
			this.GroupBox1.Size = new System.Drawing.Size(893, 210);
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
			this.ListCommon.Location = new System.Drawing.Point(3, 17);
			this.ListCommon.Name = "ListCommon";
			this.ListCommon.Size = new System.Drawing.Size(887, 190);
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
			this.Panel1.Location = new System.Drawing.Point(0, 210);
			this.Panel1.Name = "Panel1";
			this.Panel1.Size = new System.Drawing.Size(893, 364);
			this.Panel1.TabIndex = 2;
			// 
			// ToolBar1
			// 
			this.ToolBar1.Controls.Add(this.cbType);
			this.ToolBar1.Dock = System.Windows.Forms.DockStyle.Top;
			this.ToolBar1.Location = new System.Drawing.Point(0, 0);
			this.ToolBar1.Name = "ToolBar1";
			this.ToolBar1.Size = new System.Drawing.Size(893, 21);
			this.ToolBar1.TabIndex = 0;
			// 
			// cbType
			// 
			this.cbType.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
			this.cbType.Location = new System.Drawing.Point(8, 0);
			this.cbType.Name = "cbType";
			this.cbType.Size = new System.Drawing.Size(233, 21);
			this.cbType.TabIndex = 0;
			this.cbType.SelectedIndexChanged += new System.EventHandler(this.cbType_SelectedIndexChanged);
			// 
			// TfmStats
			// 
			this.AutoScaleBaseSize = new System.Drawing.Size(5, 14);
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
			this.ToolBar1.ResumeLayout(false);
			this.ResumeLayout(false);
		}
	}
}