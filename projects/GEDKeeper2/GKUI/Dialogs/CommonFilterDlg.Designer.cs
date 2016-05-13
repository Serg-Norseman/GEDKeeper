using System;

namespace GKUI.Dialogs
{
	partial class CommonFilterDlg
	{
		private System.Windows.Forms.Button btnAccept;
		private System.Windows.Forms.Button btnCancel;
		protected System.Windows.Forms.TabControl tabsFilters;
		private System.Windows.Forms.TabPage tsFieldsFilter;
		private System.Windows.Forms.Button btnReset;
		private System.Windows.Forms.DataGridView dataGridView1;

		private void InitializeComponent()
		{
		    System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(CommonFilterDlg));
		    this.btnAccept = new System.Windows.Forms.Button();
		    this.btnCancel = new System.Windows.Forms.Button();
		    this.tabsFilters = new System.Windows.Forms.TabControl();
		    this.tsFieldsFilter = new System.Windows.Forms.TabPage();
		    this.dataGridView1 = new System.Windows.Forms.DataGridView();
		    this.btnReset = new System.Windows.Forms.Button();
		    this.tabsFilters.SuspendLayout();
		    this.tsFieldsFilter.SuspendLayout();
		    ((System.ComponentModel.ISupportInitialize)(this.dataGridView1)).BeginInit();
		    this.SuspendLayout();
		    // 
		    // btnAccept
		    // 
		    this.btnAccept.Image = ((System.Drawing.Image)(resources.GetObject("btnAccept.Image")));
		    this.btnAccept.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
		    this.btnAccept.Location = new System.Drawing.Point(569, 514);
		    this.btnAccept.Name = "btnAccept";
		    this.btnAccept.Size = new System.Drawing.Size(113, 30);
		    this.btnAccept.TabIndex = 1;
		    this.btnAccept.Text = "btnAccept";
		    this.btnAccept.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
		    this.btnAccept.Click += new System.EventHandler(this.btnAccept_Click);
		    // 
		    // btnCancel
		    // 
		    this.btnCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
		    this.btnCancel.Image = ((System.Drawing.Image)(resources.GetObject("btnCancel.Image")));
		    this.btnCancel.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
		    this.btnCancel.Location = new System.Drawing.Point(698, 514);
		    this.btnCancel.Name = "btnCancel";
		    this.btnCancel.Size = new System.Drawing.Size(114, 30);
		    this.btnCancel.TabIndex = 2;
		    this.btnCancel.Text = "btnCancel";
		    this.btnCancel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
		    // 
		    // PageControl1
		    // 
		    this.tabsFilters.Controls.Add(this.tsFieldsFilter);
		    this.tabsFilters.Location = new System.Drawing.Point(19, 19);
		    this.tabsFilters.Name = "PageControl1";
		    this.tabsFilters.SelectedIndex = 0;
		    this.tabsFilters.Size = new System.Drawing.Size(793, 487);
		    this.tabsFilters.TabIndex = 0;
		    // 
		    // tsFieldsFilter
		    // 
		    this.tsFieldsFilter.Controls.Add(this.dataGridView1);
		    this.tsFieldsFilter.Location = new System.Drawing.Point(4, 26);
		    this.tsFieldsFilter.Name = "tsFieldsFilter";
		    this.tsFieldsFilter.Size = new System.Drawing.Size(785, 457);
		    this.tsFieldsFilter.TabIndex = 1;
		    this.tsFieldsFilter.Text = "tsFieldsFilter";
		    // 
		    // dataGridView1
		    // 
		    this.dataGridView1.AllowUserToResizeRows = false;
		    this.dataGridView1.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize;
		    this.dataGridView1.Dock = System.Windows.Forms.DockStyle.Fill;
		    this.dataGridView1.Location = new System.Drawing.Point(0, 0);
		    this.dataGridView1.MultiSelect = false;
		    this.dataGridView1.Name = "dataGridView1";
		    this.dataGridView1.Size = new System.Drawing.Size(785, 457);
		    this.dataGridView1.TabIndex = 6;
		    // 
		    // btnReset
		    // 
		    this.btnReset.Location = new System.Drawing.Point(19, 514);
		    this.btnReset.Name = "btnReset";
		    this.btnReset.Size = new System.Drawing.Size(114, 30);
		    this.btnReset.TabIndex = 3;
		    this.btnReset.Text = "btnReset";
		    this.btnReset.Click += new System.EventHandler(this.btnReset_Click);
		    // 
		    // CommonFilterDlg
		    // 
		    this.AcceptButton = this.btnAccept;
		    this.AutoScaleDimensions = new System.Drawing.SizeF(120F, 120F);
		    this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
		    this.AutoSize = true;
		    this.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
		    this.CancelButton = this.btnCancel;
		    this.ClientSize = new System.Drawing.Size(830, 559);
		    this.Controls.Add(this.btnReset);
		    this.Controls.Add(this.btnAccept);
		    this.Controls.Add(this.btnCancel);
		    this.Controls.Add(this.tabsFilters);
		    this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
		    this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
		    this.MaximizeBox = false;
		    this.MinimizeBox = false;
		    this.Name = "CommonFilterDlg";
		    this.Padding = new System.Windows.Forms.Padding(16);
		    this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
		    this.Text = "CommonFilterDlg";
		    this.tabsFilters.ResumeLayout(false);
		    this.tsFieldsFilter.ResumeLayout(false);
		    ((System.ComponentModel.ISupportInitialize)(this.dataGridView1)).EndInit();
		    this.ResumeLayout(false);
		}
	}
}