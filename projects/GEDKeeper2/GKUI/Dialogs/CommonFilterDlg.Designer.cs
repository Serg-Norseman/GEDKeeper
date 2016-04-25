using System;

namespace GKUI.Dialogs
{
	partial class CommonFilterDlg
	{
		private System.Windows.Forms.Button btnAccept;
		private System.Windows.Forms.Button btnCancel;
		protected System.Windows.Forms.TabControl PageControl1;
		private System.Windows.Forms.TabPage tsFieldsFilter;

		private void InitializeComponent()
		{
			this.btnAccept = new System.Windows.Forms.Button();
			this.btnCancel = new System.Windows.Forms.Button();
			this.PageControl1 = new System.Windows.Forms.TabControl();
			this.tsFieldsFilter = new System.Windows.Forms.TabPage();
			this.dataGridView1 = new System.Windows.Forms.DataGridView();
			this.btnReset = new System.Windows.Forms.Button();
			this.PageControl1.SuspendLayout();
			this.tsFieldsFilter.SuspendLayout();
			((System.ComponentModel.ISupportInitialize)(this.dataGridView1)).BeginInit();
			this.SuspendLayout();
			// 
			// btnAccept
			// 
			this.btnAccept.Image = global::GKResources.iBtnAccept;
			this.btnAccept.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
			this.btnAccept.Location = new System.Drawing.Point(556, 514);
			this.btnAccept.Name = "btnAccept";
			this.btnAccept.Size = new System.Drawing.Size(113, 30);
			this.btnAccept.TabIndex = 1;
			this.btnAccept.Text = "Принять";
			this.btnAccept.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
			this.btnAccept.Click += new System.EventHandler(this.btnAccept_Click);
			// 
			// btnCancel
			// 
			this.btnCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
			this.btnCancel.Image = global::GKResources.iBtnCancel;
			this.btnCancel.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
			this.btnCancel.Location = new System.Drawing.Point(690, 514);
			this.btnCancel.Name = "btnCancel";
			this.btnCancel.Size = new System.Drawing.Size(114, 30);
			this.btnCancel.TabIndex = 2;
			this.btnCancel.Text = "Закрыть";
			this.btnCancel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
			// 
			// PageControl1
			// 
			this.PageControl1.Controls.Add(this.tsFieldsFilter);
			this.PageControl1.Location = new System.Drawing.Point(11, 10);
			this.PageControl1.Name = "PageControl1";
			this.PageControl1.SelectedIndex = 0;
			this.PageControl1.Size = new System.Drawing.Size(793, 487);
			this.PageControl1.TabIndex = 0;
			// 
			// tsFieldsFilter
			// 
			this.tsFieldsFilter.Controls.Add(this.dataGridView1);
			this.tsFieldsFilter.Location = new System.Drawing.Point(4, 26);
			this.tsFieldsFilter.Name = "tsFieldsFilter";
			this.tsFieldsFilter.Size = new System.Drawing.Size(785, 457);
			this.tsFieldsFilter.TabIndex = 1;
			this.tsFieldsFilter.Text = "Фильтр полей";
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
			this.btnReset.Location = new System.Drawing.Point(11, 514);
			this.btnReset.Name = "btnReset";
			this.btnReset.Size = new System.Drawing.Size(114, 30);
			this.btnReset.TabIndex = 3;
			this.btnReset.Text = "Сбросить";
			this.btnReset.Click += new System.EventHandler(this.btnReset_Click);
			// 
			// CommonFilterDlg
			// 
			this.AcceptButton = this.btnAccept;
			this.AutoScaleDimensions = new System.Drawing.SizeF(120F, 120F);
			this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
			this.CancelButton = this.btnCancel;
			this.ClientSize = new System.Drawing.Size(816, 559);
			this.Controls.Add(this.btnReset);
			this.Controls.Add(this.btnAccept);
			this.Controls.Add(this.btnCancel);
			this.Controls.Add(this.PageControl1);
			this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
			this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
			this.MaximizeBox = false;
			this.MinimizeBox = false;
			this.Name = "CommonFilterDlg";
			this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
			this.Text = "Фильтр";
			this.PageControl1.ResumeLayout(false);
			this.tsFieldsFilter.ResumeLayout(false);
			((System.ComponentModel.ISupportInitialize)(this.dataGridView1)).EndInit();
			this.ResumeLayout(false);
		}
		private System.Windows.Forms.Button btnReset;
		private System.Windows.Forms.DataGridView dataGridView1;
	}
}