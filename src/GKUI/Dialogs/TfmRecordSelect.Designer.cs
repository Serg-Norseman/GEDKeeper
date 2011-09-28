using System;

namespace GKUI
{
	partial class TfmRecordSelect
	{
		private System.Windows.Forms.Button btnSelect;
		private System.Windows.Forms.Button btnCreate;
		private System.Windows.Forms.Button btnCancel;
		private System.Windows.Forms.Panel panList;
		private System.Windows.Forms.Panel panFilter;
		public System.Windows.Forms.TextBox edFastFilter;

		private void InitializeComponent()
		{
			this.btnSelect = new System.Windows.Forms.Button();
			this.btnCreate = new System.Windows.Forms.Button();
			this.btnCancel = new System.Windows.Forms.Button();
			this.panFilter = new System.Windows.Forms.Panel();
			this.edFastFilter = new System.Windows.Forms.TextBox();
			this.panList = new System.Windows.Forms.Panel();
			this.panFilter.SuspendLayout();
			this.SuspendLayout();
			// 
			// btnSelect
			// 
			this.btnSelect.Image = global::GKResources.iBtnAccept;
			this.btnSelect.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
			this.btnSelect.Location = new System.Drawing.Point(200, 384);
			this.btnSelect.Name = "btnSelect";
			this.btnSelect.Size = new System.Drawing.Size(81, 25);
			this.btnSelect.TabIndex = 3;
			this.btnSelect.Text = "Выбрать";
			this.btnSelect.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
			this.btnSelect.Click += new System.EventHandler(this.btnSelect_Click);
			// 
			// btnCreate
			// 
			this.btnCreate.Location = new System.Drawing.Point(104, 384);
			this.btnCreate.Name = "btnCreate";
			this.btnCreate.Size = new System.Drawing.Size(81, 25);
			this.btnCreate.TabIndex = 2;
			this.btnCreate.Text = "Добавить";
			this.btnCreate.Click += new System.EventHandler(this.btnCreate_Click);
			// 
			// btnCancel
			// 
			this.btnCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
			this.btnCancel.Image = global::GKResources.iBtnCancel;
			this.btnCancel.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
			this.btnCancel.Location = new System.Drawing.Point(296, 384);
			this.btnCancel.Name = "btnCancel";
			this.btnCancel.Size = new System.Drawing.Size(81, 25);
			this.btnCancel.TabIndex = 4;
			this.btnCancel.Text = "Отменить";
			this.btnCancel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
			// 
			// panFilter
			// 
			this.panFilter.Controls.Add(this.edFastFilter);
			this.panFilter.Dock = System.Windows.Forms.DockStyle.Top;
			this.panFilter.Location = new System.Drawing.Point(0, 0);
			this.panFilter.Name = "panFilter";
			this.panFilter.Size = new System.Drawing.Size(385, 41);
			this.panFilter.TabIndex = 0;
			// 
			// edFastFilter
			// 
			this.edFastFilter.Location = new System.Drawing.Point(8, 8);
			this.edFastFilter.Name = "edFastFilter";
			this.edFastFilter.Size = new System.Drawing.Size(361, 21);
			this.edFastFilter.TabIndex = 0;
			this.edFastFilter.TextChanged += new System.EventHandler(this.edFastFilter_TextChanged);
			// 
			// panList
			// 
			this.panList.Dock = System.Windows.Forms.DockStyle.Top;
			this.panList.Location = new System.Drawing.Point(0, 41);
			this.panList.Name = "panList";
			this.panList.Size = new System.Drawing.Size(385, 329);
			this.panList.TabIndex = 1;
			// 
			// TfmRecordSelect
			// 
			this.AutoScaleBaseSize = new System.Drawing.Size(5, 14);
			this.CancelButton = this.btnCancel;
			this.ClientSize = new System.Drawing.Size(385, 417);
			this.Controls.Add(this.panList);
			this.Controls.Add(this.panFilter);
			this.Controls.Add(this.btnSelect);
			this.Controls.Add(this.btnCreate);
			this.Controls.Add(this.btnCancel);
			this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
			this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
			this.KeyPreview = true;
			this.MaximizeBox = false;
			this.MinimizeBox = false;
			this.Name = "TfmRecordSelect";
			this.ShowInTaskbar = false;
			this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
			this.Text = "Выбор записи";
			this.panFilter.ResumeLayout(false);
			this.panFilter.PerformLayout();
			this.ResumeLayout(false);
		}
	}
}