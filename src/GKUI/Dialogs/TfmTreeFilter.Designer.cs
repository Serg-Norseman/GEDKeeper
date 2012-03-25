using System;

namespace GKUI
{
	partial class TfmTreeFilter
	{
		private System.Windows.Forms.Button btnAccept;
		private System.Windows.Forms.Button btnCancel;
		private System.Windows.Forms.Label Label5;
		private System.Windows.Forms.ComboBox cbSource;
		private System.Windows.Forms.GroupBox rgBranchCut;
		private System.Windows.Forms.RadioButton rbCutNone;
		private System.Windows.Forms.RadioButton rbCutYears;
		private System.Windows.Forms.RadioButton rbCutPersons;
		private System.Windows.Forms.Label Label1;
		private System.Windows.Forms.NumericUpDown edYear;
		private System.Windows.Forms.Panel Panel1;

		private void InitializeComponent()
		{
			this.btnAccept = new System.Windows.Forms.Button();
			this.btnCancel = new System.Windows.Forms.Button();
			this.Label5 = new System.Windows.Forms.Label();
			this.cbSource = new System.Windows.Forms.ComboBox();
			this.rgBranchCut = new System.Windows.Forms.GroupBox();
			this.Label1 = new System.Windows.Forms.Label();
			this.rbCutNone = new System.Windows.Forms.RadioButton();
			this.rbCutYears = new System.Windows.Forms.RadioButton();
			this.rbCutPersons = new System.Windows.Forms.RadioButton();
			this.edYear = new System.Windows.Forms.NumericUpDown();
			this.Panel1 = new System.Windows.Forms.Panel();
			this.rgBranchCut.SuspendLayout();
			((System.ComponentModel.ISupportInitialize)(this.edYear)).BeginInit();
			this.SuspendLayout();
			// 
			// btnAccept
			// 
			this.btnAccept.Image = global::GKResources.iBtnAccept;
			this.btnAccept.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
			this.btnAccept.Location = new System.Drawing.Point(216, 328);
			this.btnAccept.Name = "btnAccept";
			this.btnAccept.Size = new System.Drawing.Size(81, 25);
			this.btnAccept.TabIndex = 3;
			this.btnAccept.Text = "Принять";
			this.btnAccept.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
			this.btnAccept.Click += new System.EventHandler(this.btnAccept_Click);
			// 
			// btnCancel
			// 
			this.btnCancel.AccessibleName = "";
			this.btnCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
			this.btnCancel.Image = global::GKResources.iBtnCancel;
			this.btnCancel.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
			this.btnCancel.Location = new System.Drawing.Point(304, 328);
			this.btnCancel.Name = "btnCancel";
			this.btnCancel.Size = new System.Drawing.Size(81, 25);
			this.btnCancel.TabIndex = 4;
			this.btnCancel.Text = "Отменить";
			this.btnCancel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
			this.btnCancel.Click += new System.EventHandler(this.btnCancel_Click);
			// 
			// Label5
			// 
			this.Label5.Location = new System.Drawing.Point(8, 280);
			this.Label5.Name = "Label5";
			this.Label5.Size = new System.Drawing.Size(60, 13);
			this.Label5.TabIndex = 1;
			this.Label5.Text = "Источники";
			// 
			// cbSource
			// 
			this.cbSource.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
			this.cbSource.Location = new System.Drawing.Point(8, 296);
			this.cbSource.Name = "cbSource";
			this.cbSource.Size = new System.Drawing.Size(377, 21);
			this.cbSource.TabIndex = 2;
			// 
			// rgBranchCut
			// 
			this.rgBranchCut.Controls.Add(this.Label1);
			this.rgBranchCut.Controls.Add(this.rbCutNone);
			this.rgBranchCut.Controls.Add(this.rbCutYears);
			this.rgBranchCut.Controls.Add(this.rbCutPersons);
			this.rgBranchCut.Controls.Add(this.edYear);
			this.rgBranchCut.Location = new System.Drawing.Point(8, 8);
			this.rgBranchCut.Name = "rgBranchCut";
			this.rgBranchCut.Size = new System.Drawing.Size(377, 265);
			this.rgBranchCut.TabIndex = 0;
			this.rgBranchCut.TabStop = false;
			this.rgBranchCut.Text = "Отсечение ветвей";
			// 
			// Label1
			// 
			this.Label1.Location = new System.Drawing.Point(32, 80);
			this.Label1.Name = "Label1";
			this.Label1.Size = new System.Drawing.Size(26, 13);
			this.Label1.TabIndex = 2;
			this.Label1.Text = "Год";
			// 
			// rbCutNone
			// 
			this.rbCutNone.Checked = true;
			this.rbCutNone.Location = new System.Drawing.Point(16, 24);
			this.rbCutNone.Name = "rbCutNone";
			this.rbCutNone.Size = new System.Drawing.Size(249, 17);
			this.rbCutNone.TabIndex = 0;
			this.rbCutNone.TabStop = true;
			this.rbCutNone.Text = "нет";
			this.rbCutNone.Click += new System.EventHandler(this.rbCutNoneClick);
			// 
			// rbCutYears
			// 
			this.rbCutYears.Location = new System.Drawing.Point(16, 48);
			this.rbCutYears.Name = "rbCutYears";
			this.rbCutYears.Size = new System.Drawing.Size(249, 17);
			this.rbCutYears.TabIndex = 1;
			this.rbCutYears.Text = "по границе лет";
			this.rbCutYears.Click += new System.EventHandler(this.rbCutNoneClick);
			// 
			// rbCutPersons
			// 
			this.rbCutPersons.Location = new System.Drawing.Point(16, 104);
			this.rbCutPersons.Name = "rbCutPersons";
			this.rbCutPersons.Size = new System.Drawing.Size(249, 17);
			this.rbCutPersons.TabIndex = 4;
			this.rbCutPersons.Text = "по заданным лицам";
			this.rbCutPersons.Click += new System.EventHandler(this.rbCutNoneClick);
			// 
			// edYear
			// 
			this.edYear.Increment = new decimal(new int[] {
									10,
									0,
									0,
									0});
			this.edYear.Location = new System.Drawing.Point(64, 72);
			this.edYear.Maximum = new decimal(new int[] {
									3000,
									0,
									0,
									0});
			this.edYear.Name = "edYear";
			this.edYear.Size = new System.Drawing.Size(121, 21);
			this.edYear.TabIndex = 3;
			// 
			// Panel1
			// 
			this.Panel1.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
			this.Panel1.Location = new System.Drawing.Point(16, 128);
			this.Panel1.Name = "Panel1";
			this.Panel1.Size = new System.Drawing.Size(360, 136);
			this.Panel1.TabIndex = 5;
			// 
			// TfmTreeFilter
			// 
			this.AcceptButton = this.btnAccept;
			this.AutoScaleBaseSize = new System.Drawing.Size(5, 14);
			this.CancelButton = this.btnCancel;
			this.ClientSize = new System.Drawing.Size(393, 361);
			this.Controls.Add(this.Label5);
			this.Controls.Add(this.cbSource);
			this.Controls.Add(this.Panel1);
			this.Controls.Add(this.rgBranchCut);
			this.Controls.Add(this.btnAccept);
			this.Controls.Add(this.btnCancel);
			this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
			this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
			this.MaximizeBox = false;
			this.MinimizeBox = false;
			this.Name = "TfmTreeFilter";
			this.ShowInTaskbar = false;
			this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
			this.Text = "Фильтр";
			this.Load += new System.EventHandler(this.TfmTreeFilter_Load);
			this.rgBranchCut.ResumeLayout(false);
			((System.ComponentModel.ISupportInitialize)(this.edYear)).EndInit();
			this.ResumeLayout(false);
		}
	}
}