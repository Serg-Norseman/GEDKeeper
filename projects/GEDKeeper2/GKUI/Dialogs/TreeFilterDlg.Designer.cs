using System;

namespace GKUI.Dialogs
{
	partial class TreeFilterDlg
	{
		private System.Windows.Forms.Button btnAccept;
		private System.Windows.Forms.Button btnCancel;
		private System.Windows.Forms.Label lblRPSources;
		private System.Windows.Forms.ComboBox cmbSource;
		private System.Windows.Forms.GroupBox rgBranchCut;
		private System.Windows.Forms.RadioButton rbCutNone;
		private System.Windows.Forms.RadioButton rbCutYears;
		private System.Windows.Forms.RadioButton rbCutPersons;
		private System.Windows.Forms.Label lblYear;
		private System.Windows.Forms.NumericUpDown edYear;
		private System.Windows.Forms.Panel Panel1;

		private void InitializeComponent()
		{
			this.btnAccept = new System.Windows.Forms.Button();
			this.btnCancel = new System.Windows.Forms.Button();
			this.lblRPSources = new System.Windows.Forms.Label();
			this.cmbSource = new System.Windows.Forms.ComboBox();
			this.rgBranchCut = new System.Windows.Forms.GroupBox();
			this.lblYear = new System.Windows.Forms.Label();
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
			this.btnAccept.Location = new System.Drawing.Point(302, 398);
			this.btnAccept.Name = "btnAccept";
			this.btnAccept.Size = new System.Drawing.Size(114, 31);
			this.btnAccept.TabIndex = 3;
			this.btnAccept.Text = "btnAccept";
			this.btnAccept.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
			this.btnAccept.Click += new System.EventHandler(this.btnAccept_Click);
			// 
			// btnCancel
			// 
			this.btnCancel.AccessibleName = "";
			this.btnCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
			this.btnCancel.Image = global::GKResources.iBtnCancel;
			this.btnCancel.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
			this.btnCancel.Location = new System.Drawing.Point(426, 398);
			this.btnCancel.Name = "btnCancel";
			this.btnCancel.Size = new System.Drawing.Size(113, 31);
			this.btnCancel.TabIndex = 4;
			this.btnCancel.Text = "btnCancel";
			this.btnCancel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
			this.btnCancel.Click += new System.EventHandler(this.btnCancel_Click);
			// 
			// lblRPSources
			// 
			this.lblRPSources.AutoSize = true;
			this.lblRPSources.Location = new System.Drawing.Point(11, 340);
			this.lblRPSources.Name = "lblRPSources";
			this.lblRPSources.Size = new System.Drawing.Size(79, 17);
			this.lblRPSources.TabIndex = 1;
			this.lblRPSources.Text = "lblRPSources";
			// 
			// cmbSource
			// 
			this.cmbSource.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
			this.cmbSource.Location = new System.Drawing.Point(11, 359);
			this.cmbSource.Name = "cmbSource";
			this.cmbSource.Size = new System.Drawing.Size(528, 25);
			this.cmbSource.TabIndex = 2;
			// 
			// rgBranchCut
			// 
			this.rgBranchCut.Controls.Add(this.lblYear);
			this.rgBranchCut.Controls.Add(this.rbCutNone);
			this.rgBranchCut.Controls.Add(this.rbCutYears);
			this.rgBranchCut.Controls.Add(this.rbCutPersons);
			this.rgBranchCut.Controls.Add(this.edYear);
			this.rgBranchCut.Location = new System.Drawing.Point(11, 10);
			this.rgBranchCut.Name = "rgBranchCut";
			this.rgBranchCut.Size = new System.Drawing.Size(528, 322);
			this.rgBranchCut.TabIndex = 0;
			this.rgBranchCut.TabStop = false;
			this.rgBranchCut.Text = "rgBranchCut";
			// 
			// lblYear
			// 
			this.lblYear.AutoSize = true;
			this.lblYear.Location = new System.Drawing.Point(43, 89);
			this.lblYear.Name = "lblYear";
			this.lblYear.Size = new System.Drawing.Size(31, 17);
			this.lblYear.TabIndex = 2;
			this.lblYear.Text = "lblYear";
			// 
			// rbCutNone
			// 
			this.rbCutNone.AutoSize = true;
			this.rbCutNone.Checked = true;
			this.rbCutNone.Location = new System.Drawing.Point(22, 29);
			this.rbCutNone.Name = "rbCutNone";
			this.rbCutNone.Size = new System.Drawing.Size(52, 21);
			this.rbCutNone.TabIndex = 0;
			this.rbCutNone.TabStop = true;
			this.rbCutNone.Text = "rbCutNone";
			this.rbCutNone.Click += new System.EventHandler(this.rbCutNoneClick);
			// 
			// rbCutYears
			// 
			this.rbCutYears.AutoSize = true;
			this.rbCutYears.Location = new System.Drawing.Point(22, 58);
			this.rbCutYears.Name = "rbCutYears";
			this.rbCutYears.Size = new System.Drawing.Size(128, 21);
			this.rbCutYears.TabIndex = 1;
			this.rbCutYears.Text = "rbCutYears";
			this.rbCutYears.Click += new System.EventHandler(this.rbCutNoneClick);
			// 
			// rbCutPersons
			// 
			this.rbCutPersons.AutoSize = true;
			this.rbCutPersons.Location = new System.Drawing.Point(22, 126);
			this.rbCutPersons.Name = "rbCutPersons";
			this.rbCutPersons.Size = new System.Drawing.Size(156, 21);
			this.rbCutPersons.TabIndex = 4;
			this.rbCutPersons.Text = "rbCutPersons";
			this.rbCutPersons.Click += new System.EventHandler(this.rbCutNoneClick);
			// 
			// edYear
			// 
			this.edYear.Increment = new decimal(new int[] {
									10,
									0,
									0,
									0});
			this.edYear.Location = new System.Drawing.Point(90, 87);
			this.edYear.Maximum = new decimal(new int[] {
									3000,
									0,
									0,
									0});
			this.edYear.Name = "edYear";
			this.edYear.Size = new System.Drawing.Size(169, 24);
			this.edYear.TabIndex = 3;
			// 
			// Panel1
			// 
			this.Panel1.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
			this.Panel1.Location = new System.Drawing.Point(22, 155);
			this.Panel1.Name = "Panel1";
			this.Panel1.Size = new System.Drawing.Size(504, 166);
			this.Panel1.TabIndex = 5;
			// 
			// TreeFilterDlg
			// 
			this.AcceptButton = this.btnAccept;
			this.AutoScaleDimensions = new System.Drawing.SizeF(120F, 120F);
			this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
			this.CancelButton = this.btnCancel;
			this.ClientSize = new System.Drawing.Size(553, 442);
			this.Controls.Add(this.lblRPSources);
			this.Controls.Add(this.cmbSource);
			this.Controls.Add(this.Panel1);
			this.Controls.Add(this.rgBranchCut);
			this.Controls.Add(this.btnAccept);
			this.Controls.Add(this.btnCancel);
			this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
			this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
			this.MaximizeBox = false;
			this.MinimizeBox = false;
			this.Name = "TreeFilterDlg";
			this.ShowInTaskbar = false;
			this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
			this.Text = "TreeFilterDlg";
			this.Load += new System.EventHandler(this.TreeFilterDlg_Load);
			this.rgBranchCut.ResumeLayout(false);
			this.rgBranchCut.PerformLayout();
			((System.ComponentModel.ISupportInitialize)(this.edYear)).EndInit();
			this.ResumeLayout(false);
			this.PerformLayout();
		}
	}
}