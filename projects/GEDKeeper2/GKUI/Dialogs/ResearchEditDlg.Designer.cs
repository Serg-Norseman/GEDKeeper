using System;

namespace GKUI.Dialogs
{
	partial class ResearchEditDlg
	{
		private System.Windows.Forms.GroupBox GroupBox1;
		private System.Windows.Forms.TextBox txtName;
		private System.Windows.Forms.Label lblName;
		private System.Windows.Forms.TabControl tabsData;
		private System.Windows.Forms.TabPage pageNotes;
		private System.Windows.Forms.TabPage pageTasks;
		private System.Windows.Forms.Button btnAccept;
		private System.Windows.Forms.Button btnCancel;
		private System.Windows.Forms.Label lblPriority;
		private System.Windows.Forms.ComboBox cmbPriority;
		private System.Windows.Forms.TabPage pageCommunications;
		private System.Windows.Forms.Label lblStatus;
		private System.Windows.Forms.ComboBox cmbStatus;
		private System.Windows.Forms.Label lblStartDate;
		private System.Windows.Forms.MaskedTextBox txtStartDate;
		private System.Windows.Forms.Label lblStopDate;
		private System.Windows.Forms.MaskedTextBox txtStopDate;
		private System.Windows.Forms.Label lblPercent;
		private System.Windows.Forms.NumericUpDown nudPercent;
		private System.Windows.Forms.TabPage pageGroups;

		private void InitializeComponent()
		{
			this.GroupBox1 = new System.Windows.Forms.GroupBox();
			this.lblName = new System.Windows.Forms.Label();
			this.lblPriority = new System.Windows.Forms.Label();
			this.lblStatus = new System.Windows.Forms.Label();
			this.lblStartDate = new System.Windows.Forms.Label();
			this.lblStopDate = new System.Windows.Forms.Label();
			this.lblPercent = new System.Windows.Forms.Label();
			this.txtName = new System.Windows.Forms.TextBox();
			this.cmbPriority = new System.Windows.Forms.ComboBox();
			this.cmbStatus = new System.Windows.Forms.ComboBox();
			this.txtStartDate = new System.Windows.Forms.MaskedTextBox();
			this.txtStopDate = new System.Windows.Forms.MaskedTextBox();
			this.nudPercent = new System.Windows.Forms.NumericUpDown();
			this.btnAccept = new System.Windows.Forms.Button();
			this.btnCancel = new System.Windows.Forms.Button();
			this.tabsData = new System.Windows.Forms.TabControl();
			this.pageTasks = new System.Windows.Forms.TabPage();
			this.pageCommunications = new System.Windows.Forms.TabPage();
			this.pageGroups = new System.Windows.Forms.TabPage();
			this.pageNotes = new System.Windows.Forms.TabPage();
			this.GroupBox1.SuspendLayout();
			((System.ComponentModel.ISupportInitialize)(this.nudPercent)).BeginInit();
			this.tabsData.SuspendLayout();
			this.SuspendLayout();
			// 
			// GroupBox1
			// 
			this.GroupBox1.Controls.Add(this.lblName);
			this.GroupBox1.Controls.Add(this.lblPriority);
			this.GroupBox1.Controls.Add(this.lblStatus);
			this.GroupBox1.Controls.Add(this.lblStartDate);
			this.GroupBox1.Controls.Add(this.lblStopDate);
			this.GroupBox1.Controls.Add(this.lblPercent);
			this.GroupBox1.Controls.Add(this.txtName);
			this.GroupBox1.Controls.Add(this.cmbPriority);
			this.GroupBox1.Controls.Add(this.cmbStatus);
			this.GroupBox1.Controls.Add(this.txtStartDate);
			this.GroupBox1.Controls.Add(this.txtStopDate);
			this.GroupBox1.Controls.Add(this.nudPercent);
			this.GroupBox1.Dock = System.Windows.Forms.DockStyle.Top;
			this.GroupBox1.Location = new System.Drawing.Point(0, 0);
			this.GroupBox1.Name = "GroupBox1";
			this.GroupBox1.Size = new System.Drawing.Size(852, 118);
			this.GroupBox1.TabIndex = 0;
			this.GroupBox1.TabStop = false;
			// 
			// lblName
			// 
			this.lblName.AutoSize = true;
			this.lblName.Location = new System.Drawing.Point(11, 22);
			this.lblName.Name = "lblName";
			this.lblName.Size = new System.Drawing.Size(67, 17);
			this.lblName.TabIndex = 0;
			this.lblName.Text = "lblName";
			// 
			// lblPriority
			// 
			this.lblPriority.AutoSize = true;
			this.lblPriority.Location = new System.Drawing.Point(11, 51);
			this.lblPriority.Name = "lblPriority";
			this.lblPriority.Size = new System.Drawing.Size(80, 17);
			this.lblPriority.TabIndex = 2;
			this.lblPriority.Text = "lblPriority";
			// 
			// lblStatus
			// 
			this.lblStatus.AutoSize = true;
			this.lblStatus.Location = new System.Drawing.Point(347, 52);
			this.lblStatus.Name = "lblStatus";
			this.lblStatus.Size = new System.Drawing.Size(78, 17);
			this.lblStatus.TabIndex = 4;
			this.lblStatus.Text = "lblStatus";
			// 
			// lblStartDate
			// 
			this.lblStartDate.AutoSize = true;
			this.lblStartDate.Location = new System.Drawing.Point(11, 81);
			this.lblStartDate.Name = "lblStartDate";
			this.lblStartDate.Size = new System.Drawing.Size(72, 17);
			this.lblStartDate.TabIndex = 8;
			this.lblStartDate.Text = "lblStartDate";
			// 
			// lblStopDate
			// 
			this.lblStopDate.AutoSize = true;
			this.lblStopDate.Location = new System.Drawing.Point(347, 81);
			this.lblStopDate.Name = "lblStopDate";
			this.lblStopDate.Size = new System.Drawing.Size(77, 17);
			this.lblStopDate.TabIndex = 10;
			this.lblStopDate.Text = "lblStopDate";
			// 
			// lblPercent
			// 
			this.lblPercent.AutoSize = true;
			this.lblPercent.Location = new System.Drawing.Point(683, 52);
			this.lblPercent.Name = "lblPercent";
			this.lblPercent.Size = new System.Drawing.Size(64, 17);
			this.lblPercent.TabIndex = 6;
			this.lblPercent.Text = "lblPercent";
			// 
			// txtName
			// 
			this.txtName.Location = new System.Drawing.Point(101, 19);
			this.txtName.Name = "txtName";
			this.txtName.Size = new System.Drawing.Size(740, 24);
			this.txtName.TabIndex = 1;
			// 
			// cmbPriority
			// 
			this.cmbPriority.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
			this.cmbPriority.Location = new System.Drawing.Point(101, 49);
			this.cmbPriority.Name = "cmbPriority";
			this.cmbPriority.Size = new System.Drawing.Size(225, 25);
			this.cmbPriority.TabIndex = 3;
			// 
			// cmbStatus
			// 
			this.cmbStatus.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
			this.cmbStatus.Location = new System.Drawing.Point(437, 49);
			this.cmbStatus.Name = "cmbStatus";
			this.cmbStatus.Size = new System.Drawing.Size(225, 25);
			this.cmbStatus.TabIndex = 5;
			// 
			// txtStartDate
			// 
			this.txtStartDate.Location = new System.Drawing.Point(101, 78);
			this.txtStartDate.Mask = "00/00/0000";
			this.txtStartDate.Name = "txtStartDate";
			this.txtStartDate.Size = new System.Drawing.Size(225, 24);
			this.txtStartDate.TabIndex = 9;
			this.txtStartDate.TextMaskFormat = System.Windows.Forms.MaskFormat.IncludePromptAndLiterals;
			// 
			// txtStopDate
			// 
			this.txtStopDate.Location = new System.Drawing.Point(437, 78);
			this.txtStopDate.Mask = "00/00/0000";
			this.txtStopDate.Name = "txtStopDate";
			this.txtStopDate.Size = new System.Drawing.Size(225, 24);
			this.txtStopDate.TabIndex = 11;
			this.txtStopDate.TextMaskFormat = System.Windows.Forms.MaskFormat.IncludePromptAndLiterals;
			// 
			// nudPercent
			// 
			this.nudPercent.Increment = new decimal(new int[] {
									5,
									0,
									0,
									0});
			this.nudPercent.Location = new System.Drawing.Point(762, 49);
			this.nudPercent.Name = "nudPercent";
			this.nudPercent.Size = new System.Drawing.Size(57, 24);
			this.nudPercent.TabIndex = 7;
			// 
			// btnAccept
			// 
			this.btnAccept.Image = global::GKResources.iBtnAccept;
			this.btnAccept.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
			this.btnAccept.Location = new System.Drawing.Point(605, 515);
			this.btnAccept.Name = "btnAccept";
			this.btnAccept.Size = new System.Drawing.Size(113, 30);
			this.btnAccept.TabIndex = 2;
			this.btnAccept.Text = "btnAccept";
			this.btnAccept.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
			this.btnAccept.Click += new System.EventHandler(this.btnAccept_Click);
			// 
			// btnCancel
			// 
			this.btnCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
			this.btnCancel.Image = global::GKResources.iBtnCancel;
			this.btnCancel.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
			this.btnCancel.Location = new System.Drawing.Point(728, 515);
			this.btnCancel.Name = "btnCancel";
			this.btnCancel.Size = new System.Drawing.Size(113, 30);
			this.btnCancel.TabIndex = 3;
			this.btnCancel.Text = "btnCancel";
			this.btnCancel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
			// 
			// tabsData
			// 
			this.tabsData.Controls.Add(this.pageTasks);
			this.tabsData.Controls.Add(this.pageCommunications);
			this.tabsData.Controls.Add(this.pageGroups);
			this.tabsData.Controls.Add(this.pageNotes);
			this.tabsData.Dock = System.Windows.Forms.DockStyle.Top;
			this.tabsData.Location = new System.Drawing.Point(0, 118);
			this.tabsData.Name = "tabsData";
			this.tabsData.SelectedIndex = 0;
			this.tabsData.Size = new System.Drawing.Size(852, 379);
			this.tabsData.TabIndex = 1;
			// 
			// pageTasks
			// 
			this.pageTasks.Location = new System.Drawing.Point(4, 26);
			this.pageTasks.Name = "pageTasks";
			this.pageTasks.Size = new System.Drawing.Size(844, 349);
			this.pageTasks.TabIndex = 0;
			this.pageTasks.Text = "pageTasks";
			// 
			// pageCommunications
			// 
			this.pageCommunications.Location = new System.Drawing.Point(4, 26);
			this.pageCommunications.Name = "pageCommunications";
			this.pageCommunications.Size = new System.Drawing.Size(844, 349);
			this.pageCommunications.TabIndex = 1;
			this.pageCommunications.Text = "pageCommunications";
			// 
			// pageGroups
			// 
			this.pageGroups.Location = new System.Drawing.Point(4, 26);
			this.pageGroups.Name = "pageGroups";
			this.pageGroups.Size = new System.Drawing.Size(844, 349);
			this.pageGroups.TabIndex = 2;
			this.pageGroups.Text = "pageGroups";
			// 
			// pageNotes
			// 
			this.pageNotes.Location = new System.Drawing.Point(4, 26);
			this.pageNotes.Name = "pageNotes";
			this.pageNotes.Size = new System.Drawing.Size(844, 349);
			this.pageNotes.TabIndex = 3;
			this.pageNotes.Text = "pageNotes";
			// 
			// ResearchEditDlg
			// 
			this.AcceptButton = this.btnAccept;
			this.AutoScaleDimensions = new System.Drawing.SizeF(120F, 120F);
			this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
			this.CancelButton = this.btnCancel;
			this.ClientSize = new System.Drawing.Size(852, 557);
			this.Controls.Add(this.tabsData);
			this.Controls.Add(this.GroupBox1);
			this.Controls.Add(this.btnAccept);
			this.Controls.Add(this.btnCancel);
			this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
			this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
			this.MaximizeBox = false;
			this.MinimizeBox = false;
			this.Name = "ResearchEditDlg";
			this.ShowInTaskbar = false;
			this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
			this.Text = "ResearchEditDlg";
			this.GroupBox1.ResumeLayout(false);
			this.GroupBox1.PerformLayout();
			((System.ComponentModel.ISupportInitialize)(this.nudPercent)).EndInit();
			this.tabsData.ResumeLayout(false);
			this.ResumeLayout(false);
		}
	}
}