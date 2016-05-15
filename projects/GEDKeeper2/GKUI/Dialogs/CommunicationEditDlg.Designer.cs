using System;

namespace GKUI.Dialogs
{
	partial class CommunicationEditDlg
	{
		private System.Windows.Forms.GroupBox GroupBox1;
		private System.Windows.Forms.TabControl tabsData;
		private System.Windows.Forms.TabPage pageNotes;
		private System.Windows.Forms.TabPage pageMultimedia;
		private System.Windows.Forms.Button btnAccept;
		private System.Windows.Forms.Button btnCancel;
		private System.Windows.Forms.Label lblTheme;
		private System.Windows.Forms.TextBox txtName;
		private System.Windows.Forms.Label lblDate;
		private System.Windows.Forms.MaskedTextBox txtDate;
		private System.Windows.Forms.Label lblType;
		private System.Windows.Forms.ComboBox cmbCorrType;
		private System.Windows.Forms.ComboBox txtDir;
		private System.Windows.Forms.Label lblCorresponder;
		private System.Windows.Forms.TextBox txtCorresponder;
		private System.Windows.Forms.Button btnPersonAdd;
		private System.ComponentModel.IContainer components;
		private System.Windows.Forms.ToolTip toolTip1;

		private void InitializeComponent()
		{
		    this.components = new System.ComponentModel.Container();
		    this.GroupBox1 = new System.Windows.Forms.GroupBox();
		    this.lblTheme = new System.Windows.Forms.Label();
		    this.lblDate = new System.Windows.Forms.Label();
		    this.lblType = new System.Windows.Forms.Label();
		    this.lblCorresponder = new System.Windows.Forms.Label();
		    this.btnPersonAdd = new System.Windows.Forms.Button();
		    this.txtName = new System.Windows.Forms.TextBox();
		    this.txtDate = new System.Windows.Forms.MaskedTextBox();
		    this.cmbCorrType = new System.Windows.Forms.ComboBox();
		    this.txtDir = new System.Windows.Forms.ComboBox();
		    this.txtCorresponder = new System.Windows.Forms.TextBox();
		    this.btnAccept = new System.Windows.Forms.Button();
		    this.btnCancel = new System.Windows.Forms.Button();
		    this.tabsData = new System.Windows.Forms.TabControl();
		    this.pageNotes = new System.Windows.Forms.TabPage();
		    this.pageMultimedia = new System.Windows.Forms.TabPage();
		    this.toolTip1 = new System.Windows.Forms.ToolTip(this.components);
		    this.GroupBox1.SuspendLayout();
		    this.tabsData.SuspendLayout();
		    this.SuspendLayout();
		    // 
		    // GroupBox1
		    // 
		    this.GroupBox1.Controls.Add(this.lblTheme);
		    this.GroupBox1.Controls.Add(this.lblDate);
		    this.GroupBox1.Controls.Add(this.lblType);
		    this.GroupBox1.Controls.Add(this.lblCorresponder);
		    this.GroupBox1.Controls.Add(this.btnPersonAdd);
		    this.GroupBox1.Controls.Add(this.txtName);
		    this.GroupBox1.Controls.Add(this.txtDate);
		    this.GroupBox1.Controls.Add(this.cmbCorrType);
		    this.GroupBox1.Controls.Add(this.txtDir);
		    this.GroupBox1.Controls.Add(this.txtCorresponder);
		    this.GroupBox1.Dock = System.Windows.Forms.DockStyle.Top;
		    this.GroupBox1.Location = new System.Drawing.Point(0, 0);
		    this.GroupBox1.Name = "GroupBox1";
		    this.GroupBox1.Size = new System.Drawing.Size(675, 118);
		    this.GroupBox1.TabIndex = 0;
		    this.GroupBox1.TabStop = false;
		    // 
		    // lblTheme
		    // 
		    this.lblTheme.AutoSize = true;
		    this.lblTheme.Location = new System.Drawing.Point(11, 22);
		    this.lblTheme.Name = "lblTheme";
		    this.lblTheme.Size = new System.Drawing.Size(62, 17);
		    this.lblTheme.TabIndex = 0;
		    this.lblTheme.Text = "lblTheme";
		    // 
		    // lblDate
		    // 
		    this.lblDate.AutoSize = true;
		    this.lblDate.Location = new System.Drawing.Point(322, 81);
		    this.lblDate.Name = "lblDate";
		    this.lblDate.Size = new System.Drawing.Size(49, 17);
		    this.lblDate.TabIndex = 8;
		    this.lblDate.Text = "lblDate";
		    // 
		    // lblType
		    // 
		    this.lblType.AutoSize = true;
		    this.lblType.Location = new System.Drawing.Point(11, 81);
		    this.lblType.Name = "lblType";
		    this.lblType.Size = new System.Drawing.Size(51, 17);
		    this.lblType.TabIndex = 6;
		    this.lblType.Text = "lblType";
		    // 
		    // lblCorresponder
		    // 
		    this.lblCorresponder.AutoSize = true;
		    this.lblCorresponder.Location = new System.Drawing.Point(11, 53);
		    this.lblCorresponder.Name = "lblCorresponder";
		    this.lblCorresponder.Size = new System.Drawing.Size(104, 17);
		    this.lblCorresponder.TabIndex = 2;
		    this.lblCorresponder.Text = "lblCorresponder";
		    // 
		    // btnPersonAdd
		    // 
		    this.btnPersonAdd.Image = global::GKResources.iRecNew;
		    this.btnPersonAdd.Location = new System.Drawing.Point(627, 45);
		    this.btnPersonAdd.Name = "btnPersonAdd";
		    this.btnPersonAdd.Size = new System.Drawing.Size(37, 32);
		    this.btnPersonAdd.TabIndex = 5;
		    this.btnPersonAdd.Click += new System.EventHandler(this.btnPersonAdd_Click);
		    // 
		    // txtName
		    // 
		    this.txtName.Location = new System.Drawing.Point(134, 19);
		    this.txtName.Name = "txtName";
		    this.txtName.Size = new System.Drawing.Size(528, 24);
		    this.txtName.TabIndex = 1;
		    // 
		    // txtDate
		    // 
		    this.txtDate.Location = new System.Drawing.Point(392, 78);
		    this.txtDate.Mask = "00/00/0000";
		    this.txtDate.Name = "txtDate";
		    this.txtDate.Size = new System.Drawing.Size(225, 24);
		    this.txtDate.TabIndex = 9;
		    this.txtDate.TextMaskFormat = System.Windows.Forms.MaskFormat.IncludePromptAndLiterals;
		    // 
		    // cmbCorrType
		    // 
		    this.cmbCorrType.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
		    this.cmbCorrType.Location = new System.Drawing.Point(134, 78);
		    this.cmbCorrType.Name = "cmbCorrType";
		    this.cmbCorrType.Size = new System.Drawing.Size(147, 25);
		    this.cmbCorrType.TabIndex = 7;
		    // 
		    // txtDir
		    // 
		    this.txtDir.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
		    this.txtDir.Location = new System.Drawing.Point(134, 49);
		    this.txtDir.Name = "txtDir";
		    this.txtDir.Size = new System.Drawing.Size(91, 25);
		    this.txtDir.TabIndex = 3;
		    // 
		    // txtCorresponder
		    // 
		    this.txtCorresponder.ForeColor = System.Drawing.SystemColors.Control;
		    this.txtCorresponder.Location = new System.Drawing.Point(235, 49);
		    this.txtCorresponder.Name = "txtCorresponder";
		    this.txtCorresponder.ReadOnly = true;
		    this.txtCorresponder.Size = new System.Drawing.Size(382, 24);
		    this.txtCorresponder.TabIndex = 4;
		    // 
		    // btnAccept
		    // 
		    this.btnAccept.Image = global::GKResources.iBtnAccept;
		    this.btnAccept.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
		    this.btnAccept.Location = new System.Drawing.Point(426, 466);
		    this.btnAccept.Name = "btnAccept";
		    this.btnAccept.Size = new System.Drawing.Size(113, 31);
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
		    this.btnCancel.Location = new System.Drawing.Point(549, 466);
		    this.btnCancel.Name = "btnCancel";
		    this.btnCancel.Size = new System.Drawing.Size(113, 31);
		    this.btnCancel.TabIndex = 3;
		    this.btnCancel.Text = "btnCancel";
		    this.btnCancel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
		    // 
		    // tabsData
		    // 
		    this.tabsData.Controls.Add(this.pageNotes);
		    this.tabsData.Controls.Add(this.pageMultimedia);
		    this.tabsData.Dock = System.Windows.Forms.DockStyle.Top;
		    this.tabsData.Location = new System.Drawing.Point(0, 118);
		    this.tabsData.Name = "tabsData";
		    this.tabsData.SelectedIndex = 0;
		    this.tabsData.Size = new System.Drawing.Size(675, 330);
		    this.tabsData.TabIndex = 1;
		    // 
		    // pageNotes
		    // 
		    this.pageNotes.Location = new System.Drawing.Point(4, 26);
		    this.pageNotes.Name = "pageNotes";
		    this.pageNotes.Size = new System.Drawing.Size(667, 300);
		    this.pageNotes.TabIndex = 0;
		    this.pageNotes.Text = "pageNotes";
		    // 
		    // pageMultimedia
		    // 
		    this.pageMultimedia.Location = new System.Drawing.Point(4, 26);
		    this.pageMultimedia.Name = "pageMultimedia";
		    this.pageMultimedia.Size = new System.Drawing.Size(667, 300);
		    this.pageMultimedia.TabIndex = 1;
		    this.pageMultimedia.Text = "pageMultimedia";
		    // 
		    // CommunicationEditDlg
		    // 
		    this.AcceptButton = this.btnAccept;
		    this.AutoScaleDimensions = new System.Drawing.SizeF(120F, 120F);
		    this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
		    this.CancelButton = this.btnCancel;
		    this.ClientSize = new System.Drawing.Size(675, 513);
		    this.Controls.Add(this.tabsData);
		    this.Controls.Add(this.GroupBox1);
		    this.Controls.Add(this.btnAccept);
		    this.Controls.Add(this.btnCancel);
		    this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
		    this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
		    this.MaximizeBox = false;
		    this.MinimizeBox = false;
		    this.Name = "CommunicationEditDlg";
		    this.ShowInTaskbar = false;
		    this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
		    this.Text = "CommunicationEditDlg";
		    this.GroupBox1.ResumeLayout(false);
		    this.GroupBox1.PerformLayout();
		    this.tabsData.ResumeLayout(false);
		    this.ResumeLayout(false);
		}
	}
}