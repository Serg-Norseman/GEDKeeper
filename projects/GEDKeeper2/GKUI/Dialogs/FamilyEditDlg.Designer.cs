using System;

namespace GKUI.Dialogs
{
	partial class FamilyEditDlg
	{
		private System.Windows.Forms.TabControl tabsFamilyData;
		private System.Windows.Forms.TabPage pageEvents;
		private System.Windows.Forms.TabPage pageNotes;
		private System.Windows.Forms.TabPage pageMultimedia;
		private System.Windows.Forms.TabPage pageSources;
		private System.Windows.Forms.TabPage pageChilds;
		private System.Windows.Forms.Button btnAccept;
		private System.Windows.Forms.Button btnCancel;
		private System.Windows.Forms.GroupBox GroupBox1;
		private System.Windows.Forms.Label lblHusband;
		private System.Windows.Forms.TextBox txtHusband;
		private System.Windows.Forms.Button btnHusbandAdd;
		private System.Windows.Forms.Button btnHusbandDelete;
		private System.Windows.Forms.Button btnHusbandSel;
		private System.Windows.Forms.Button btnWifeSel;
		private System.Windows.Forms.Button btnWifeDelete;
		private System.Windows.Forms.Button btnWifeAdd;
		private System.Windows.Forms.TextBox txtWife;
		private System.Windows.Forms.Label lblWife;
		private System.Windows.Forms.Label lblStatus;
		private System.Windows.Forms.ComboBox cmbMarriageStatus;
		private System.Windows.Forms.Label lblRestriction;
		private System.Windows.Forms.ComboBox cmbRestriction;

		private void InitializeComponent()
		{
			this.btnAccept = new System.Windows.Forms.Button();
			this.btnCancel = new System.Windows.Forms.Button();
			this.GroupBox1 = new System.Windows.Forms.GroupBox();
			this.lblHusband = new System.Windows.Forms.Label();
			this.btnHusbandAdd = new System.Windows.Forms.Button();
			this.btnHusbandDelete = new System.Windows.Forms.Button();
			this.btnHusbandSel = new System.Windows.Forms.Button();
			this.btnWifeSel = new System.Windows.Forms.Button();
			this.btnWifeDelete = new System.Windows.Forms.Button();
			this.btnWifeAdd = new System.Windows.Forms.Button();
			this.lblWife = new System.Windows.Forms.Label();
			this.lblStatus = new System.Windows.Forms.Label();
			this.txtHusband = new System.Windows.Forms.TextBox();
			this.txtWife = new System.Windows.Forms.TextBox();
			this.cmbMarriageStatus = new System.Windows.Forms.ComboBox();
			this.lblRestriction = new System.Windows.Forms.Label();
			this.cmbRestriction = new System.Windows.Forms.ComboBox();
			this.tabsFamilyData = new System.Windows.Forms.TabControl();
			this.pageChilds = new System.Windows.Forms.TabPage();
			this.pageEvents = new System.Windows.Forms.TabPage();
			this.pageNotes = new System.Windows.Forms.TabPage();
			this.pageMultimedia = new System.Windows.Forms.TabPage();
			this.pageSources = new System.Windows.Forms.TabPage();
			this.GroupBox1.SuspendLayout();
			this.tabsFamilyData.SuspendLayout();
			this.SuspendLayout();
			// 
			// btnAccept
			// 
			this.btnAccept.Image = global::GKResources.iBtnAccept;
			this.btnAccept.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
			this.btnAccept.Location = new System.Drawing.Point(462, 491);
			this.btnAccept.Name = "btnAccept";
			this.btnAccept.Size = new System.Drawing.Size(114, 31);
			this.btnAccept.TabIndex = 4;
			this.btnAccept.Text = "btnAccept";
			this.btnAccept.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
			this.btnAccept.Click += new System.EventHandler(this.btnAccept_Click);
			// 
			// btnCancel
			// 
			this.btnCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
			this.btnCancel.Image = global::GKResources.iBtnCancel;
			this.btnCancel.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
			this.btnCancel.Location = new System.Drawing.Point(582, 491);
			this.btnCancel.Name = "btnCancel";
			this.btnCancel.Size = new System.Drawing.Size(114, 31);
			this.btnCancel.TabIndex = 5;
			this.btnCancel.Text = "btnCancel";
			this.btnCancel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
			// 
			// GroupBox1
			// 
			this.GroupBox1.Controls.Add(this.lblHusband);
			this.GroupBox1.Controls.Add(this.btnHusbandAdd);
			this.GroupBox1.Controls.Add(this.btnHusbandDelete);
			this.GroupBox1.Controls.Add(this.btnHusbandSel);
			this.GroupBox1.Controls.Add(this.btnWifeSel);
			this.GroupBox1.Controls.Add(this.btnWifeDelete);
			this.GroupBox1.Controls.Add(this.btnWifeAdd);
			this.GroupBox1.Controls.Add(this.lblWife);
			this.GroupBox1.Controls.Add(this.lblStatus);
			this.GroupBox1.Controls.Add(this.txtHusband);
			this.GroupBox1.Controls.Add(this.txtWife);
			this.GroupBox1.Controls.Add(this.cmbMarriageStatus);
			this.GroupBox1.Dock = System.Windows.Forms.DockStyle.Top;
			this.GroupBox1.Location = new System.Drawing.Point(0, 0);
			this.GroupBox1.Name = "GroupBox1";
			this.GroupBox1.Size = new System.Drawing.Size(708, 157);
			this.GroupBox1.TabIndex = 0;
			this.GroupBox1.TabStop = false;
			this.GroupBox1.Text = "GroupBox1";
			// 
			// lblHusband
			// 
			this.lblHusband.AutoSize = true;
			this.lblHusband.Location = new System.Drawing.Point(22, 32);
			this.lblHusband.Name = "lblHusband";
			this.lblHusband.Size = new System.Drawing.Size(36, 17);
			this.lblHusband.TabIndex = 0;
			this.lblHusband.Text = "lblHusband";
			// 
			// btnHusbandAdd
			// 
			this.btnHusbandAdd.AccessibleDescription = "Выбрать или добавить мужа";
			this.btnHusbandAdd.Enabled = false;
			this.btnHusbandAdd.Image = global::GKResources.iRecNew;
			this.btnHusbandAdd.Location = new System.Drawing.Point(560, 26);
			this.btnHusbandAdd.Name = "btnHusbandAdd";
			this.btnHusbandAdd.Size = new System.Drawing.Size(39, 34);
			this.btnHusbandAdd.TabIndex = 2;
			this.btnHusbandAdd.Click += new System.EventHandler(this.btnHusbandAddClick);
			// 
			// btnHusbandDelete
			// 
			this.btnHusbandDelete.AccessibleDescription = "Отсоединить мужа";
			this.btnHusbandDelete.Enabled = false;
			this.btnHusbandDelete.Image = global::GKResources.iRecDelete;
			this.btnHusbandDelete.Location = new System.Drawing.Point(601, 26);
			this.btnHusbandDelete.Name = "btnHusbandDelete";
			this.btnHusbandDelete.Size = new System.Drawing.Size(39, 34);
			this.btnHusbandDelete.TabIndex = 3;
			this.btnHusbandDelete.Click += new System.EventHandler(this.btnHusbandDeleteClick);
			// 
			// btnHusbandSel
			// 
			this.btnHusbandSel.AccessibleDescription = "Перейти на запись мужа";
			this.btnHusbandSel.Image = global::GKResources.iToMan;
			this.btnHusbandSel.Location = new System.Drawing.Point(641, 26);
			this.btnHusbandSel.Name = "btnHusbandSel";
			this.btnHusbandSel.Size = new System.Drawing.Size(39, 34);
			this.btnHusbandSel.TabIndex = 4;
			this.btnHusbandSel.Click += new System.EventHandler(this.btnHusbandSelClick);
			// 
			// btnWifeSel
			// 
			this.btnWifeSel.AccessibleDescription = "Перейти на запись жены";
			this.btnWifeSel.Image = global::GKResources.iToMan;
			this.btnWifeSel.Location = new System.Drawing.Point(641, 64);
			this.btnWifeSel.Name = "btnWifeSel";
			this.btnWifeSel.Size = new System.Drawing.Size(39, 34);
			this.btnWifeSel.TabIndex = 9;
			this.btnWifeSel.Click += new System.EventHandler(this.btnWifeSelClick);
			// 
			// btnWifeDelete
			// 
			this.btnWifeDelete.AccessibleDescription = "Отсоединить жену";
			this.btnWifeDelete.Enabled = false;
			this.btnWifeDelete.Image = global::GKResources.iRecDelete;
			this.btnWifeDelete.Location = new System.Drawing.Point(601, 64);
			this.btnWifeDelete.Name = "btnWifeDelete";
			this.btnWifeDelete.Size = new System.Drawing.Size(39, 34);
			this.btnWifeDelete.TabIndex = 8;
			this.btnWifeDelete.Click += new System.EventHandler(this.btnWifeDeleteClick);
			// 
			// btnWifeAdd
			// 
			this.btnWifeAdd.AccessibleDescription = "Выбрать или добавить жену";
			this.btnWifeAdd.Enabled = false;
			this.btnWifeAdd.Image = global::GKResources.iRecNew;
			this.btnWifeAdd.Location = new System.Drawing.Point(560, 64);
			this.btnWifeAdd.Name = "btnWifeAdd";
			this.btnWifeAdd.Size = new System.Drawing.Size(39, 34);
			this.btnWifeAdd.TabIndex = 7;
			this.btnWifeAdd.Click += new System.EventHandler(this.btnWifeAddClick);
			// 
			// lblWife
			// 
			this.lblWife.AutoSize = true;
			this.lblWife.Location = new System.Drawing.Point(22, 71);
			this.lblWife.Name = "lblWife";
			this.lblWife.Size = new System.Drawing.Size(44, 17);
			this.lblWife.TabIndex = 5;
			this.lblWife.Text = "lblWife";
			// 
			// lblStatus
			// 
			this.lblStatus.AutoSize = true;
			this.lblStatus.Location = new System.Drawing.Point(22, 110);
			this.lblStatus.Name = "lblStatus";
			this.lblStatus.Size = new System.Drawing.Size(55, 17);
			this.lblStatus.TabIndex = 10;
			this.lblStatus.Text = "lblStatus";
			// 
			// txtHusband
			// 
			this.txtHusband.ForeColor = System.Drawing.SystemColors.Control;
			this.txtHusband.Location = new System.Drawing.Point(90, 29);
			this.txtHusband.Name = "txtHusband";
			this.txtHusband.ReadOnly = true;
			this.txtHusband.Size = new System.Drawing.Size(460, 24);
			this.txtHusband.TabIndex = 1;
			this.txtHusband.TextChanged += new System.EventHandler(this.EditHusband_TextChanged);
			// 
			// txtWife
			// 
			this.txtWife.ForeColor = System.Drawing.SystemColors.Control;
			this.txtWife.Location = new System.Drawing.Point(90, 68);
			this.txtWife.Name = "txtWife";
			this.txtWife.ReadOnly = true;
			this.txtWife.Size = new System.Drawing.Size(460, 24);
			this.txtWife.TabIndex = 6;
			this.txtWife.TextChanged += new System.EventHandler(this.EditWife_TextChanged);
			// 
			// cmbMarriageStatus
			// 
			this.cmbMarriageStatus.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
			this.cmbMarriageStatus.Location = new System.Drawing.Point(90, 107);
			this.cmbMarriageStatus.Name = "cmbMarriageStatus";
			this.cmbMarriageStatus.Size = new System.Drawing.Size(203, 25);
			this.cmbMarriageStatus.TabIndex = 11;
			// 
			// lblRestriction
			// 
			this.lblRestriction.AutoSize = true;
			this.lblRestriction.Location = new System.Drawing.Point(12, 498);
			this.lblRestriction.Name = "lblRestriction";
			this.lblRestriction.Size = new System.Drawing.Size(187, 17);
			this.lblRestriction.TabIndex = 2;
			this.lblRestriction.Text = "lblRestriction";
			// 
			// cmbRestriction
			// 
			this.cmbRestriction.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
			this.cmbRestriction.Location = new System.Drawing.Point(224, 495);
			this.cmbRestriction.Name = "cmbRestriction";
			this.cmbRestriction.Size = new System.Drawing.Size(203, 25);
			this.cmbRestriction.TabIndex = 3;
			this.cmbRestriction.SelectedIndexChanged += new System.EventHandler(this.cbRestriction_SelectedIndexChanged);
			// 
			// tabsFamilyData
			// 
			this.tabsFamilyData.Controls.Add(this.pageChilds);
			this.tabsFamilyData.Controls.Add(this.pageEvents);
			this.tabsFamilyData.Controls.Add(this.pageNotes);
			this.tabsFamilyData.Controls.Add(this.pageMultimedia);
			this.tabsFamilyData.Controls.Add(this.pageSources);
			this.tabsFamilyData.Dock = System.Windows.Forms.DockStyle.Top;
			this.tabsFamilyData.Location = new System.Drawing.Point(0, 157);
			this.tabsFamilyData.Name = "tabsFamilyData";
			this.tabsFamilyData.SelectedIndex = 0;
			this.tabsFamilyData.Size = new System.Drawing.Size(708, 320);
			this.tabsFamilyData.TabIndex = 1;
			// 
			// pageChilds
			// 
			this.pageChilds.Location = new System.Drawing.Point(4, 26);
			this.pageChilds.Name = "pageChilds";
			this.pageChilds.Size = new System.Drawing.Size(700, 290);
			this.pageChilds.TabIndex = 0;
			this.pageChilds.Text = "pageChilds";
			// 
			// pageEvents
			// 
			this.pageEvents.Location = new System.Drawing.Point(4, 26);
			this.pageEvents.Name = "pageEvents";
			this.pageEvents.Size = new System.Drawing.Size(700, 290);
			this.pageEvents.TabIndex = 1;
			this.pageEvents.Text = "pageEvents";
			// 
			// pageNotes
			// 
			this.pageNotes.Location = new System.Drawing.Point(4, 26);
			this.pageNotes.Name = "pageNotes";
			this.pageNotes.Size = new System.Drawing.Size(700, 290);
			this.pageNotes.TabIndex = 2;
			this.pageNotes.Text = "pageNotes";
			// 
			// pageMultimedia
			// 
			this.pageMultimedia.Location = new System.Drawing.Point(4, 26);
			this.pageMultimedia.Name = "pageMultimedia";
			this.pageMultimedia.Size = new System.Drawing.Size(700, 290);
			this.pageMultimedia.TabIndex = 3;
			this.pageMultimedia.Text = "pageMultimedia";
			// 
			// pageSources
			// 
			this.pageSources.Location = new System.Drawing.Point(4, 26);
			this.pageSources.Name = "pageSources";
			this.pageSources.Size = new System.Drawing.Size(700, 290);
			this.pageSources.TabIndex = 4;
			this.pageSources.Text = "pageSources";
			// 
			// FamilyEditDlg
			// 
			this.AcceptButton = this.btnAccept;
			this.AutoScaleDimensions = new System.Drawing.SizeF(120F, 120F);
			this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
			this.CancelButton = this.btnCancel;
			this.ClientSize = new System.Drawing.Size(708, 541);
			this.Controls.Add(this.tabsFamilyData);
			this.Controls.Add(this.GroupBox1);
			this.Controls.Add(this.btnAccept);
			this.Controls.Add(this.btnCancel);
			this.Controls.Add(this.lblRestriction);
			this.Controls.Add(this.cmbRestriction);
			this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
			this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
			this.MaximizeBox = false;
			this.MinimizeBox = false;
			this.Name = "FamilyEditDlg";
			this.ShowInTaskbar = false;
			this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
			this.Text = "FamilyEditDlg";
			this.GroupBox1.ResumeLayout(false);
			this.GroupBox1.PerformLayout();
			this.tabsFamilyData.ResumeLayout(false);
			this.ResumeLayout(false);
			this.PerformLayout();
		}
	}
}