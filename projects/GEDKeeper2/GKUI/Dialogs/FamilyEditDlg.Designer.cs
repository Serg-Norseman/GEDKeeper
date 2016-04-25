using System;

namespace GKUI.Dialogs
{
	partial class FamilyEditDlg
	{
		private System.Windows.Forms.TabControl PagesFamilyData;
		private System.Windows.Forms.TabPage SheetEvents;
		private System.Windows.Forms.TabPage SheetNotes;
		private System.Windows.Forms.TabPage SheetMultimedia;
		private System.Windows.Forms.TabPage SheetSources;
		private System.Windows.Forms.TabPage SheetChilds;
		private System.Windows.Forms.Button btnAccept;
		private System.Windows.Forms.Button btnCancel;
		private System.Windows.Forms.GroupBox GroupBox1;
		private System.Windows.Forms.Label Label1;
		private System.Windows.Forms.TextBox EditHusband;
		private System.Windows.Forms.Button btnHusbandAdd;
		private System.Windows.Forms.Button btnHusbandDelete;
		private System.Windows.Forms.Button btnHusbandSel;
		private System.Windows.Forms.Button btnWifeSel;
		private System.Windows.Forms.Button btnWifeDelete;
		private System.Windows.Forms.Button btnWifeAdd;
		private System.Windows.Forms.TextBox EditWife;
		private System.Windows.Forms.Label Label2;
		private System.Windows.Forms.Label Label6;
		private System.Windows.Forms.ComboBox edMarriageStatus;
		private System.Windows.Forms.Label Label5;
		private System.Windows.Forms.ComboBox cbRestriction;

		private void InitializeComponent()
		{
			this.btnAccept = new System.Windows.Forms.Button();
			this.btnCancel = new System.Windows.Forms.Button();
			this.GroupBox1 = new System.Windows.Forms.GroupBox();
			this.Label1 = new System.Windows.Forms.Label();
			this.btnHusbandAdd = new System.Windows.Forms.Button();
			this.btnHusbandDelete = new System.Windows.Forms.Button();
			this.btnHusbandSel = new System.Windows.Forms.Button();
			this.btnWifeSel = new System.Windows.Forms.Button();
			this.btnWifeDelete = new System.Windows.Forms.Button();
			this.btnWifeAdd = new System.Windows.Forms.Button();
			this.Label2 = new System.Windows.Forms.Label();
			this.Label6 = new System.Windows.Forms.Label();
			this.EditHusband = new System.Windows.Forms.TextBox();
			this.EditWife = new System.Windows.Forms.TextBox();
			this.edMarriageStatus = new System.Windows.Forms.ComboBox();
			this.Label5 = new System.Windows.Forms.Label();
			this.cbRestriction = new System.Windows.Forms.ComboBox();
			this.PagesFamilyData = new System.Windows.Forms.TabControl();
			this.SheetChilds = new System.Windows.Forms.TabPage();
			this.SheetEvents = new System.Windows.Forms.TabPage();
			this.SheetNotes = new System.Windows.Forms.TabPage();
			this.SheetMultimedia = new System.Windows.Forms.TabPage();
			this.SheetSources = new System.Windows.Forms.TabPage();
			this.GroupBox1.SuspendLayout();
			this.PagesFamilyData.SuspendLayout();
			this.SuspendLayout();
			// 
			// btnAccept
			// 
			this.btnAccept.Image = global::GKResources.iBtnAccept;
			this.btnAccept.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
			this.btnAccept.Location = new System.Drawing.Point(328, 408);
			this.btnAccept.Name = "btnAccept";
			this.btnAccept.Size = new System.Drawing.Size(81, 25);
			this.btnAccept.TabIndex = 4;
			this.btnAccept.Text = "Принять";
			this.btnAccept.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
			this.btnAccept.Click += new System.EventHandler(this.btnAccept_Click);
			// 
			// btnCancel
			// 
			this.btnCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
			this.btnCancel.Image = global::GKResources.iBtnCancel;
			this.btnCancel.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
			this.btnCancel.Location = new System.Drawing.Point(416, 408);
			this.btnCancel.Name = "btnCancel";
			this.btnCancel.Size = new System.Drawing.Size(81, 25);
			this.btnCancel.TabIndex = 5;
			this.btnCancel.Text = "Отменить";
			this.btnCancel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
			// 
			// GroupBox1
			// 
			this.GroupBox1.Controls.Add(this.Label1);
			this.GroupBox1.Controls.Add(this.btnHusbandAdd);
			this.GroupBox1.Controls.Add(this.btnHusbandDelete);
			this.GroupBox1.Controls.Add(this.btnHusbandSel);
			this.GroupBox1.Controls.Add(this.btnWifeSel);
			this.GroupBox1.Controls.Add(this.btnWifeDelete);
			this.GroupBox1.Controls.Add(this.btnWifeAdd);
			this.GroupBox1.Controls.Add(this.Label2);
			this.GroupBox1.Controls.Add(this.Label6);
			this.GroupBox1.Controls.Add(this.EditHusband);
			this.GroupBox1.Controls.Add(this.EditWife);
			this.GroupBox1.Controls.Add(this.edMarriageStatus);
			this.GroupBox1.Dock = System.Windows.Forms.DockStyle.Top;
			this.GroupBox1.Location = new System.Drawing.Point(0, 0);
			this.GroupBox1.Name = "GroupBox1";
			this.GroupBox1.Size = new System.Drawing.Size(505, 129);
			this.GroupBox1.TabIndex = 0;
			this.GroupBox1.TabStop = false;
			this.GroupBox1.Text = "Семья";
			// 
			// Label1
			// 
			this.Label1.Location = new System.Drawing.Point(16, 32);
			this.Label1.Name = "Label1";
			this.Label1.Size = new System.Drawing.Size(35, 13);
			this.Label1.TabIndex = 0;
			this.Label1.Text = "Муж";
			// 
			// btnHusbandAdd
			// 
			this.btnHusbandAdd.AccessibleDescription = "Выбрать или добавить мужа";
			this.btnHusbandAdd.Enabled = false;
			this.btnHusbandAdd.Image = global::GKResources.iRecNew;
			this.btnHusbandAdd.Location = new System.Drawing.Point(400, 21);
			this.btnHusbandAdd.Name = "btnHusbandAdd";
			this.btnHusbandAdd.Size = new System.Drawing.Size(28, 28);
			this.btnHusbandAdd.TabIndex = 2;
			this.btnHusbandAdd.Click += new System.EventHandler(this.btnHusbandAddClick);
			// 
			// btnHusbandDelete
			// 
			this.btnHusbandDelete.AccessibleDescription = "Отсоединить мужа";
			this.btnHusbandDelete.Enabled = false;
			this.btnHusbandDelete.Image = global::GKResources.iRecDelete;
			this.btnHusbandDelete.Location = new System.Drawing.Point(429, 21);
			this.btnHusbandDelete.Name = "btnHusbandDelete";
			this.btnHusbandDelete.Size = new System.Drawing.Size(28, 28);
			this.btnHusbandDelete.TabIndex = 3;
			this.btnHusbandDelete.Click += new System.EventHandler(this.btnHusbandDeleteClick);
			// 
			// btnHusbandSel
			// 
			this.btnHusbandSel.AccessibleDescription = "Перейти на запись мужа";
			this.btnHusbandSel.Image = global::GKResources.iToMan;
			this.btnHusbandSel.Location = new System.Drawing.Point(458, 21);
			this.btnHusbandSel.Name = "btnHusbandSel";
			this.btnHusbandSel.Size = new System.Drawing.Size(28, 28);
			this.btnHusbandSel.TabIndex = 4;
			this.btnHusbandSel.Click += new System.EventHandler(this.btnHusbandSelClick);
			// 
			// btnWifeSel
			// 
			this.btnWifeSel.AccessibleDescription = "Перейти на запись жены";
			this.btnWifeSel.Image = global::GKResources.iToMan;
			this.btnWifeSel.Location = new System.Drawing.Point(458, 53);
			this.btnWifeSel.Name = "btnWifeSel";
			this.btnWifeSel.Size = new System.Drawing.Size(28, 28);
			this.btnWifeSel.TabIndex = 9;
			this.btnWifeSel.Click += new System.EventHandler(this.btnWifeSelClick);
			// 
			// btnWifeDelete
			// 
			this.btnWifeDelete.AccessibleDescription = "Отсоединить жену";
			this.btnWifeDelete.Enabled = false;
			this.btnWifeDelete.Image = global::GKResources.iRecDelete;
			this.btnWifeDelete.Location = new System.Drawing.Point(429, 53);
			this.btnWifeDelete.Name = "btnWifeDelete";
			this.btnWifeDelete.Size = new System.Drawing.Size(28, 28);
			this.btnWifeDelete.TabIndex = 8;
			this.btnWifeDelete.Click += new System.EventHandler(this.btnWifeDeleteClick);
			// 
			// btnWifeAdd
			// 
			this.btnWifeAdd.AccessibleDescription = "Выбрать или добавить жену";
			this.btnWifeAdd.Enabled = false;
			this.btnWifeAdd.Image = global::GKResources.iRecNew;
			this.btnWifeAdd.Location = new System.Drawing.Point(400, 53);
			this.btnWifeAdd.Name = "btnWifeAdd";
			this.btnWifeAdd.Size = new System.Drawing.Size(28, 28);
			this.btnWifeAdd.TabIndex = 7;
			this.btnWifeAdd.Click += new System.EventHandler(this.btnWifeAddClick);
			// 
			// Label2
			// 
			this.Label2.Location = new System.Drawing.Point(16, 64);
			this.Label2.Name = "Label2";
			this.Label2.Size = new System.Drawing.Size(35, 13);
			this.Label2.TabIndex = 5;
			this.Label2.Text = "Жена";
			// 
			// Label6
			// 
			this.Label6.Location = new System.Drawing.Point(16, 96);
			this.Label6.Name = "Label6";
			this.Label6.Size = new System.Drawing.Size(45, 13);
			this.Label6.TabIndex = 10;
			this.Label6.Text = "Статус";
			// 
			// EditHusband
			// 
			this.EditHusband.ForeColor = System.Drawing.SystemColors.Control;
			this.EditHusband.Location = new System.Drawing.Point(64, 24);
			this.EditHusband.Name = "EditHusband";
			this.EditHusband.ReadOnly = true;
			this.EditHusband.Size = new System.Drawing.Size(329, 21);
			this.EditHusband.TabIndex = 1;
			this.EditHusband.TextChanged += new System.EventHandler(this.EditHusband_TextChanged);
			// 
			// EditWife
			// 
			this.EditWife.ForeColor = System.Drawing.SystemColors.Control;
			this.EditWife.Location = new System.Drawing.Point(64, 56);
			this.EditWife.Name = "EditWife";
			this.EditWife.ReadOnly = true;
			this.EditWife.Size = new System.Drawing.Size(329, 21);
			this.EditWife.TabIndex = 6;
			this.EditWife.TextChanged += new System.EventHandler(this.EditWife_TextChanged);
			// 
			// EditMarriageStatus
			// 
			this.edMarriageStatus.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
			this.edMarriageStatus.Location = new System.Drawing.Point(64, 88);
			this.edMarriageStatus.Name = "EditMarriageStatus";
			this.edMarriageStatus.Size = new System.Drawing.Size(145, 21);
			this.edMarriageStatus.TabIndex = 11;
			// 
			// Label5
			// 
			this.Label5.Location = new System.Drawing.Point(8, 416);
			this.Label5.Name = "Label5";
			this.Label5.Size = new System.Drawing.Size(150, 13);
			this.Label5.TabIndex = 2;
			this.Label5.Text = "Ограничение безопасности";
			// 
			// cbRestriction
			// 
			this.cbRestriction.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
			this.cbRestriction.Location = new System.Drawing.Point(160, 408);
			this.cbRestriction.Name = "cbRestriction";
			this.cbRestriction.Size = new System.Drawing.Size(145, 21);
			this.cbRestriction.TabIndex = 3;
			this.cbRestriction.SelectedIndexChanged += new System.EventHandler(this.cbRestriction_SelectedIndexChanged);
			// 
			// PagesFamilyData
			// 
			this.PagesFamilyData.Controls.Add(this.SheetChilds);
			this.PagesFamilyData.Controls.Add(this.SheetEvents);
			this.PagesFamilyData.Controls.Add(this.SheetNotes);
			this.PagesFamilyData.Controls.Add(this.SheetMultimedia);
			this.PagesFamilyData.Controls.Add(this.SheetSources);
			this.PagesFamilyData.Dock = System.Windows.Forms.DockStyle.Top;
			this.PagesFamilyData.Location = new System.Drawing.Point(0, 129);
			this.PagesFamilyData.Name = "PagesFamilyData";
			this.PagesFamilyData.SelectedIndex = 0;
			this.PagesFamilyData.Size = new System.Drawing.Size(505, 264);
			this.PagesFamilyData.TabIndex = 1;
			// 
			// SheetChilds
			// 
			this.SheetChilds.Location = new System.Drawing.Point(4, 22);
			this.SheetChilds.Name = "SheetChilds";
			this.SheetChilds.Size = new System.Drawing.Size(497, 238);
			this.SheetChilds.TabIndex = 0;
			this.SheetChilds.Text = "Дети";
			// 
			// SheetEvents
			// 
			this.SheetEvents.Location = new System.Drawing.Point(4, 22);
			this.SheetEvents.Name = "SheetEvents";
			this.SheetEvents.Size = new System.Drawing.Size(497, 238);
			this.SheetEvents.TabIndex = 1;
			this.SheetEvents.Text = "События";
			// 
			// SheetNotes
			// 
			this.SheetNotes.Location = new System.Drawing.Point(4, 22);
			this.SheetNotes.Name = "SheetNotes";
			this.SheetNotes.Size = new System.Drawing.Size(497, 238);
			this.SheetNotes.TabIndex = 2;
			this.SheetNotes.Text = "Заметки";
			// 
			// SheetMultimedia
			// 
			this.SheetMultimedia.Location = new System.Drawing.Point(4, 22);
			this.SheetMultimedia.Name = "SheetMultimedia";
			this.SheetMultimedia.Size = new System.Drawing.Size(497, 238);
			this.SheetMultimedia.TabIndex = 3;
			this.SheetMultimedia.Text = "Мультимедиа";
			// 
			// SheetSources
			// 
			this.SheetSources.Location = new System.Drawing.Point(4, 22);
			this.SheetSources.Name = "SheetSources";
			this.SheetSources.Size = new System.Drawing.Size(497, 238);
			this.SheetSources.TabIndex = 4;
			this.SheetSources.Text = "Источники";
			// 
			// TfmFamilyEdit
			// 
			this.AcceptButton = this.btnAccept;
			this.AutoScaleBaseSize = new System.Drawing.Size(5, 14);
			this.CancelButton = this.btnCancel;
			this.ClientSize = new System.Drawing.Size(505, 441);
			this.Controls.Add(this.PagesFamilyData);
			this.Controls.Add(this.GroupBox1);
			this.Controls.Add(this.btnAccept);
			this.Controls.Add(this.btnCancel);
			this.Controls.Add(this.Label5);
			this.Controls.Add(this.cbRestriction);
			this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
			this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
			this.MaximizeBox = false;
			this.MinimizeBox = false;
			this.Name = "TfmFamilyEdit";
			this.ShowInTaskbar = false;
			this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
			this.Text = "Редактирование семьи";
			this.GroupBox1.ResumeLayout(false);
			this.GroupBox1.PerformLayout();
			this.PagesFamilyData.ResumeLayout(false);
			this.ResumeLayout(false);
		}
	}
}