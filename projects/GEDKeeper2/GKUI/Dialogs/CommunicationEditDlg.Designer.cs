using System;

namespace GKUI.Dialogs
{
	partial class CommunicationEditDlg
	{
		private System.Windows.Forms.GroupBox GroupBox1;
		private System.Windows.Forms.TabControl PagesGroupData;
		private System.Windows.Forms.TabPage SheetNotes;
		private System.Windows.Forms.TabPage SheetMultimedia;
		private System.Windows.Forms.Button btnAccept;
		private System.Windows.Forms.Button btnCancel;
		private System.Windows.Forms.Label Label1;
		private System.Windows.Forms.TextBox EditName;
		private System.Windows.Forms.Label Label4;
		private System.Windows.Forms.MaskedTextBox EditDate;
		private System.Windows.Forms.Label Label2;
		private System.Windows.Forms.ComboBox EditCorrType;
		private System.Windows.Forms.ComboBox EditDir;
		private System.Windows.Forms.Label Label5;
		private System.Windows.Forms.TextBox EditCorresponder;
		private System.Windows.Forms.Button btnPersonAdd;

		private void InitializeComponent()
		{
			this.GroupBox1 = new System.Windows.Forms.GroupBox();
			this.Label1 = new System.Windows.Forms.Label();
			this.Label4 = new System.Windows.Forms.Label();
			this.Label2 = new System.Windows.Forms.Label();
			this.Label5 = new System.Windows.Forms.Label();
			this.btnPersonAdd = new System.Windows.Forms.Button();
			this.EditName = new System.Windows.Forms.TextBox();
			this.EditDate = new System.Windows.Forms.MaskedTextBox();
			this.EditCorrType = new System.Windows.Forms.ComboBox();
			this.EditDir = new System.Windows.Forms.ComboBox();
			this.EditCorresponder = new System.Windows.Forms.TextBox();
			this.btnAccept = new System.Windows.Forms.Button();
			this.btnCancel = new System.Windows.Forms.Button();
			this.PagesGroupData = new System.Windows.Forms.TabControl();
			this.SheetNotes = new System.Windows.Forms.TabPage();
			this.SheetMultimedia = new System.Windows.Forms.TabPage();
			this.GroupBox1.SuspendLayout();
			this.PagesGroupData.SuspendLayout();
			this.SuspendLayout();
			// 
			// GroupBox1
			// 
			this.GroupBox1.Controls.Add(this.Label1);
			this.GroupBox1.Controls.Add(this.Label4);
			this.GroupBox1.Controls.Add(this.Label2);
			this.GroupBox1.Controls.Add(this.Label5);
			this.GroupBox1.Controls.Add(this.btnPersonAdd);
			this.GroupBox1.Controls.Add(this.EditName);
			this.GroupBox1.Controls.Add(this.EditDate);
			this.GroupBox1.Controls.Add(this.EditCorrType);
			this.GroupBox1.Controls.Add(this.EditDir);
			this.GroupBox1.Controls.Add(this.EditCorresponder);
			this.GroupBox1.Dock = System.Windows.Forms.DockStyle.Top;
			this.GroupBox1.Location = new System.Drawing.Point(0, 0);
			this.GroupBox1.Name = "GroupBox1";
			this.GroupBox1.Size = new System.Drawing.Size(675, 118);
			this.GroupBox1.TabIndex = 0;
			this.GroupBox1.TabStop = false;
			// 
			// Label1
			// 
			this.Label1.AutoSize = true;
			this.Label1.Location = new System.Drawing.Point(11, 22);
			this.Label1.Name = "Label1";
			this.Label1.Size = new System.Drawing.Size(39, 17);
			this.Label1.TabIndex = 0;
			this.Label1.Text = "Тема";
			// 
			// Label4
			// 
			this.Label4.AutoSize = true;
			this.Label4.Location = new System.Drawing.Point(322, 81);
			this.Label4.Name = "Label4";
			this.Label4.Size = new System.Drawing.Size(40, 17);
			this.Label4.TabIndex = 8;
			this.Label4.Text = "Дата";
			// 
			// Label2
			// 
			this.Label2.AutoSize = true;
			this.Label2.Location = new System.Drawing.Point(11, 81);
			this.Label2.Name = "Label2";
			this.Label2.Size = new System.Drawing.Size(32, 17);
			this.Label2.TabIndex = 6;
			this.Label2.Text = "Тип";
			// 
			// Label5
			// 
			this.Label5.AutoSize = true;
			this.Label5.Location = new System.Drawing.Point(11, 53);
			this.Label5.Name = "Label5";
			this.Label5.Size = new System.Drawing.Size(109, 17);
			this.Label5.TabIndex = 2;
			this.Label5.Text = "Корреспондент";
			// 
			// btnPersonAdd
			// 
			this.btnPersonAdd.AccessibleDescription = "Выбрать персональную запись";
			this.btnPersonAdd.Image = global::GKResources.iRecNew;
			this.btnPersonAdd.Location = new System.Drawing.Point(627, 45);
			this.btnPersonAdd.Name = "btnPersonAdd";
			this.btnPersonAdd.Size = new System.Drawing.Size(37, 32);
			this.btnPersonAdd.TabIndex = 5;
			this.btnPersonAdd.Click += new System.EventHandler(this.btnPersonAdd_Click);
			// 
			// EditName
			// 
			this.EditName.Location = new System.Drawing.Point(134, 19);
			this.EditName.Name = "EditName";
			this.EditName.Size = new System.Drawing.Size(528, 24);
			this.EditName.TabIndex = 1;
			// 
			// EditDate
			// 
			this.EditDate.Location = new System.Drawing.Point(392, 78);
			this.EditDate.Mask = "00/00/0000";
			this.EditDate.Name = "EditDate";
			this.EditDate.Size = new System.Drawing.Size(225, 24);
			this.EditDate.TabIndex = 9;
			this.EditDate.TextMaskFormat = System.Windows.Forms.MaskFormat.IncludePromptAndLiterals;
			// 
			// EditCorrType
			// 
			this.EditCorrType.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
			this.EditCorrType.Location = new System.Drawing.Point(134, 78);
			this.EditCorrType.Name = "EditCorrType";
			this.EditCorrType.Size = new System.Drawing.Size(147, 25);
			this.EditCorrType.TabIndex = 7;
			// 
			// EditDir
			// 
			this.EditDir.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
			this.EditDir.Items.AddRange(new object[] {
									"от",
									"к"});
			this.EditDir.Location = new System.Drawing.Point(134, 49);
			this.EditDir.Name = "EditDir";
			this.EditDir.Size = new System.Drawing.Size(91, 25);
			this.EditDir.TabIndex = 3;
			// 
			// EditCorresponder
			// 
			this.EditCorresponder.ForeColor = System.Drawing.SystemColors.Control;
			this.EditCorresponder.Location = new System.Drawing.Point(235, 49);
			this.EditCorresponder.Name = "EditCorresponder";
			this.EditCorresponder.ReadOnly = true;
			this.EditCorresponder.Size = new System.Drawing.Size(382, 24);
			this.EditCorresponder.TabIndex = 4;
			// 
			// btnAccept
			// 
			this.btnAccept.Image = global::GKResources.iBtnAccept;
			this.btnAccept.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
			this.btnAccept.Location = new System.Drawing.Point(426, 466);
			this.btnAccept.Name = "btnAccept";
			this.btnAccept.Size = new System.Drawing.Size(113, 31);
			this.btnAccept.TabIndex = 2;
			this.btnAccept.Text = "Принять";
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
			this.btnCancel.Text = "Отменить";
			this.btnCancel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
			// 
			// PagesGroupData
			// 
			this.PagesGroupData.Controls.Add(this.SheetNotes);
			this.PagesGroupData.Controls.Add(this.SheetMultimedia);
			this.PagesGroupData.Dock = System.Windows.Forms.DockStyle.Top;
			this.PagesGroupData.Location = new System.Drawing.Point(0, 118);
			this.PagesGroupData.Name = "PagesGroupData";
			this.PagesGroupData.SelectedIndex = 0;
			this.PagesGroupData.Size = new System.Drawing.Size(675, 330);
			this.PagesGroupData.TabIndex = 1;
			// 
			// SheetNotes
			// 
			this.SheetNotes.Location = new System.Drawing.Point(4, 26);
			this.SheetNotes.Name = "SheetNotes";
			this.SheetNotes.Size = new System.Drawing.Size(667, 300);
			this.SheetNotes.TabIndex = 0;
			this.SheetNotes.Text = "Заметки";
			// 
			// SheetMultimedia
			// 
			this.SheetMultimedia.Location = new System.Drawing.Point(4, 26);
			this.SheetMultimedia.Name = "SheetMultimedia";
			this.SheetMultimedia.Size = new System.Drawing.Size(667, 300);
			this.SheetMultimedia.TabIndex = 1;
			this.SheetMultimedia.Text = "Мультимедиа";
			// 
			// CommunicationEditDlg
			// 
			this.AcceptButton = this.btnAccept;
			this.AutoScaleDimensions = new System.Drawing.SizeF(120F, 120F);
			this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
			this.CancelButton = this.btnCancel;
			this.ClientSize = new System.Drawing.Size(675, 513);
			this.Controls.Add(this.PagesGroupData);
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
			this.Text = "Редактирование коммуникации";
			this.GroupBox1.ResumeLayout(false);
			this.GroupBox1.PerformLayout();
			this.PagesGroupData.ResumeLayout(false);
			this.ResumeLayout(false);
		}
	}
}