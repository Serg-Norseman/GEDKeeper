using System;

namespace GKUI.Dialogs
{
	partial class ResearchEditDlg
	{
		private System.Windows.Forms.GroupBox GroupBox1;
		private System.Windows.Forms.TextBox EditName;
		private System.Windows.Forms.Label Label1;
		private System.Windows.Forms.TabControl PagesGroupData;
		private System.Windows.Forms.TabPage SheetNotes;
		private System.Windows.Forms.TabPage SheetTasks;
		private System.Windows.Forms.Button btnAccept;
		private System.Windows.Forms.Button btnCancel;
		private System.Windows.Forms.Label Label2;
		private System.Windows.Forms.ComboBox EditPriority;
		private System.Windows.Forms.TabPage SheetCommunications;
		private System.Windows.Forms.Label Label3;
		private System.Windows.Forms.ComboBox EditStatus;
		private System.Windows.Forms.Label Label4;
		private System.Windows.Forms.MaskedTextBox EditStartDate;
		private System.Windows.Forms.Label Label5;
		private System.Windows.Forms.MaskedTextBox EditStopDate;
		private System.Windows.Forms.Label Label6;
		private System.Windows.Forms.NumericUpDown EditPercent;
		private System.Windows.Forms.TabPage SheetGroups;

		private void InitializeComponent()
		{
			this.GroupBox1 = new System.Windows.Forms.GroupBox();
			this.Label1 = new System.Windows.Forms.Label();
			this.Label2 = new System.Windows.Forms.Label();
			this.Label3 = new System.Windows.Forms.Label();
			this.Label4 = new System.Windows.Forms.Label();
			this.Label5 = new System.Windows.Forms.Label();
			this.Label6 = new System.Windows.Forms.Label();
			this.EditName = new System.Windows.Forms.TextBox();
			this.EditPriority = new System.Windows.Forms.ComboBox();
			this.EditStatus = new System.Windows.Forms.ComboBox();
			this.EditStartDate = new System.Windows.Forms.MaskedTextBox();
			this.EditStopDate = new System.Windows.Forms.MaskedTextBox();
			this.EditPercent = new System.Windows.Forms.NumericUpDown();
			this.btnAccept = new System.Windows.Forms.Button();
			this.btnCancel = new System.Windows.Forms.Button();
			this.PagesGroupData = new System.Windows.Forms.TabControl();
			this.SheetTasks = new System.Windows.Forms.TabPage();
			this.SheetCommunications = new System.Windows.Forms.TabPage();
			this.SheetGroups = new System.Windows.Forms.TabPage();
			this.SheetNotes = new System.Windows.Forms.TabPage();
			this.GroupBox1.SuspendLayout();
			((System.ComponentModel.ISupportInitialize)(this.EditPercent)).BeginInit();
			this.PagesGroupData.SuspendLayout();
			this.SuspendLayout();
			// 
			// GroupBox1
			// 
			this.GroupBox1.Controls.Add(this.Label1);
			this.GroupBox1.Controls.Add(this.Label2);
			this.GroupBox1.Controls.Add(this.Label3);
			this.GroupBox1.Controls.Add(this.Label4);
			this.GroupBox1.Controls.Add(this.Label5);
			this.GroupBox1.Controls.Add(this.Label6);
			this.GroupBox1.Controls.Add(this.EditName);
			this.GroupBox1.Controls.Add(this.EditPriority);
			this.GroupBox1.Controls.Add(this.EditStatus);
			this.GroupBox1.Controls.Add(this.EditStartDate);
			this.GroupBox1.Controls.Add(this.EditStopDate);
			this.GroupBox1.Controls.Add(this.EditPercent);
			this.GroupBox1.Dock = System.Windows.Forms.DockStyle.Top;
			this.GroupBox1.Location = new System.Drawing.Point(0, 0);
			this.GroupBox1.Name = "GroupBox1";
			this.GroupBox1.Size = new System.Drawing.Size(852, 118);
			this.GroupBox1.TabIndex = 0;
			this.GroupBox1.TabStop = false;
			// 
			// Label1
			// 
			this.Label1.AutoSize = true;
			this.Label1.Location = new System.Drawing.Point(11, 22);
			this.Label1.Name = "Label1";
			this.Label1.Size = new System.Drawing.Size(67, 17);
			this.Label1.TabIndex = 0;
			this.Label1.Text = "Название";
			// 
			// Label2
			// 
			this.Label2.AutoSize = true;
			this.Label2.Location = new System.Drawing.Point(11, 51);
			this.Label2.Name = "Label2";
			this.Label2.Size = new System.Drawing.Size(80, 17);
			this.Label2.TabIndex = 2;
			this.Label2.Text = "Приоритет";
			// 
			// Label3
			// 
			this.Label3.AutoSize = true;
			this.Label3.Location = new System.Drawing.Point(347, 52);
			this.Label3.Name = "Label3";
			this.Label3.Size = new System.Drawing.Size(78, 17);
			this.Label3.TabIndex = 4;
			this.Label3.Text = "Состояние";
			// 
			// Label4
			// 
			this.Label4.AutoSize = true;
			this.Label4.Location = new System.Drawing.Point(11, 81);
			this.Label4.Name = "Label4";
			this.Label4.Size = new System.Drawing.Size(72, 17);
			this.Label4.TabIndex = 8;
			this.Label4.Text = "Запущено";
			// 
			// Label5
			// 
			this.Label5.AutoSize = true;
			this.Label5.Location = new System.Drawing.Point(347, 81);
			this.Label5.Name = "Label5";
			this.Label5.Size = new System.Drawing.Size(77, 17);
			this.Label5.TabIndex = 10;
			this.Label5.Text = "Завершено";
			// 
			// Label6
			// 
			this.Label6.AutoSize = true;
			this.Label6.Location = new System.Drawing.Point(683, 52);
			this.Label6.Name = "Label6";
			this.Label6.Size = new System.Drawing.Size(64, 17);
			this.Label6.TabIndex = 6;
			this.Label6.Text = "Процент";
			// 
			// EditName
			// 
			this.EditName.Location = new System.Drawing.Point(101, 19);
			this.EditName.Name = "EditName";
			this.EditName.Size = new System.Drawing.Size(740, 24);
			this.EditName.TabIndex = 1;
			// 
			// EditPriority
			// 
			this.EditPriority.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
			this.EditPriority.Location = new System.Drawing.Point(101, 49);
			this.EditPriority.Name = "EditPriority";
			this.EditPriority.Size = new System.Drawing.Size(225, 25);
			this.EditPriority.TabIndex = 3;
			// 
			// EditStatus
			// 
			this.EditStatus.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
			this.EditStatus.Location = new System.Drawing.Point(437, 49);
			this.EditStatus.Name = "EditStatus";
			this.EditStatus.Size = new System.Drawing.Size(225, 25);
			this.EditStatus.TabIndex = 5;
			// 
			// EditStartDate
			// 
			this.EditStartDate.Location = new System.Drawing.Point(101, 78);
			this.EditStartDate.Mask = "00/00/0000";
			this.EditStartDate.Name = "EditStartDate";
			this.EditStartDate.Size = new System.Drawing.Size(225, 24);
			this.EditStartDate.TabIndex = 9;
			this.EditStartDate.TextMaskFormat = System.Windows.Forms.MaskFormat.IncludePromptAndLiterals;
			// 
			// EditStopDate
			// 
			this.EditStopDate.Location = new System.Drawing.Point(437, 78);
			this.EditStopDate.Mask = "00/00/0000";
			this.EditStopDate.Name = "EditStopDate";
			this.EditStopDate.Size = new System.Drawing.Size(225, 24);
			this.EditStopDate.TabIndex = 11;
			this.EditStopDate.TextMaskFormat = System.Windows.Forms.MaskFormat.IncludePromptAndLiterals;
			// 
			// EditPercent
			// 
			this.EditPercent.Increment = new decimal(new int[] {
									5,
									0,
									0,
									0});
			this.EditPercent.Location = new System.Drawing.Point(762, 49);
			this.EditPercent.Name = "EditPercent";
			this.EditPercent.Size = new System.Drawing.Size(57, 24);
			this.EditPercent.TabIndex = 7;
			// 
			// btnAccept
			// 
			this.btnAccept.Image = global::GKResources.iBtnAccept;
			this.btnAccept.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
			this.btnAccept.Location = new System.Drawing.Point(605, 515);
			this.btnAccept.Name = "btnAccept";
			this.btnAccept.Size = new System.Drawing.Size(113, 30);
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
			this.btnCancel.Location = new System.Drawing.Point(728, 515);
			this.btnCancel.Name = "btnCancel";
			this.btnCancel.Size = new System.Drawing.Size(113, 30);
			this.btnCancel.TabIndex = 3;
			this.btnCancel.Text = "Отменить";
			this.btnCancel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
			// 
			// PagesGroupData
			// 
			this.PagesGroupData.Controls.Add(this.SheetTasks);
			this.PagesGroupData.Controls.Add(this.SheetCommunications);
			this.PagesGroupData.Controls.Add(this.SheetGroups);
			this.PagesGroupData.Controls.Add(this.SheetNotes);
			this.PagesGroupData.Dock = System.Windows.Forms.DockStyle.Top;
			this.PagesGroupData.Location = new System.Drawing.Point(0, 118);
			this.PagesGroupData.Name = "PagesGroupData";
			this.PagesGroupData.SelectedIndex = 0;
			this.PagesGroupData.Size = new System.Drawing.Size(852, 379);
			this.PagesGroupData.TabIndex = 1;
			// 
			// SheetTasks
			// 
			this.SheetTasks.Location = new System.Drawing.Point(4, 26);
			this.SheetTasks.Name = "SheetTasks";
			this.SheetTasks.Size = new System.Drawing.Size(844, 349);
			this.SheetTasks.TabIndex = 0;
			this.SheetTasks.Text = "Задачи";
			// 
			// SheetCommunications
			// 
			this.SheetCommunications.Location = new System.Drawing.Point(4, 26);
			this.SheetCommunications.Name = "SheetCommunications";
			this.SheetCommunications.Size = new System.Drawing.Size(844, 349);
			this.SheetCommunications.TabIndex = 1;
			this.SheetCommunications.Text = "Коммуникации";
			// 
			// SheetGroups
			// 
			this.SheetGroups.Location = new System.Drawing.Point(4, 26);
			this.SheetGroups.Name = "SheetGroups";
			this.SheetGroups.Size = new System.Drawing.Size(844, 349);
			this.SheetGroups.TabIndex = 2;
			this.SheetGroups.Text = "Группы";
			// 
			// SheetNotes
			// 
			this.SheetNotes.Location = new System.Drawing.Point(4, 26);
			this.SheetNotes.Name = "SheetNotes";
			this.SheetNotes.Size = new System.Drawing.Size(844, 349);
			this.SheetNotes.TabIndex = 3;
			this.SheetNotes.Text = "Заметки";
			// 
			// ResearchEditDlg
			// 
			this.AcceptButton = this.btnAccept;
			this.AutoScaleDimensions = new System.Drawing.SizeF(120F, 120F);
			this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
			this.CancelButton = this.btnCancel;
			this.ClientSize = new System.Drawing.Size(852, 557);
			this.Controls.Add(this.PagesGroupData);
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
			this.Text = "Редактирование исследования";
			this.GroupBox1.ResumeLayout(false);
			this.GroupBox1.PerformLayout();
			((System.ComponentModel.ISupportInitialize)(this.EditPercent)).EndInit();
			this.PagesGroupData.ResumeLayout(false);
			this.ResumeLayout(false);
		}
	}
}