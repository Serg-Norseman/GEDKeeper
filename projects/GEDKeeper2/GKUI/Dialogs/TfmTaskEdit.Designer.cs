using System;

namespace GKUI.Dialogs
{
	partial class TfmTaskEdit
	{
		private System.Windows.Forms.GroupBox GroupBox1;
		private System.Windows.Forms.TabControl PagesGroupData;
		private System.Windows.Forms.TabPage SheetNotes;
		private System.Windows.Forms.Button btnAccept;
		private System.Windows.Forms.Button btnCancel;
		private System.Windows.Forms.Label Label2;
		private System.Windows.Forms.ComboBox EditPriority;
		private System.Windows.Forms.Label Label4;
		private System.Windows.Forms.MaskedTextBox EditStartDate;
		private System.Windows.Forms.MaskedTextBox EditStopDate;
		private System.Windows.Forms.Label Label5;
		private System.Windows.Forms.Label Label1;
		private System.Windows.Forms.ComboBox cbGoalType;
		private System.Windows.Forms.TextBox EditGoal;
		private System.Windows.Forms.Button btnGoalSelect;

		private void InitializeComponent()
		{
			this.GroupBox1 = new System.Windows.Forms.GroupBox();
			this.Label2 = new System.Windows.Forms.Label();
			this.Label4 = new System.Windows.Forms.Label();
			this.Label5 = new System.Windows.Forms.Label();
			this.Label1 = new System.Windows.Forms.Label();
			this.btnGoalSelect = new System.Windows.Forms.Button();
			this.EditPriority = new System.Windows.Forms.ComboBox();
			this.EditStartDate = new System.Windows.Forms.MaskedTextBox();
			this.EditStopDate = new System.Windows.Forms.MaskedTextBox();
			this.cbGoalType = new System.Windows.Forms.ComboBox();
			this.EditGoal = new System.Windows.Forms.TextBox();
			this.btnAccept = new System.Windows.Forms.Button();
			this.btnCancel = new System.Windows.Forms.Button();
			this.PagesGroupData = new System.Windows.Forms.TabControl();
			this.SheetNotes = new System.Windows.Forms.TabPage();
			this.GroupBox1.SuspendLayout();
			this.PagesGroupData.SuspendLayout();
			this.SuspendLayout();
			// 
			// GroupBox1
			// 
			this.GroupBox1.Controls.Add(this.Label2);
			this.GroupBox1.Controls.Add(this.Label4);
			this.GroupBox1.Controls.Add(this.Label5);
			this.GroupBox1.Controls.Add(this.Label1);
			this.GroupBox1.Controls.Add(this.btnGoalSelect);
			this.GroupBox1.Controls.Add(this.EditPriority);
			this.GroupBox1.Controls.Add(this.EditStartDate);
			this.GroupBox1.Controls.Add(this.EditStopDate);
			this.GroupBox1.Controls.Add(this.cbGoalType);
			this.GroupBox1.Controls.Add(this.EditGoal);
			this.GroupBox1.Dock = System.Windows.Forms.DockStyle.Top;
			this.GroupBox1.Location = new System.Drawing.Point(0, 0);
			this.GroupBox1.Name = "GroupBox1";
			this.GroupBox1.Size = new System.Drawing.Size(481, 97);
			this.GroupBox1.TabIndex = 0;
			this.GroupBox1.TabStop = false;
			// 
			// Label2
			// 
			this.Label2.Location = new System.Drawing.Point(8, 48);
			this.Label2.Name = "Label2";
			this.Label2.Size = new System.Drawing.Size(62, 13);
			this.Label2.TabIndex = 4;
			this.Label2.Text = "Приоритет";
			// 
			// Label4
			// 
			this.Label4.Location = new System.Drawing.Point(8, 72);
			this.Label4.Name = "Label4";
			this.Label4.Size = new System.Drawing.Size(60, 13);
			this.Label4.TabIndex = 6;
			this.Label4.Text = "Запущено";
			// 
			// Label5
			// 
			this.Label5.Location = new System.Drawing.Point(248, 72);
			this.Label5.Name = "Label5";
			this.Label5.Size = new System.Drawing.Size(64, 13);
			this.Label5.TabIndex = 8;
			this.Label5.Text = "Завершено";
			// 
			// Label1
			// 
			this.Label1.Location = new System.Drawing.Point(8, 24);
			this.Label1.Name = "Label1";
			this.Label1.Size = new System.Drawing.Size(40, 13);
			this.Label1.TabIndex = 0;
			this.Label1.Text = "Цель";
			// 
			// btnGoalSelect
			// 
			this.btnGoalSelect.AccessibleDescription = "Выбрать запись цели";
			this.btnGoalSelect.Image = global::GKResources.iRecNew;
			this.btnGoalSelect.Location = new System.Drawing.Point(448, 13);
			this.btnGoalSelect.Name = "btnGoalSelect";
			this.btnGoalSelect.Size = new System.Drawing.Size(28, 28);
			this.btnGoalSelect.TabIndex = 3;
			this.btnGoalSelect.Click += new System.EventHandler(this.btnGoalSelect_Click);
			// 
			// EditPriority
			// 
			this.EditPriority.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
			this.EditPriority.Location = new System.Drawing.Point(72, 40);
			this.EditPriority.Name = "EditPriority";
			this.EditPriority.Size = new System.Drawing.Size(161, 21);
			this.EditPriority.TabIndex = 5;
			// 
			// EditStartDate
			// 
			this.EditStartDate.Location = new System.Drawing.Point(72, 64);
			this.EditStartDate.Mask = "00/00/0000";
			this.EditStartDate.Name = "EditStartDate";
			this.EditStartDate.Size = new System.Drawing.Size(161, 21);
			this.EditStartDate.TabIndex = 7;
			this.EditStartDate.TextMaskFormat = System.Windows.Forms.MaskFormat.IncludePromptAndLiterals;
			// 
			// EditStopDate
			// 
			this.EditStopDate.Location = new System.Drawing.Point(312, 64);
			this.EditStopDate.Mask = "00/00/0000";
			this.EditStopDate.Name = "EditStopDate";
			this.EditStopDate.Size = new System.Drawing.Size(161, 21);
			this.EditStopDate.TabIndex = 9;
			this.EditStopDate.TextMaskFormat = System.Windows.Forms.MaskFormat.IncludePromptAndLiterals;
			// 
			// cbGoalType
			// 
			this.cbGoalType.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
			this.cbGoalType.Location = new System.Drawing.Point(72, 16);
			this.cbGoalType.Name = "cbGoalType";
			this.cbGoalType.Size = new System.Drawing.Size(113, 21);
			this.cbGoalType.TabIndex = 1;
			this.cbGoalType.SelectedIndexChanged += new System.EventHandler(this.cbGoalType_SelectedIndexChanged);
			// 
			// EditGoal
			// 
			this.EditGoal.Location = new System.Drawing.Point(192, 16);
			this.EditGoal.Name = "EditGoal";
			this.EditGoal.ReadOnly = true;
			this.EditGoal.Size = new System.Drawing.Size(249, 21);
			this.EditGoal.TabIndex = 2;
			// 
			// btnAccept
			// 
			this.btnAccept.Image = global::GKResources.iBtnAccept;
			this.btnAccept.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
			this.btnAccept.Location = new System.Drawing.Point(304, 368);
			this.btnAccept.Name = "btnAccept";
			this.btnAccept.Size = new System.Drawing.Size(81, 25);
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
			this.btnCancel.Location = new System.Drawing.Point(392, 368);
			this.btnCancel.Name = "btnCancel";
			this.btnCancel.Size = new System.Drawing.Size(81, 25);
			this.btnCancel.TabIndex = 3;
			this.btnCancel.Text = "Отменить";
			this.btnCancel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
			// 
			// PagesGroupData
			// 
			this.PagesGroupData.Controls.Add(this.SheetNotes);
			this.PagesGroupData.Dock = System.Windows.Forms.DockStyle.Top;
			this.PagesGroupData.Location = new System.Drawing.Point(0, 97);
			this.PagesGroupData.Name = "PagesGroupData";
			this.PagesGroupData.SelectedIndex = 0;
			this.PagesGroupData.Size = new System.Drawing.Size(481, 256);
			this.PagesGroupData.TabIndex = 1;
			// 
			// SheetNotes
			// 
			this.SheetNotes.Location = new System.Drawing.Point(4, 22);
			this.SheetNotes.Name = "SheetNotes";
			this.SheetNotes.Size = new System.Drawing.Size(473, 230);
			this.SheetNotes.TabIndex = 0;
			this.SheetNotes.Text = "Заметки";
			// 
			// TfmTaskEdit
			// 
			this.AcceptButton = this.btnAccept;
			this.AutoScaleBaseSize = new System.Drawing.Size(5, 14);
			this.CancelButton = this.btnCancel;
			this.ClientSize = new System.Drawing.Size(481, 402);
			this.Controls.Add(this.PagesGroupData);
			this.Controls.Add(this.GroupBox1);
			this.Controls.Add(this.btnAccept);
			this.Controls.Add(this.btnCancel);
			this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
			this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
			this.MaximizeBox = false;
			this.MinimizeBox = false;
			this.Name = "TfmTaskEdit";
			this.ShowInTaskbar = false;
			this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
			this.Text = "Редактирование задачи";
			this.GroupBox1.ResumeLayout(false);
			this.GroupBox1.PerformLayout();
			this.PagesGroupData.ResumeLayout(false);
			this.ResumeLayout(false);
		}
	}
}