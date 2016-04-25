using System;

namespace GKUI.Dialogs
{
	partial class SourceEditDlg
	{
		private System.Windows.Forms.Button btnAccept;
		private System.Windows.Forms.Button btnCancel;
		private System.Windows.Forms.TabControl PagesData;
		private System.Windows.Forms.TabPage SheetNotes;
		private System.Windows.Forms.TabPage SheetMultimedia;
		private System.Windows.Forms.TabPage SheetRepositories;
		private System.Windows.Forms.TabPage SheetText;
		private System.Windows.Forms.TextBox EditText;
		private System.Windows.Forms.TabPage SheetCommon;
		private System.Windows.Forms.Label Label1;
		private System.Windows.Forms.TextBox EditShortTitle;
		private System.Windows.Forms.Label Label3;
		private System.Windows.Forms.TextBox EditAuthor;
		private System.Windows.Forms.Label Label2;
		private System.Windows.Forms.TextBox EditTitle;
		private System.Windows.Forms.Label Label4;
		private System.Windows.Forms.TextBox EditPublication;

		private void InitializeComponent()
		{
			this.btnAccept = new System.Windows.Forms.Button();
			this.btnCancel = new System.Windows.Forms.Button();
			this.PagesData = new System.Windows.Forms.TabControl();
			this.SheetCommon = new System.Windows.Forms.TabPage();
			this.Label1 = new System.Windows.Forms.Label();
			this.Label3 = new System.Windows.Forms.Label();
			this.Label2 = new System.Windows.Forms.Label();
			this.Label4 = new System.Windows.Forms.Label();
			this.EditShortTitle = new System.Windows.Forms.TextBox();
			this.EditAuthor = new System.Windows.Forms.TextBox();
			this.EditTitle = new System.Windows.Forms.TextBox();
			this.EditPublication = new System.Windows.Forms.TextBox();
			this.SheetText = new System.Windows.Forms.TabPage();
			this.EditText = new System.Windows.Forms.TextBox();
			this.SheetRepositories = new System.Windows.Forms.TabPage();
			this.SheetNotes = new System.Windows.Forms.TabPage();
			this.SheetMultimedia = new System.Windows.Forms.TabPage();
			this.PagesData.SuspendLayout();
			this.SheetCommon.SuspendLayout();
			this.SheetText.SuspendLayout();
			this.SuspendLayout();
			// 
			// btnAccept
			// 
			this.btnAccept.Image = global::GKResources.iBtnAccept;
			this.btnAccept.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
			this.btnAccept.Location = new System.Drawing.Point(504, 505);
			this.btnAccept.Name = "btnAccept";
			this.btnAccept.Size = new System.Drawing.Size(113, 31);
			this.btnAccept.TabIndex = 1;
			this.btnAccept.Text = "Принять";
			this.btnAccept.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
			this.btnAccept.Click += new System.EventHandler(this.btnAccept_Click);
			// 
			// btnCancel
			// 
			this.btnCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
			this.btnCancel.Image = global::GKResources.iBtnCancel;
			this.btnCancel.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
			this.btnCancel.Location = new System.Drawing.Point(627, 505);
			this.btnCancel.Name = "btnCancel";
			this.btnCancel.Size = new System.Drawing.Size(114, 31);
			this.btnCancel.TabIndex = 2;
			this.btnCancel.Text = "Отменить";
			this.btnCancel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
			// 
			// PagesData
			// 
			this.PagesData.Controls.Add(this.SheetCommon);
			this.PagesData.Controls.Add(this.SheetText);
			this.PagesData.Controls.Add(this.SheetRepositories);
			this.PagesData.Controls.Add(this.SheetNotes);
			this.PagesData.Controls.Add(this.SheetMultimedia);
			this.PagesData.Dock = System.Windows.Forms.DockStyle.Top;
			this.PagesData.Location = new System.Drawing.Point(0, 0);
			this.PagesData.Name = "PagesData";
			this.PagesData.SelectedIndex = 0;
			this.PagesData.Size = new System.Drawing.Size(752, 487);
			this.PagesData.TabIndex = 0;
			// 
			// SheetCommon
			// 
			this.SheetCommon.Controls.Add(this.Label1);
			this.SheetCommon.Controls.Add(this.Label3);
			this.SheetCommon.Controls.Add(this.Label2);
			this.SheetCommon.Controls.Add(this.Label4);
			this.SheetCommon.Controls.Add(this.EditShortTitle);
			this.SheetCommon.Controls.Add(this.EditAuthor);
			this.SheetCommon.Controls.Add(this.EditTitle);
			this.SheetCommon.Controls.Add(this.EditPublication);
			this.SheetCommon.Location = new System.Drawing.Point(4, 26);
			this.SheetCommon.Name = "SheetCommon";
			this.SheetCommon.Size = new System.Drawing.Size(744, 457);
			this.SheetCommon.TabIndex = 0;
			this.SheetCommon.Text = "Основное";
			// 
			// Label1
			// 
			this.Label1.AutoSize = true;
			this.Label1.Location = new System.Drawing.Point(11, 10);
			this.Label1.Name = "Label1";
			this.Label1.Size = new System.Drawing.Size(123, 17);
			this.Label1.TabIndex = 0;
			this.Label1.Text = "Краткое название";
			// 
			// Label3
			// 
			this.Label3.AutoSize = true;
			this.Label3.Location = new System.Drawing.Point(11, 39);
			this.Label3.Name = "Label3";
			this.Label3.Size = new System.Drawing.Size(47, 17);
			this.Label3.TabIndex = 2;
			this.Label3.Text = "Автор";
			// 
			// Label2
			// 
			this.Label2.AutoSize = true;
			this.Label2.Location = new System.Drawing.Point(11, 175);
			this.Label2.Name = "Label2";
			this.Label2.Size = new System.Drawing.Size(67, 17);
			this.Label2.TabIndex = 4;
			this.Label2.Text = "Название";
			// 
			// Label4
			// 
			this.Label4.AutoSize = true;
			this.Label4.Location = new System.Drawing.Point(11, 311);
			this.Label4.Name = "Label4";
			this.Label4.Size = new System.Drawing.Size(103, 17);
			this.Label4.TabIndex = 6;
			this.Label4.Text = "Опубликовано";
			// 
			// EditShortTitle
			// 
			this.EditShortTitle.Location = new System.Drawing.Point(157, 10);
			this.EditShortTitle.Name = "EditShortTitle";
			this.EditShortTitle.Size = new System.Drawing.Size(326, 24);
			this.EditShortTitle.TabIndex = 1;
			this.EditShortTitle.TextChanged += new System.EventHandler(this.EditShortTitle_TextChanged);
			// 
			// EditAuthor
			// 
			this.EditAuthor.Location = new System.Drawing.Point(157, 39);
			this.EditAuthor.Multiline = true;
			this.EditAuthor.Name = "EditAuthor";
			this.EditAuthor.ScrollBars = System.Windows.Forms.ScrollBars.Vertical;
			this.EditAuthor.Size = new System.Drawing.Size(572, 127);
			this.EditAuthor.TabIndex = 3;
			// 
			// EditTitle
			// 
			this.EditTitle.Location = new System.Drawing.Point(157, 175);
			this.EditTitle.Multiline = true;
			this.EditTitle.Name = "EditTitle";
			this.EditTitle.ScrollBars = System.Windows.Forms.ScrollBars.Vertical;
			this.EditTitle.Size = new System.Drawing.Size(572, 127);
			this.EditTitle.TabIndex = 5;
			// 
			// EditPublication
			// 
			this.EditPublication.Location = new System.Drawing.Point(157, 311);
			this.EditPublication.Multiline = true;
			this.EditPublication.Name = "EditPublication";
			this.EditPublication.ScrollBars = System.Windows.Forms.ScrollBars.Vertical;
			this.EditPublication.Size = new System.Drawing.Size(572, 127);
			this.EditPublication.TabIndex = 7;
			// 
			// SheetText
			// 
			this.SheetText.Controls.Add(this.EditText);
			this.SheetText.Location = new System.Drawing.Point(4, 26);
			this.SheetText.Name = "SheetText";
			this.SheetText.Size = new System.Drawing.Size(744, 457);
			this.SheetText.TabIndex = 1;
			this.SheetText.Text = "Текст";
			// 
			// EditText
			// 
			this.EditText.Dock = System.Windows.Forms.DockStyle.Fill;
			this.EditText.Location = new System.Drawing.Point(0, 0);
			this.EditText.Multiline = true;
			this.EditText.Name = "EditText";
			this.EditText.ScrollBars = System.Windows.Forms.ScrollBars.Both;
			this.EditText.Size = new System.Drawing.Size(744, 457);
			this.EditText.TabIndex = 0;
			// 
			// SheetRepositories
			// 
			this.SheetRepositories.Location = new System.Drawing.Point(4, 26);
			this.SheetRepositories.Name = "SheetRepositories";
			this.SheetRepositories.Size = new System.Drawing.Size(744, 457);
			this.SheetRepositories.TabIndex = 2;
			this.SheetRepositories.Text = "Архивы";
			// 
			// SheetNotes
			// 
			this.SheetNotes.Location = new System.Drawing.Point(4, 26);
			this.SheetNotes.Name = "SheetNotes";
			this.SheetNotes.Size = new System.Drawing.Size(744, 457);
			this.SheetNotes.TabIndex = 3;
			this.SheetNotes.Text = "Заметки";
			// 
			// SheetMultimedia
			// 
			this.SheetMultimedia.Location = new System.Drawing.Point(4, 26);
			this.SheetMultimedia.Name = "SheetMultimedia";
			this.SheetMultimedia.Size = new System.Drawing.Size(744, 457);
			this.SheetMultimedia.TabIndex = 4;
			this.SheetMultimedia.Text = "Мультимедиа";
			// 
			// SourceEditDlg
			// 
			this.AcceptButton = this.btnAccept;
			this.AutoScaleDimensions = new System.Drawing.SizeF(120F, 120F);
			this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
			this.CancelButton = this.btnCancel;
			this.ClientSize = new System.Drawing.Size(752, 549);
			this.Controls.Add(this.btnAccept);
			this.Controls.Add(this.btnCancel);
			this.Controls.Add(this.PagesData);
			this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
			this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
			this.MaximizeBox = false;
			this.MinimizeBox = false;
			this.Name = "SourceEditDlg";
			this.ShowInTaskbar = false;
			this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
			this.Text = "Источник";
			this.PagesData.ResumeLayout(false);
			this.SheetCommon.ResumeLayout(false);
			this.SheetCommon.PerformLayout();
			this.SheetText.ResumeLayout(false);
			this.SheetText.PerformLayout();
			this.ResumeLayout(false);
		}
	}
}