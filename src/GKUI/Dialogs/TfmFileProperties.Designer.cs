using System;

namespace GKUI
{
	partial class TfmFileProperties
	{
		private System.Windows.Forms.Button btnAccept;
		private System.Windows.Forms.Button btnCancel;
		private System.Windows.Forms.TabPage SheetAuthor;
		private System.Windows.Forms.Label Label1;
		private System.Windows.Forms.Label Label2;
		private System.Windows.Forms.Label Label3;
		private System.Windows.Forms.TextBox EditName;
		private System.Windows.Forms.TextBox EditTel;
		private System.Windows.Forms.TextBox MemoAddress;
		private System.Windows.Forms.TabPage SheetAdvanced;
		public System.Windows.Forms.TabControl PageControl1;

		private void InitializeComponent()
		{
			this.btnAccept = new System.Windows.Forms.Button();
			this.btnCancel = new System.Windows.Forms.Button();
			this.PageControl1 = new System.Windows.Forms.TabControl();
			this.SheetAuthor = new System.Windows.Forms.TabPage();
			this.Label1 = new System.Windows.Forms.Label();
			this.Label2 = new System.Windows.Forms.Label();
			this.Label3 = new System.Windows.Forms.Label();
			this.EditName = new System.Windows.Forms.TextBox();
			this.EditTel = new System.Windows.Forms.TextBox();
			this.MemoAddress = new System.Windows.Forms.TextBox();
			this.SheetAdvanced = new System.Windows.Forms.TabPage();
			this.PageControl1.SuspendLayout();
			this.SheetAuthor.SuspendLayout();
			this.SuspendLayout();
			// 
			// btnAccept
			// 
			this.btnAccept.Image = global::GKResources.iBtnAccept;
			this.btnAccept.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
			this.btnAccept.Location = new System.Drawing.Point(272, 296);
			this.btnAccept.Name = "btnAccept";
			this.btnAccept.Size = new System.Drawing.Size(81, 25);
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
			this.btnCancel.Location = new System.Drawing.Point(360, 296);
			this.btnCancel.Name = "btnCancel";
			this.btnCancel.Size = new System.Drawing.Size(81, 25);
			this.btnCancel.TabIndex = 2;
			this.btnCancel.Text = "Отменить";
			this.btnCancel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
			// 
			// PageControl1
			// 
			this.PageControl1.Controls.Add(this.SheetAuthor);
			this.PageControl1.Controls.Add(this.SheetAdvanced);
			this.PageControl1.Location = new System.Drawing.Point(8, 8);
			this.PageControl1.Name = "PageControl1";
			this.PageControl1.SelectedIndex = 0;
			this.PageControl1.Size = new System.Drawing.Size(433, 273);
			this.PageControl1.TabIndex = 0;
			// 
			// SheetAuthor
			// 
			this.SheetAuthor.Controls.Add(this.Label1);
			this.SheetAuthor.Controls.Add(this.Label2);
			this.SheetAuthor.Controls.Add(this.Label3);
			this.SheetAuthor.Controls.Add(this.EditName);
			this.SheetAuthor.Controls.Add(this.EditTel);
			this.SheetAuthor.Controls.Add(this.MemoAddress);
			this.SheetAuthor.Location = new System.Drawing.Point(4, 22);
			this.SheetAuthor.Name = "SheetAuthor";
			this.SheetAuthor.Size = new System.Drawing.Size(425, 247);
			this.SheetAuthor.TabIndex = 0;
			this.SheetAuthor.Text = "Автор";
			// 
			// Label1
			// 
			this.Label1.Location = new System.Drawing.Point(8, 8);
			this.Label1.Name = "Label1";
			this.Label1.Size = new System.Drawing.Size(40, 13);
			this.Label1.TabIndex = 0;
			this.Label1.Text = "Имя";
			// 
			// Label2
			// 
			this.Label2.Location = new System.Drawing.Point(8, 32);
			this.Label2.Name = "Label2";
			this.Label2.Size = new System.Drawing.Size(40, 13);
			this.Label2.TabIndex = 2;
			this.Label2.Text = "Адрес";
			// 
			// Label3
			// 
			this.Label3.Location = new System.Drawing.Point(8, 152);
			this.Label3.Name = "Label3";
			this.Label3.Size = new System.Drawing.Size(52, 13);
			this.Label3.TabIndex = 4;
			this.Label3.Text = "Телефон";
			// 
			// EditName
			// 
			this.EditName.Location = new System.Drawing.Point(64, 8);
			this.EditName.Name = "EditName";
			this.EditName.Size = new System.Drawing.Size(353, 21);
			this.EditName.TabIndex = 1;
			// 
			// EditTel
			// 
			this.EditTel.Location = new System.Drawing.Point(64, 152);
			this.EditTel.Name = "EditTel";
			this.EditTel.Size = new System.Drawing.Size(353, 21);
			this.EditTel.TabIndex = 5;
			// 
			// MemoAddress
			// 
			this.MemoAddress.Location = new System.Drawing.Point(64, 32);
			this.MemoAddress.Multiline = true;
			this.MemoAddress.Name = "MemoAddress";
			this.MemoAddress.Size = new System.Drawing.Size(353, 113);
			this.MemoAddress.TabIndex = 3;
			// 
			// SheetAdvanced
			// 
			this.SheetAdvanced.Location = new System.Drawing.Point(4, 22);
			this.SheetAdvanced.Name = "SheetAdvanced";
			this.SheetAdvanced.Size = new System.Drawing.Size(425, 247);
			this.SheetAdvanced.TabIndex = 1;
			this.SheetAdvanced.Text = "Прочие";
			// 
			// TfmFileProperties
			// 
			this.AcceptButton = this.btnAccept;
			this.AutoScaleBaseSize = new System.Drawing.Size(5, 14);
			this.CancelButton = this.btnCancel;
			this.ClientSize = new System.Drawing.Size(449, 329);
			this.Controls.Add(this.btnAccept);
			this.Controls.Add(this.btnCancel);
			this.Controls.Add(this.PageControl1);
			this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
			this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
			this.MaximizeBox = false;
			this.MinimizeBox = false;
			this.Name = "TfmFileProperties";
			this.ShowInTaskbar = false;
			this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
			this.Text = "Свойства файла";
			this.PageControl1.ResumeLayout(false);
			this.SheetAuthor.ResumeLayout(false);
			this.SheetAuthor.PerformLayout();
			this.ResumeLayout(false);
		}
	}
}