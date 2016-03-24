namespace GKUI.Dialogs
{
	partial class TfmPersonalNameEdit
	{
		/// <summary>
		/// Required designer variable.
		/// </summary>
		private System.ComponentModel.IContainer components = null;

		/// <summary>
		/// Clean up any resources being used.
		/// </summary>
		/// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
		protected override void Dispose(bool disposing)
		{
			if (disposing && (components != null))
			{
				components.Dispose();
			}
			base.Dispose(disposing);
		}

		#region Windows Form Designer generated code

		/// <summary>
		/// Required method for Designer support - do not modify
		/// the contents of this method with the code editor.
		/// </summary>
		private void InitializeComponent()
		{
			this.Label1 = new System.Windows.Forms.Label();
			this.btnAccept = new System.Windows.Forms.Button();
			this.Label2 = new System.Windows.Forms.Label();
			this.Label3 = new System.Windows.Forms.Label();
			this.btnCancel = new System.Windows.Forms.Button();
			this.Label8 = new System.Windows.Forms.Label();
			this.Label6 = new System.Windows.Forms.Label();
			this.Label9 = new System.Windows.Forms.Label();
			this.Label7 = new System.Windows.Forms.Label();
			this.edSurname = new System.Windows.Forms.TextBox();
			this.edName = new System.Windows.Forms.TextBox();
			this.edPatronymic = new System.Windows.Forms.TextBox();
			this.edPieceSurnamePrefix = new System.Windows.Forms.TextBox();
			this.edPiecePrefix = new System.Windows.Forms.TextBox();
			this.edPieceSuffix = new System.Windows.Forms.TextBox();
			this.edPieceNickname = new System.Windows.Forms.TextBox();
			this.lblType = new System.Windows.Forms.Label();
			this.cbNameType = new System.Windows.Forms.ComboBox();
			this.SuspendLayout();
			// 
			// Label1
			// 
			this.Label1.AutoSize = true;
			this.Label1.Location = new System.Drawing.Point(10, 7);
			this.Label1.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
			this.Label1.Name = "Label1";
			this.Label1.Size = new System.Drawing.Size(51, 13);
			this.Label1.TabIndex = 14;
			this.Label1.Text = "Фамилия";
			// 
			// btnAccept
			// 
			this.btnAccept.Image = global::GKResources.iBtnAccept;
			this.btnAccept.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
			this.btnAccept.Location = new System.Drawing.Point(157, 177);
			this.btnAccept.Margin = new System.Windows.Forms.Padding(2);
			this.btnAccept.Name = "btnAccept";
			this.btnAccept.Size = new System.Drawing.Size(91, 24);
			this.btnAccept.TabIndex = 19;
			this.btnAccept.Text = "Принять";
			this.btnAccept.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
			this.btnAccept.Click += new System.EventHandler(this.btnAccept_Click);
			// 
			// Label2
			// 
			this.Label2.AutoSize = true;
			this.Label2.Location = new System.Drawing.Point(10, 46);
			this.Label2.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
			this.Label2.Name = "Label2";
			this.Label2.Size = new System.Drawing.Size(26, 13);
			this.Label2.TabIndex = 16;
			this.Label2.Text = "Имя";
			// 
			// Label3
			// 
			this.Label3.AutoSize = true;
			this.Label3.Location = new System.Drawing.Point(10, 86);
			this.Label3.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
			this.Label3.Name = "Label3";
			this.Label3.Size = new System.Drawing.Size(56, 13);
			this.Label3.TabIndex = 18;
			this.Label3.Text = "Отчество";
			// 
			// btnCancel
			// 
			this.btnCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
			this.btnCancel.Image = global::GKResources.iBtnCancel;
			this.btnCancel.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
			this.btnCancel.Location = new System.Drawing.Point(252, 177);
			this.btnCancel.Margin = new System.Windows.Forms.Padding(2);
			this.btnCancel.Name = "btnCancel";
			this.btnCancel.Size = new System.Drawing.Size(91, 24);
			this.btnCancel.TabIndex = 22;
			this.btnCancel.Text = "Отменить";
			this.btnCancel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
			// 
			// Label8
			// 
			this.Label8.AutoSize = true;
			this.Label8.Location = new System.Drawing.Point(207, 7);
			this.Label8.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
			this.Label8.Name = "Label8";
			this.Label8.Size = new System.Drawing.Size(98, 13);
			this.Label8.TabIndex = 24;
			this.Label8.Text = "Префикс фамилии";
			// 
			// Label6
			// 
			this.Label6.AutoSize = true;
			this.Label6.Location = new System.Drawing.Point(207, 46);
			this.Label6.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
			this.Label6.Name = "Label6";
			this.Label6.Size = new System.Drawing.Size(84, 13);
			this.Label6.TabIndex = 26;
			this.Label6.Text = "Префикс имени";
			// 
			// Label9
			// 
			this.Label9.AutoSize = true;
			this.Label9.Location = new System.Drawing.Point(207, 86);
			this.Label9.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
			this.Label9.Name = "Label9";
			this.Label9.Size = new System.Drawing.Size(86, 13);
			this.Label9.TabIndex = 28;
			this.Label9.Text = "Суффикс имени";
			// 
			// Label7
			// 
			this.Label7.AutoSize = true;
			this.Label7.Location = new System.Drawing.Point(205, 124);
			this.Label7.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
			this.Label7.Name = "Label7";
			this.Label7.Size = new System.Drawing.Size(58, 13);
			this.Label7.TabIndex = 21;
			this.Label7.Text = "Прозвище";
			// 
			// edSurname
			// 
			this.edSurname.Location = new System.Drawing.Point(10, 23);
			this.edSurname.Margin = new System.Windows.Forms.Padding(2);
			this.edSurname.Name = "edSurname";
			this.edSurname.Size = new System.Drawing.Size(182, 21);
			this.edSurname.TabIndex = 15;
			// 
			// edName
			// 
			this.edName.Location = new System.Drawing.Point(10, 62);
			this.edName.Margin = new System.Windows.Forms.Padding(2);
			this.edName.Name = "edName";
			this.edName.Size = new System.Drawing.Size(182, 21);
			this.edName.TabIndex = 17;
			// 
			// edPatronymic
			// 
			this.edPatronymic.Location = new System.Drawing.Point(10, 101);
			this.edPatronymic.Margin = new System.Windows.Forms.Padding(2);
			this.edPatronymic.Name = "edPatronymic";
			this.edPatronymic.Size = new System.Drawing.Size(182, 21);
			this.edPatronymic.TabIndex = 20;
			// 
			// edPieceSurnamePrefix
			// 
			this.edPieceSurnamePrefix.Location = new System.Drawing.Point(207, 23);
			this.edPieceSurnamePrefix.Margin = new System.Windows.Forms.Padding(2);
			this.edPieceSurnamePrefix.Name = "edPieceSurnamePrefix";
			this.edPieceSurnamePrefix.Size = new System.Drawing.Size(136, 21);
			this.edPieceSurnamePrefix.TabIndex = 25;
			// 
			// edPiecePrefix
			// 
			this.edPiecePrefix.Location = new System.Drawing.Point(207, 62);
			this.edPiecePrefix.Margin = new System.Windows.Forms.Padding(2);
			this.edPiecePrefix.Name = "edPiecePrefix";
			this.edPiecePrefix.Size = new System.Drawing.Size(136, 21);
			this.edPiecePrefix.TabIndex = 27;
			// 
			// edPieceSuffix
			// 
			this.edPieceSuffix.Location = new System.Drawing.Point(207, 101);
			this.edPieceSuffix.Margin = new System.Windows.Forms.Padding(2);
			this.edPieceSuffix.Name = "edPieceSuffix";
			this.edPieceSuffix.Size = new System.Drawing.Size(136, 21);
			this.edPieceSuffix.TabIndex = 29;
			// 
			// edPieceNickname
			// 
			this.edPieceNickname.Location = new System.Drawing.Point(207, 140);
			this.edPieceNickname.Margin = new System.Windows.Forms.Padding(2);
			this.edPieceNickname.Name = "edPieceNickname";
			this.edPieceNickname.Size = new System.Drawing.Size(136, 21);
			this.edPieceNickname.TabIndex = 23;
			// 
			// lblType
			// 
			this.lblType.AutoSize = true;
			this.lblType.Location = new System.Drawing.Point(11, 124);
			this.lblType.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
			this.lblType.Name = "lblType";
			this.lblType.Size = new System.Drawing.Size(25, 13);
			this.lblType.TabIndex = 30;
			this.lblType.Text = "Тип";
			// 
			// cbNameType
			// 
			this.cbNameType.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
			this.cbNameType.Location = new System.Drawing.Point(11, 140);
			this.cbNameType.Margin = new System.Windows.Forms.Padding(2);
			this.cbNameType.Name = "cbNameType";
			this.cbNameType.Size = new System.Drawing.Size(181, 21);
			this.cbNameType.TabIndex = 31;
			// 
			// TfmPersonalNameEdit
			// 
			this.AcceptButton = this.btnAccept;
			this.AutoScaleDimensions = new System.Drawing.SizeF(96F, 96F);
			this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
			this.CancelButton = this.btnCancel;
			this.ClientSize = new System.Drawing.Size(355, 213);
			this.Controls.Add(this.lblType);
			this.Controls.Add(this.cbNameType);
			this.Controls.Add(this.Label1);
			this.Controls.Add(this.btnAccept);
			this.Controls.Add(this.Label2);
			this.Controls.Add(this.Label3);
			this.Controls.Add(this.btnCancel);
			this.Controls.Add(this.Label8);
			this.Controls.Add(this.Label6);
			this.Controls.Add(this.Label9);
			this.Controls.Add(this.Label7);
			this.Controls.Add(this.edSurname);
			this.Controls.Add(this.edName);
			this.Controls.Add(this.edPatronymic);
			this.Controls.Add(this.edPieceSurnamePrefix);
			this.Controls.Add(this.edPiecePrefix);
			this.Controls.Add(this.edPieceSuffix);
			this.Controls.Add(this.edPieceNickname);
			this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
			this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
			this.Margin = new System.Windows.Forms.Padding(2);
			this.MaximizeBox = false;
			this.MinimizeBox = false;
			this.Name = "TfmPersonalNameEdit";
			this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
			this.Text = "TfmPersonalNameEdit";
			this.ResumeLayout(false);
			this.PerformLayout();
		}
		private System.Windows.Forms.ComboBox cbNameType;
		private System.Windows.Forms.Label lblType;
		private System.Windows.Forms.TextBox edPieceNickname;
		private System.Windows.Forms.TextBox edPieceSuffix;
		private System.Windows.Forms.TextBox edPiecePrefix;
		private System.Windows.Forms.TextBox edPieceSurnamePrefix;
		private System.Windows.Forms.TextBox edPatronymic;
		private System.Windows.Forms.TextBox edName;
		private System.Windows.Forms.TextBox edSurname;
		private System.Windows.Forms.Label Label7;
		private System.Windows.Forms.Label Label9;
		private System.Windows.Forms.Label Label6;
		private System.Windows.Forms.Label Label8;
		private System.Windows.Forms.Button btnCancel;
		private System.Windows.Forms.Label Label3;
		private System.Windows.Forms.Label Label2;
		private System.Windows.Forms.Button btnAccept;
		private System.Windows.Forms.Label Label1;

		#endregion
	}
}