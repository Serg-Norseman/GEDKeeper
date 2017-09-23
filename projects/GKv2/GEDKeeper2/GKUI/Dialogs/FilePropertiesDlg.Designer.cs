namespace GKUI.Dialogs
{
	partial class FilePropertiesDlg
	{
		private System.Windows.Forms.Button btnAccept;
		private System.Windows.Forms.Button btnCancel;
		private System.Windows.Forms.TabPage pageAuthor;
		private System.Windows.Forms.Label lblName;
		private System.Windows.Forms.Label lblAddress;
		private System.Windows.Forms.Label lblTelephone;
		private System.Windows.Forms.TextBox txtName;
		private System.Windows.Forms.TextBox txtTel;
		private System.Windows.Forms.TextBox txtAddress;
		private System.Windows.Forms.TabPage pageOther;
		private System.Windows.Forms.TabControl PageControl1;
		private System.Windows.Forms.ColumnHeader columnHeader2;
		private System.Windows.Forms.ColumnHeader columnHeader1;
		private System.Windows.Forms.ListView lvRecordStats;

		private void InitializeComponent()
		{
		    this.btnAccept = new System.Windows.Forms.Button();
		    this.btnCancel = new System.Windows.Forms.Button();
		    this.PageControl1 = new System.Windows.Forms.TabControl();
		    this.pageAuthor = new System.Windows.Forms.TabPage();
		    this.btnLangEdit = new System.Windows.Forms.Button();
		    this.lblName = new System.Windows.Forms.Label();
		    this.lblAddress = new System.Windows.Forms.Label();
		    this.lblLanguage = new System.Windows.Forms.Label();
		    this.lblTelephone = new System.Windows.Forms.Label();
		    this.txtName = new System.Windows.Forms.TextBox();
		    this.txtLanguage = new System.Windows.Forms.TextBox();
		    this.txtTel = new System.Windows.Forms.TextBox();
		    this.txtAddress = new System.Windows.Forms.TextBox();
		    this.pageOther = new System.Windows.Forms.TabPage();
		    this.lvRecordStats = new System.Windows.Forms.ListView();
		    this.columnHeader1 = new System.Windows.Forms.ColumnHeader();
		    this.columnHeader2 = new System.Windows.Forms.ColumnHeader();
		    this.PageControl1.SuspendLayout();
		    this.pageAuthor.SuspendLayout();
		    this.pageOther.SuspendLayout();
		    this.SuspendLayout();
		    // 
		    // btnAccept
		    // 
		    this.btnAccept.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
		    this.btnAccept.Location = new System.Drawing.Point(381, 359);
		    this.btnAccept.Margin = new System.Windows.Forms.Padding(2);
		    this.btnAccept.Name = "btnAccept";
		    this.btnAccept.Size = new System.Drawing.Size(112, 31);
		    this.btnAccept.TabIndex = 1;
		    this.btnAccept.Text = "btnAccept";
		    this.btnAccept.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
		    this.btnAccept.Click += new System.EventHandler(this.btnAccept_Click);
		    // 
		    // btnCancel
		    // 
		    this.btnCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
		    this.btnCancel.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
		    this.btnCancel.Location = new System.Drawing.Point(504, 359);
		    this.btnCancel.Margin = new System.Windows.Forms.Padding(2);
		    this.btnCancel.Name = "btnCancel";
		    this.btnCancel.Size = new System.Drawing.Size(112, 31);
		    this.btnCancel.TabIndex = 2;
		    this.btnCancel.Text = "btnCancel";
		    this.btnCancel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
		    // 
		    // PageControl1
		    // 
		    this.PageControl1.Controls.Add(this.pageAuthor);
		    this.PageControl1.Controls.Add(this.pageOther);
		    this.PageControl1.Location = new System.Drawing.Point(11, 10);
		    this.PageControl1.Margin = new System.Windows.Forms.Padding(2);
		    this.PageControl1.Name = "PageControl1";
		    this.PageControl1.SelectedIndex = 0;
		    this.PageControl1.Size = new System.Drawing.Size(606, 331);
		    this.PageControl1.TabIndex = 0;
		    // 
		    // pageAuthor
		    // 
		    this.pageAuthor.Controls.Add(this.btnLangEdit);
		    this.pageAuthor.Controls.Add(this.lblName);
		    this.pageAuthor.Controls.Add(this.lblAddress);
		    this.pageAuthor.Controls.Add(this.lblLanguage);
		    this.pageAuthor.Controls.Add(this.lblTelephone);
		    this.pageAuthor.Controls.Add(this.txtName);
		    this.pageAuthor.Controls.Add(this.txtLanguage);
		    this.pageAuthor.Controls.Add(this.txtTel);
		    this.pageAuthor.Controls.Add(this.txtAddress);
		    this.pageAuthor.Location = new System.Drawing.Point(4, 26);
		    this.pageAuthor.Margin = new System.Windows.Forms.Padding(2);
		    this.pageAuthor.Name = "pageAuthor";
		    this.pageAuthor.Size = new System.Drawing.Size(598, 301);
		    this.pageAuthor.TabIndex = 0;
		    this.pageAuthor.Text = "pageAuthor";
		    // 
		    // btnLangEdit
		    // 
		    this.btnLangEdit.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
		    this.btnLangEdit.Location = new System.Drawing.Point(545, 220);
		    this.btnLangEdit.Name = "btnLangEdit";
		    this.btnLangEdit.Size = new System.Drawing.Size(39, 34);
		    this.btnLangEdit.TabIndex = 6;
		    this.btnLangEdit.Click += new System.EventHandler(this.btnLangEdit_Click);
		    // 
		    // lblName
		    // 
		    this.lblName.AutoSize = true;
		    this.lblName.Location = new System.Drawing.Point(11, 14);
		    this.lblName.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
		    this.lblName.Name = "lblName";
		    this.lblName.Size = new System.Drawing.Size(55, 17);
		    this.lblName.TabIndex = 0;
		    this.lblName.Text = "lblName";
		    // 
		    // lblAddress
		    // 
		    this.lblAddress.AutoSize = true;
		    this.lblAddress.Location = new System.Drawing.Point(11, 39);
		    this.lblAddress.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
		    this.lblAddress.Name = "lblAddress";
		    this.lblAddress.Size = new System.Drawing.Size(68, 17);
		    this.lblAddress.TabIndex = 2;
		    this.lblAddress.Text = "lblAddress";
		    // 
		    // lblLanguage
		    // 
		    this.lblLanguage.AutoSize = true;
		    this.lblLanguage.Location = new System.Drawing.Point(11, 229);
		    this.lblLanguage.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
		    this.lblLanguage.Name = "lblLanguage";
		    this.lblLanguage.Size = new System.Drawing.Size(80, 17);
		    this.lblLanguage.TabIndex = 4;
		    this.lblLanguage.Text = "lblLanguage";
		    // 
		    // lblTelephone
		    // 
		    this.lblTelephone.AutoSize = true;
		    this.lblTelephone.Location = new System.Drawing.Point(11, 189);
		    this.lblTelephone.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
		    this.lblTelephone.Name = "lblTelephone";
		    this.lblTelephone.Size = new System.Drawing.Size(83, 17);
		    this.lblTelephone.TabIndex = 4;
		    this.lblTelephone.Text = "lblTelephone";
		    // 
		    // txtName
		    // 
		    this.txtName.Location = new System.Drawing.Point(145, 10);
		    this.txtName.Margin = new System.Windows.Forms.Padding(2);
		    this.txtName.Name = "txtName";
		    this.txtName.Size = new System.Drawing.Size(439, 24);
		    this.txtName.TabIndex = 1;
		    // 
		    // txtLanguage
		    // 
		    this.txtLanguage.Location = new System.Drawing.Point(145, 226);
		    this.txtLanguage.Margin = new System.Windows.Forms.Padding(2);
		    this.txtLanguage.Name = "txtLanguage";
		    this.txtLanguage.ReadOnly = true;
		    this.txtLanguage.Size = new System.Drawing.Size(386, 24);
		    this.txtLanguage.TabIndex = 5;
		    // 
		    // txtTel
		    // 
		    this.txtTel.Location = new System.Drawing.Point(145, 185);
		    this.txtTel.Margin = new System.Windows.Forms.Padding(2);
		    this.txtTel.Name = "txtTel";
		    this.txtTel.Size = new System.Drawing.Size(439, 24);
		    this.txtTel.TabIndex = 5;
		    // 
		    // txtAddress
		    // 
		    this.txtAddress.Location = new System.Drawing.Point(145, 39);
		    this.txtAddress.Margin = new System.Windows.Forms.Padding(2);
		    this.txtAddress.Multiline = true;
		    this.txtAddress.Name = "txtAddress";
		    this.txtAddress.Size = new System.Drawing.Size(439, 136);
		    this.txtAddress.TabIndex = 3;
		    // 
		    // pageOther
		    // 
		    this.pageOther.Controls.Add(this.lvRecordStats);
		    this.pageOther.Location = new System.Drawing.Point(4, 26);
		    this.pageOther.Margin = new System.Windows.Forms.Padding(2);
		    this.pageOther.Name = "pageOther";
		    this.pageOther.Size = new System.Drawing.Size(598, 301);
		    this.pageOther.TabIndex = 1;
		    this.pageOther.Text = "pageOther";
		    // 
		    // lvRecordStats
		    // 
		    this.lvRecordStats.Columns.AddRange(new System.Windows.Forms.ColumnHeader[] {
		    		    		    this.columnHeader1,
		    		    		    this.columnHeader2});
		    this.lvRecordStats.Dock = System.Windows.Forms.DockStyle.Fill;
		    this.lvRecordStats.FullRowSelect = true;
		    this.lvRecordStats.Location = new System.Drawing.Point(0, 0);
		    this.lvRecordStats.Margin = new System.Windows.Forms.Padding(2);
		    this.lvRecordStats.MultiSelect = false;
		    this.lvRecordStats.Name = "lvRecordStats";
		    this.lvRecordStats.Size = new System.Drawing.Size(598, 301);
		    this.lvRecordStats.TabIndex = 1;
		    this.lvRecordStats.UseCompatibleStateImageBehavior = false;
		    this.lvRecordStats.View = System.Windows.Forms.View.Details;
		    // 
		    // columnHeader1
		    // 
		    this.columnHeader1.Text = "Records";
		    this.columnHeader1.Width = 300;
		    // 
		    // columnHeader2
		    // 
		    this.columnHeader2.Text = "Count";
		    this.columnHeader2.TextAlign = System.Windows.Forms.HorizontalAlignment.Right;
		    this.columnHeader2.Width = 100;
		    // 
		    // FilePropertiesDlg
		    // 
		    this.AcceptButton = this.btnAccept;
		    this.AutoScaleDimensions = new System.Drawing.SizeF(120F, 120F);
		    this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
		    this.CancelButton = this.btnCancel;
		    this.ClientSize = new System.Drawing.Size(630, 405);
		    this.Controls.Add(this.btnAccept);
		    this.Controls.Add(this.btnCancel);
		    this.Controls.Add(this.PageControl1);
		    this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
		    this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
		    this.Margin = new System.Windows.Forms.Padding(2);
		    this.MaximizeBox = false;
		    this.MinimizeBox = false;
		    this.Name = "FilePropertiesDlg";
		    this.ShowInTaskbar = false;
		    this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
		    this.Text = "FilePropertiesDlg";
		    this.PageControl1.ResumeLayout(false);
		    this.pageAuthor.ResumeLayout(false);
		    this.pageAuthor.PerformLayout();
		    this.pageOther.ResumeLayout(false);
		    this.ResumeLayout(false);
		}
		private System.Windows.Forms.Button btnLangEdit;
		private System.Windows.Forms.TextBox txtLanguage;
		private System.Windows.Forms.Label lblLanguage;
	}
}