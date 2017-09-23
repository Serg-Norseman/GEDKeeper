namespace GKUI.Dialogs
{
	partial class AddressEditDlg
	{
		private System.Windows.Forms.Button btnAccept;
		private System.Windows.Forms.Button btnCancel;
		private System.Windows.Forms.TabControl tabsAddrData;
		private System.Windows.Forms.TabPage pagePhones;
		private System.Windows.Forms.TabPage pageEmails;
		private System.Windows.Forms.TabPage pageCommon;
		private System.Windows.Forms.TabPage pageWebPages;
		private System.Windows.Forms.Label lblCountry;
		private System.Windows.Forms.Label lblState;
		private System.Windows.Forms.Label lblCity;
		private System.Windows.Forms.Label lblPostalCode;
		private System.Windows.Forms.Label lblAddress;
		private System.Windows.Forms.TextBox txtCountry;
		private System.Windows.Forms.TextBox txtState;
		private System.Windows.Forms.TextBox txtCity;
		private System.Windows.Forms.TextBox txtPostalCode;
		private System.Windows.Forms.TextBox txtAddress;

		private void InitializeComponent()
		{
			this.btnAccept = new System.Windows.Forms.Button();
			this.btnCancel = new System.Windows.Forms.Button();
			this.tabsAddrData = new System.Windows.Forms.TabControl();
			this.pageCommon = new System.Windows.Forms.TabPage();
			this.lblCountry = new System.Windows.Forms.Label();
			this.lblState = new System.Windows.Forms.Label();
			this.lblCity = new System.Windows.Forms.Label();
			this.lblPostalCode = new System.Windows.Forms.Label();
			this.lblAddress = new System.Windows.Forms.Label();
			this.txtCountry = new System.Windows.Forms.TextBox();
			this.txtState = new System.Windows.Forms.TextBox();
			this.txtCity = new System.Windows.Forms.TextBox();
			this.txtPostalCode = new System.Windows.Forms.TextBox();
			this.txtAddress = new System.Windows.Forms.TextBox();
			this.pagePhones = new System.Windows.Forms.TabPage();
			this.pageEmails = new System.Windows.Forms.TabPage();
			this.pageWebPages = new System.Windows.Forms.TabPage();
			this.tabsAddrData.SuspendLayout();
			this.pageCommon.SuspendLayout();
			this.SuspendLayout();
			// 
			// btnAccept
			// 
			this.btnAccept.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
			this.btnAccept.Location = new System.Drawing.Point(325, 340);
			this.btnAccept.Name = "btnAccept";
			this.btnAccept.Size = new System.Drawing.Size(113, 30);
			this.btnAccept.TabIndex = 1;
			this.btnAccept.Text = "btnAccept";
			this.btnAccept.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
			this.btnAccept.Click += new System.EventHandler(this.btnAccept_Click);
			// 
			// btnCancel
			// 
			this.btnCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
			this.btnCancel.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
			this.btnCancel.Location = new System.Drawing.Point(448, 340);
			this.btnCancel.Name = "btnCancel";
			this.btnCancel.Size = new System.Drawing.Size(113, 30);
			this.btnCancel.TabIndex = 2;
			this.btnCancel.Text = "btnCancel";
			this.btnCancel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
			// 
			// tabsAddrData
			// 
			this.tabsAddrData.Controls.Add(this.pageCommon);
			this.tabsAddrData.Controls.Add(this.pagePhones);
			this.tabsAddrData.Controls.Add(this.pageEmails);
			this.tabsAddrData.Controls.Add(this.pageWebPages);
			this.tabsAddrData.Dock = System.Windows.Forms.DockStyle.Top;
			this.tabsAddrData.Location = new System.Drawing.Point(0, 0);
			this.tabsAddrData.Name = "tabsAddrData";
			this.tabsAddrData.SelectedIndex = 0;
			this.tabsAddrData.Size = new System.Drawing.Size(572, 321);
			this.tabsAddrData.TabIndex = 0;
			// 
			// pageCommon
			// 
			this.pageCommon.Controls.Add(this.lblCountry);
			this.pageCommon.Controls.Add(this.lblState);
			this.pageCommon.Controls.Add(this.lblCity);
			this.pageCommon.Controls.Add(this.lblPostalCode);
			this.pageCommon.Controls.Add(this.lblAddress);
			this.pageCommon.Controls.Add(this.txtCountry);
			this.pageCommon.Controls.Add(this.txtState);
			this.pageCommon.Controls.Add(this.txtCity);
			this.pageCommon.Controls.Add(this.txtPostalCode);
			this.pageCommon.Controls.Add(this.txtAddress);
			this.pageCommon.Location = new System.Drawing.Point(4, 26);
			this.pageCommon.Name = "pageCommon";
			this.pageCommon.Size = new System.Drawing.Size(564, 291);
			this.pageCommon.TabIndex = 0;
			this.pageCommon.Text = "pageCommon";
			// 
			// lblCountry
			// 
			this.lblCountry.AutoSize = true;
			this.lblCountry.Location = new System.Drawing.Point(11, 10);
			this.lblCountry.Name = "lblCountry";
			this.lblCountry.Size = new System.Drawing.Size(55, 17);
			this.lblCountry.TabIndex = 0;
			this.lblCountry.Text = "lblCountry";
			// 
			// lblState
			// 
			this.lblState.AutoSize = true;
			this.lblState.Location = new System.Drawing.Point(302, 10);
			this.lblState.Name = "lblState";
			this.lblState.Size = new System.Drawing.Size(103, 17);
			this.lblState.TabIndex = 1;
			this.lblState.Text = "lblState";
			// 
			// lblCity
			// 
			this.lblCity.AutoSize = true;
			this.lblCity.Location = new System.Drawing.Point(11, 68);
			this.lblCity.Name = "lblCity";
			this.lblCity.Size = new System.Drawing.Size(47, 17);
			this.lblCity.TabIndex = 2;
			this.lblCity.Text = "lblCity";
			// 
			// lblPostalCode
			// 
			this.lblPostalCode.AutoSize = true;
			this.lblPostalCode.Location = new System.Drawing.Point(302, 68);
			this.lblPostalCode.Name = "lblPostalCode";
			this.lblPostalCode.Size = new System.Drawing.Size(101, 17);
			this.lblPostalCode.TabIndex = 3;
			this.lblPostalCode.Text = "lblPostalCode";
			// 
			// lblAddress
			// 
			this.lblAddress.AutoSize = true;
			this.lblAddress.Location = new System.Drawing.Point(11, 126);
			this.lblAddress.Name = "lblAddress";
			this.lblAddress.Size = new System.Drawing.Size(46, 17);
			this.lblAddress.TabIndex = 4;
			this.lblAddress.Text = "lblAddress";
			// 
			// txtCountry
			// 
			this.txtCountry.Location = new System.Drawing.Point(11, 29);
			this.txtCountry.Name = "txtCountry";
			this.txtCountry.Size = new System.Drawing.Size(282, 24);
			this.txtCountry.TabIndex = 0;
			// 
			// txtState
			// 
			this.txtState.Location = new System.Drawing.Point(302, 29);
			this.txtState.Name = "txtState";
			this.txtState.Size = new System.Drawing.Size(248, 24);
			this.txtState.TabIndex = 1;
			// 
			// txtCity
			// 
			this.txtCity.Location = new System.Drawing.Point(11, 87);
			this.txtCity.Name = "txtCity";
			this.txtCity.Size = new System.Drawing.Size(282, 24);
			this.txtCity.TabIndex = 2;
			// 
			// txtPostalCode
			// 
			this.txtPostalCode.Location = new System.Drawing.Point(302, 87);
			this.txtPostalCode.Name = "txtPostalCode";
			this.txtPostalCode.Size = new System.Drawing.Size(248, 24);
			this.txtPostalCode.TabIndex = 3;
			// 
			// txtAddress
			// 
			this.txtAddress.Location = new System.Drawing.Point(11, 146);
			this.txtAddress.Name = "txtAddress";
			this.txtAddress.Size = new System.Drawing.Size(539, 24);
			this.txtAddress.TabIndex = 4;
			// 
			// pagePhones
			// 
			this.pagePhones.Location = new System.Drawing.Point(4, 26);
			this.pagePhones.Name = "pagePhones";
			this.pagePhones.Size = new System.Drawing.Size(564, 291);
			this.pagePhones.TabIndex = 1;
			this.pagePhones.Text = "pagePhones";
			// 
			// pageEmails
			// 
			this.pageEmails.Location = new System.Drawing.Point(4, 26);
			this.pageEmails.Name = "pageEmails";
			this.pageEmails.Size = new System.Drawing.Size(564, 291);
			this.pageEmails.TabIndex = 2;
			this.pageEmails.Text = "pageEmails";
			// 
			// pageWebPages
			// 
			this.pageWebPages.Location = new System.Drawing.Point(4, 26);
			this.pageWebPages.Name = "pageWebPages";
			this.pageWebPages.Size = new System.Drawing.Size(564, 291);
			this.pageWebPages.TabIndex = 3;
			this.pageWebPages.Text = "pageWebPages";
			// 
			// AddressEditDlg
			// 
			this.AcceptButton = this.btnAccept;
			this.AutoScaleDimensions = new System.Drawing.SizeF(120F, 120F);
			this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
			this.CancelButton = this.btnCancel;
			this.ClientSize = new System.Drawing.Size(572, 385);
			this.Controls.Add(this.btnAccept);
			this.Controls.Add(this.btnCancel);
			this.Controls.Add(this.tabsAddrData);
			this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
			this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
			this.MaximizeBox = false;
			this.MinimizeBox = false;
			this.Name = "AddressEditDlg";
			this.ShowInTaskbar = false;
			this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
			this.Text = "AddressEditDlg";
			this.tabsAddrData.ResumeLayout(false);
			this.pageCommon.ResumeLayout(false);
			this.pageCommon.PerformLayout();
			this.ResumeLayout(false);
		}
	}
}