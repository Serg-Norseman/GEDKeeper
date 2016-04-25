using System;
using System.Drawing;
using System.Windows.Forms;

namespace GKUI.Dialogs
{
	partial class AddressEditDlg
	{
		private System.Windows.Forms.Button btnAccept;
		private System.Windows.Forms.Button btnCancel;
		private System.Windows.Forms.TabControl PageAddrData;
		private System.Windows.Forms.TabPage SheetPhones;
		private System.Windows.Forms.TabPage SheetEmails;
		private System.Windows.Forms.TabPage SheetCommon;
		private System.Windows.Forms.TabPage SheetWebPages;
		private System.Windows.Forms.Label Label1;
		private System.Windows.Forms.Label Label2;
		private System.Windows.Forms.Label Label3;
		private System.Windows.Forms.Label Label4;
		private System.Windows.Forms.Label Label5;
		private System.Windows.Forms.TextBox edCountry;
		private System.Windows.Forms.TextBox edState;
		private System.Windows.Forms.TextBox edCity;
		private System.Windows.Forms.TextBox edPostalCode;
		private System.Windows.Forms.TextBox edAddress;

		private void InitializeComponent()
		{
			this.btnAccept = new System.Windows.Forms.Button();
			this.btnCancel = new System.Windows.Forms.Button();
			this.PageAddrData = new System.Windows.Forms.TabControl();
			this.SheetCommon = new System.Windows.Forms.TabPage();
			this.Label1 = new System.Windows.Forms.Label();
			this.Label2 = new System.Windows.Forms.Label();
			this.Label3 = new System.Windows.Forms.Label();
			this.Label4 = new System.Windows.Forms.Label();
			this.Label5 = new System.Windows.Forms.Label();
			this.edCountry = new System.Windows.Forms.TextBox();
			this.edState = new System.Windows.Forms.TextBox();
			this.edCity = new System.Windows.Forms.TextBox();
			this.edPostalCode = new System.Windows.Forms.TextBox();
			this.edAddress = new System.Windows.Forms.TextBox();
			this.SheetPhones = new System.Windows.Forms.TabPage();
			this.SheetEmails = new System.Windows.Forms.TabPage();
			this.SheetWebPages = new System.Windows.Forms.TabPage();
			this.PageAddrData.SuspendLayout();
			this.SheetCommon.SuspendLayout();
			this.SuspendLayout();
			// 
			// btnAccept
			// 
			this.btnAccept.Image = global::GKResources.iBtnAccept;
			this.btnAccept.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
			this.btnAccept.Location = new System.Drawing.Point(232, 280);
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
			this.btnCancel.Location = new System.Drawing.Point(320, 280);
			this.btnCancel.Name = "btnCancel";
			this.btnCancel.Size = new System.Drawing.Size(81, 25);
			this.btnCancel.TabIndex = 2;
			this.btnCancel.Text = "Отменить";
			this.btnCancel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
			// 
			// PageAddrData
			// 
			this.PageAddrData.Controls.Add(this.SheetCommon);
			this.PageAddrData.Controls.Add(this.SheetPhones);
			this.PageAddrData.Controls.Add(this.SheetEmails);
			this.PageAddrData.Controls.Add(this.SheetWebPages);
			this.PageAddrData.Dock = System.Windows.Forms.DockStyle.Top;
			this.PageAddrData.Location = new System.Drawing.Point(0, 0);
			this.PageAddrData.Name = "PageAddrData";
			this.PageAddrData.SelectedIndex = 0;
			this.PageAddrData.Size = new System.Drawing.Size(409, 264);
			this.PageAddrData.TabIndex = 0;
			// 
			// SheetCommon
			// 
			this.SheetCommon.Controls.Add(this.Label1);
			this.SheetCommon.Controls.Add(this.Label2);
			this.SheetCommon.Controls.Add(this.Label3);
			this.SheetCommon.Controls.Add(this.Label4);
			this.SheetCommon.Controls.Add(this.Label5);
			this.SheetCommon.Controls.Add(this.edCountry);
			this.SheetCommon.Controls.Add(this.edState);
			this.SheetCommon.Controls.Add(this.edCity);
			this.SheetCommon.Controls.Add(this.edPostalCode);
			this.SheetCommon.Controls.Add(this.edAddress);
			this.SheetCommon.Location = new System.Drawing.Point(4, 22);
			this.SheetCommon.Name = "SheetCommon";
			this.SheetCommon.Size = new System.Drawing.Size(401, 238);
			this.SheetCommon.TabIndex = 0;
			this.SheetCommon.Text = "Адрес";
			// 
			// Label1
			// 
			this.Label1.Location = new System.Drawing.Point(8, 8);
			this.Label1.Name = "Label1";
			this.Label1.Size = new System.Drawing.Size(60, 13);
			this.Label1.TabIndex = 0;
			this.Label1.Text = "Страна";
			// 
			// Label2
			// 
			this.Label2.Location = new System.Drawing.Point(216, 8);
			this.Label2.Name = "Label2";
			this.Label2.Size = new System.Drawing.Size(90, 13);
			this.Label2.TabIndex = 1;
			this.Label2.Text = "Штат/Область";
			// 
			// Label3
			// 
			this.Label3.Location = new System.Drawing.Point(8, 56);
			this.Label3.Name = "Label3";
			this.Label3.Size = new System.Drawing.Size(60, 13);
			this.Label3.TabIndex = 2;
			this.Label3.Text = "Город";
			// 
			// Label4
			// 
			this.Label4.Location = new System.Drawing.Point(216, 56);
			this.Label4.Name = "Label4";
			this.Label4.Size = new System.Drawing.Size(90, 13);
			this.Label4.TabIndex = 3;
			this.Label4.Text = "Почтовый код";
			// 
			// Label5
			// 
			this.Label5.Location = new System.Drawing.Point(8, 104);
			this.Label5.Name = "Label5";
			this.Label5.Size = new System.Drawing.Size(60, 13);
			this.Label5.TabIndex = 4;
			this.Label5.Text = "Адрес";
			// 
			// edCountry
			// 
			this.edCountry.Location = new System.Drawing.Point(8, 24);
			this.edCountry.Name = "edCountry";
			this.edCountry.Size = new System.Drawing.Size(201, 21);
			this.edCountry.TabIndex = 0;
			// 
			// edState
			// 
			this.edState.Location = new System.Drawing.Point(216, 24);
			this.edState.Name = "edState";
			this.edState.Size = new System.Drawing.Size(177, 21);
			this.edState.TabIndex = 1;
			// 
			// edCity
			// 
			this.edCity.Location = new System.Drawing.Point(8, 72);
			this.edCity.Name = "edCity";
			this.edCity.Size = new System.Drawing.Size(201, 21);
			this.edCity.TabIndex = 2;
			// 
			// edPostalCode
			// 
			this.edPostalCode.Location = new System.Drawing.Point(216, 72);
			this.edPostalCode.Name = "edPostalCode";
			this.edPostalCode.Size = new System.Drawing.Size(177, 21);
			this.edPostalCode.TabIndex = 3;
			// 
			// edAddress
			// 
			this.edAddress.Location = new System.Drawing.Point(8, 120);
			this.edAddress.Name = "edAddress";
			this.edAddress.Size = new System.Drawing.Size(385, 21);
			this.edAddress.TabIndex = 4;
			// 
			// SheetPhones
			// 
			this.SheetPhones.Location = new System.Drawing.Point(4, 22);
			this.SheetPhones.Name = "SheetPhones";
			this.SheetPhones.Size = new System.Drawing.Size(401, 238);
			this.SheetPhones.TabIndex = 1;
			this.SheetPhones.Text = "Телефоны";
			// 
			// SheetEmails
			// 
			this.SheetEmails.Location = new System.Drawing.Point(4, 22);
			this.SheetEmails.Name = "SheetEmails";
			this.SheetEmails.Size = new System.Drawing.Size(401, 238);
			this.SheetEmails.TabIndex = 2;
			this.SheetEmails.Text = "Эл. почта";
			// 
			// SheetWebPages
			// 
			this.SheetWebPages.Location = new System.Drawing.Point(4, 22);
			this.SheetWebPages.Name = "SheetWebPages";
			this.SheetWebPages.Size = new System.Drawing.Size(401, 238);
			this.SheetWebPages.TabIndex = 3;
			this.SheetWebPages.Text = "Веб-страницы";
			// 
			// TfmAddressEdit
			// 
			this.AcceptButton = this.btnAccept;
			this.AutoScaleBaseSize = new System.Drawing.Size(5, 14);
			this.CancelButton = this.btnCancel;
			this.ClientSize = new System.Drawing.Size(409, 313);
			this.Controls.Add(this.btnAccept);
			this.Controls.Add(this.btnCancel);
			this.Controls.Add(this.PageAddrData);
			this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
			this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
			this.MaximizeBox = false;
			this.MinimizeBox = false;
			this.Name = "TfmAddressEdit";
			this.ShowInTaskbar = false;
			this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
			this.Text = "Адрес";
			this.PageAddrData.ResumeLayout(false);
			this.SheetCommon.ResumeLayout(false);
			this.SheetCommon.PerformLayout();
			this.ResumeLayout(false);
		}
	}
}