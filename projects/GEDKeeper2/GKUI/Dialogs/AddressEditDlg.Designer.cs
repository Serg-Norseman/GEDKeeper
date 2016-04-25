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
			System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(AddressEditDlg));
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
			this.btnAccept.Image = ((System.Drawing.Image)(resources.GetObject("btnAccept.Image")));
			this.btnAccept.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
			this.btnAccept.Location = new System.Drawing.Point(325, 340);
			this.btnAccept.Name = "btnAccept";
			this.btnAccept.Size = new System.Drawing.Size(113, 30);
			this.btnAccept.TabIndex = 1;
			this.btnAccept.Text = "Принять";
			this.btnAccept.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
			this.btnAccept.Click += new System.EventHandler(this.btnAccept_Click);
			// 
			// btnCancel
			// 
			this.btnCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
			this.btnCancel.Image = ((System.Drawing.Image)(resources.GetObject("btnCancel.Image")));
			this.btnCancel.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
			this.btnCancel.Location = new System.Drawing.Point(448, 340);
			this.btnCancel.Name = "btnCancel";
			this.btnCancel.Size = new System.Drawing.Size(113, 30);
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
			this.PageAddrData.Size = new System.Drawing.Size(572, 321);
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
			this.SheetCommon.Location = new System.Drawing.Point(4, 26);
			this.SheetCommon.Name = "SheetCommon";
			this.SheetCommon.Size = new System.Drawing.Size(564, 291);
			this.SheetCommon.TabIndex = 0;
			this.SheetCommon.Text = "Адрес";
			// 
			// Label1
			// 
			this.Label1.AutoSize = true;
			this.Label1.Location = new System.Drawing.Point(11, 10);
			this.Label1.Name = "Label1";
			this.Label1.Size = new System.Drawing.Size(55, 17);
			this.Label1.TabIndex = 0;
			this.Label1.Text = "Страна";
			// 
			// Label2
			// 
			this.Label2.AutoSize = true;
			this.Label2.Location = new System.Drawing.Point(302, 10);
			this.Label2.Name = "Label2";
			this.Label2.Size = new System.Drawing.Size(103, 17);
			this.Label2.TabIndex = 1;
			this.Label2.Text = "Штат/Область";
			// 
			// Label3
			// 
			this.Label3.AutoSize = true;
			this.Label3.Location = new System.Drawing.Point(11, 68);
			this.Label3.Name = "Label3";
			this.Label3.Size = new System.Drawing.Size(47, 17);
			this.Label3.TabIndex = 2;
			this.Label3.Text = "Город";
			// 
			// Label4
			// 
			this.Label4.AutoSize = true;
			this.Label4.Location = new System.Drawing.Point(302, 68);
			this.Label4.Name = "Label4";
			this.Label4.Size = new System.Drawing.Size(101, 17);
			this.Label4.TabIndex = 3;
			this.Label4.Text = "Почтовый код";
			// 
			// Label5
			// 
			this.Label5.AutoSize = true;
			this.Label5.Location = new System.Drawing.Point(11, 126);
			this.Label5.Name = "Label5";
			this.Label5.Size = new System.Drawing.Size(46, 17);
			this.Label5.TabIndex = 4;
			this.Label5.Text = "Адрес";
			// 
			// edCountry
			// 
			this.edCountry.Location = new System.Drawing.Point(11, 29);
			this.edCountry.Name = "edCountry";
			this.edCountry.Size = new System.Drawing.Size(282, 24);
			this.edCountry.TabIndex = 0;
			// 
			// edState
			// 
			this.edState.Location = new System.Drawing.Point(302, 29);
			this.edState.Name = "edState";
			this.edState.Size = new System.Drawing.Size(248, 24);
			this.edState.TabIndex = 1;
			// 
			// edCity
			// 
			this.edCity.Location = new System.Drawing.Point(11, 87);
			this.edCity.Name = "edCity";
			this.edCity.Size = new System.Drawing.Size(282, 24);
			this.edCity.TabIndex = 2;
			// 
			// edPostalCode
			// 
			this.edPostalCode.Location = new System.Drawing.Point(302, 87);
			this.edPostalCode.Name = "edPostalCode";
			this.edPostalCode.Size = new System.Drawing.Size(248, 24);
			this.edPostalCode.TabIndex = 3;
			// 
			// edAddress
			// 
			this.edAddress.Location = new System.Drawing.Point(11, 146);
			this.edAddress.Name = "edAddress";
			this.edAddress.Size = new System.Drawing.Size(539, 24);
			this.edAddress.TabIndex = 4;
			// 
			// SheetPhones
			// 
			this.SheetPhones.Location = new System.Drawing.Point(4, 26);
			this.SheetPhones.Name = "SheetPhones";
			this.SheetPhones.Size = new System.Drawing.Size(564, 291);
			this.SheetPhones.TabIndex = 1;
			this.SheetPhones.Text = "Телефоны";
			// 
			// SheetEmails
			// 
			this.SheetEmails.Location = new System.Drawing.Point(4, 26);
			this.SheetEmails.Name = "SheetEmails";
			this.SheetEmails.Size = new System.Drawing.Size(564, 291);
			this.SheetEmails.TabIndex = 2;
			this.SheetEmails.Text = "Эл. почта";
			// 
			// SheetWebPages
			// 
			this.SheetWebPages.Location = new System.Drawing.Point(4, 26);
			this.SheetWebPages.Name = "SheetWebPages";
			this.SheetWebPages.Size = new System.Drawing.Size(564, 291);
			this.SheetWebPages.TabIndex = 3;
			this.SheetWebPages.Text = "Веб-страницы";
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
			this.Controls.Add(this.PageAddrData);
			this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
			this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
			this.MaximizeBox = false;
			this.MinimizeBox = false;
			this.Name = "AddressEditDlg";
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