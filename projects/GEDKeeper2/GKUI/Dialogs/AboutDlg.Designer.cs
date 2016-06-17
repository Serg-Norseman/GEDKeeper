using System;
using System.Drawing;
using System.Windows.Forms;

namespace GKUI.Dialogs
{
	partial class AboutDlg
	{
		private System.Windows.Forms.Label lblProduct;
		private System.Windows.Forms.Label lblVersion;
		private System.Windows.Forms.Label lblCopyright;
		private System.Windows.Forms.Label lblMail;
		private System.Windows.Forms.Button btnClose;
		private System.Windows.Forms.Label lblProjSite;

		private void InitializeComponent()
		{
		    this.lblProduct = new System.Windows.Forms.Label();
		    this.lblVersion = new System.Windows.Forms.Label();
		    this.btnClose = new System.Windows.Forms.Button();
		    this.lblCopyright = new System.Windows.Forms.Label();
		    this.lblMail = new System.Windows.Forms.Label();
		    this.lblProjSite = new System.Windows.Forms.Label();
		    this.SuspendLayout();
		    // 
		    // lblProduct
		    // 
		    this.lblProduct.AutoSize = true;
		    this.lblProduct.Font = new System.Drawing.Font("Times New Roman", 20.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
		    this.lblProduct.Location = new System.Drawing.Point(12, 9);
		    this.lblProduct.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
		    this.lblProduct.Name = "lblProduct";
		    this.lblProduct.Size = new System.Drawing.Size(170, 38);
		    this.lblProduct.TabIndex = 0;
		    this.lblProduct.Text = "lblProduct";
		    // 
		    // lblVersion
		    // 
		    this.lblVersion.AutoSize = true;
		    this.lblVersion.Font = new System.Drawing.Font("Times New Roman", 11.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
		    this.lblVersion.Location = new System.Drawing.Point(15, 58);
		    this.lblVersion.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
		    this.lblVersion.Name = "lblVersion";
		    this.lblVersion.Size = new System.Drawing.Size(91, 22);
		    this.lblVersion.TabIndex = 1;
		    this.lblVersion.Text = "lblVersion";
		    // 
		    // btnClose
		    // 
		    this.btnClose.DialogResult = System.Windows.Forms.DialogResult.Cancel;
		    this.btnClose.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
		    this.btnClose.Location = new System.Drawing.Point(269, 235);
		    this.btnClose.Margin = new System.Windows.Forms.Padding(2);
		    this.btnClose.Name = "btnClose";
		    this.btnClose.Size = new System.Drawing.Size(114, 30);
		    this.btnClose.TabIndex = 0;
		    this.btnClose.Text = "btnClose";
		    this.btnClose.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
		    // 
		    // lblCopyright
		    // 
		    this.lblCopyright.AutoSize = true;
		    this.lblCopyright.Font = new System.Drawing.Font("Times New Roman", 11.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
		    this.lblCopyright.Location = new System.Drawing.Point(15, 91);
		    this.lblCopyright.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
		    this.lblCopyright.Name = "lblCopyright";
		    this.lblCopyright.Size = new System.Drawing.Size(112, 22);
		    this.lblCopyright.TabIndex = 1;
		    this.lblCopyright.Text = "lblCopyright";
		    // 
		    // lblMail
		    // 
		    this.lblMail.AutoSize = true;
		    this.lblMail.Cursor = System.Windows.Forms.Cursors.Hand;
		    this.lblMail.Font = new System.Drawing.Font("Times New Roman", 8.25F, ((System.Drawing.FontStyle)((System.Drawing.FontStyle.Bold | System.Drawing.FontStyle.Underline))), System.Drawing.GraphicsUnit.Point, ((byte)(204)));
		    this.lblMail.ForeColor = System.Drawing.Color.Blue;
		    this.lblMail.Location = new System.Drawing.Point(15, 155);
		    this.lblMail.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
		    this.lblMail.Name = "lblMail";
		    this.lblMail.Size = new System.Drawing.Size(159, 17);
		    this.lblMail.TabIndex = 2;
		    this.lblMail.Text = "http://vk.com/gedkeeper";
		    this.lblMail.Click += new System.EventHandler(this.LabelMail_Click);
		    // 
		    // lblProjSite
		    // 
		    this.lblProjSite.AutoSize = true;
		    this.lblProjSite.Cursor = System.Windows.Forms.Cursors.Hand;
		    this.lblProjSite.Font = new System.Drawing.Font("Times New Roman", 8.25F, ((System.Drawing.FontStyle)((System.Drawing.FontStyle.Bold | System.Drawing.FontStyle.Underline))), System.Drawing.GraphicsUnit.Point, ((byte)(204)));
		    this.lblProjSite.ForeColor = System.Drawing.Color.Blue;
		    this.lblProjSite.Location = new System.Drawing.Point(15, 188);
		    this.lblProjSite.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
		    this.lblProjSite.Name = "label1";
		    this.lblProjSite.Size = new System.Drawing.Size(177, 17);
		    this.lblProjSite.TabIndex = 2;
		    this.lblProjSite.Text = "https://gedkeeper.github.io/";
		    this.lblProjSite.Click += new System.EventHandler(this.LabelMail_Click);
		    // 
		    // AboutDlg
		    // 
		    this.AutoScaleDimensions = new System.Drawing.SizeF(120F, 120F);
		    this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
		    this.CancelButton = this.btnClose;
		    this.ClientSize = new System.Drawing.Size(394, 276);
		    this.Controls.Add(this.lblProduct);
		    this.Controls.Add(this.lblVersion);
		    this.Controls.Add(this.lblCopyright);
		    this.Controls.Add(this.lblProjSite);
		    this.Controls.Add(this.lblMail);
		    this.Controls.Add(this.btnClose);
		    this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
		    this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
		    this.Margin = new System.Windows.Forms.Padding(2);
		    this.MaximizeBox = false;
		    this.MinimizeBox = false;
		    this.Name = "AboutDlg";
		    this.ShowInTaskbar = false;
		    this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
		    this.Text = "AboutDlg";
		    this.ResumeLayout(false);
		    this.PerformLayout();
		}
	}
}