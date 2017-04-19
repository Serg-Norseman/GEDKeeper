using System;
using System.Drawing;
using System.Windows.Forms;

namespace GKUI.Dialogs
{
	partial class PortraitSelectDlg
	{
		private System.Windows.Forms.Button btnAccept;
		private System.Windows.Forms.Button btnCancel;
		private GKUI.Components.ImageView imageView1;
		private System.Windows.Forms.Panel panel1;

		private void InitializeComponent()
		{
		    System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(PortraitSelectDlg));
		    this.panel1 = new System.Windows.Forms.Panel();
		    this.btnAccept = new System.Windows.Forms.Button();
		    this.btnCancel = new System.Windows.Forms.Button();
		    this.imageView1 = new GKUI.Components.ImageView();
		    this.panel1.SuspendLayout();
		    this.SuspendLayout();
		    // 
		    // panel1
		    // 
		    this.panel1.Controls.Add(this.btnAccept);
		    this.panel1.Controls.Add(this.btnCancel);
		    this.panel1.Dock = System.Windows.Forms.DockStyle.Bottom;
		    this.panel1.Location = new System.Drawing.Point(0, 623);
		    this.panel1.Name = "panel1";
		    this.panel1.Padding = new System.Windows.Forms.Padding(10);
		    this.panel1.Size = new System.Drawing.Size(871, 58);
		    this.panel1.TabIndex = 3;
		    // 
		    // btnAccept
		    // 
		    this.btnAccept.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
		    this.btnAccept.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
		    this.btnAccept.Location = new System.Drawing.Point(646, 14);
		    this.btnAccept.Margin = new System.Windows.Forms.Padding(4);
		    this.btnAccept.Name = "btnAccept";
		    this.btnAccept.Size = new System.Drawing.Size(101, 31);
		    this.btnAccept.TabIndex = 3;
		    this.btnAccept.Text = "btnAccept";
		    this.btnAccept.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
		    this.btnAccept.Click += new System.EventHandler(this.btnAccept_Click);
		    // 
		    // btnCancel
		    // 
		    this.btnCancel.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
		    this.btnCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
		    this.btnCancel.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
		    this.btnCancel.Location = new System.Drawing.Point(756, 14);
		    this.btnCancel.Margin = new System.Windows.Forms.Padding(4);
		    this.btnCancel.Name = "btnCancel";
		    this.btnCancel.Size = new System.Drawing.Size(101, 31);
		    this.btnCancel.TabIndex = 4;
		    this.btnCancel.Text = "btnCancel";
		    this.btnCancel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
		    // 
		    // imageView1
		    // 
		    this.imageView1.Dock = System.Windows.Forms.DockStyle.Fill;
		    this.imageView1.Location = new System.Drawing.Point(0, 0);
		    this.imageView1.Margin = new System.Windows.Forms.Padding(4);
		    this.imageView1.Name = "imageView1";
		    this.imageView1.SelectionMode = GKUI.Components.ImageBoxSelectionMode.Zoom;
		    this.imageView1.SelectionRegion = ((System.Drawing.RectangleF)(resources.GetObject("imageView1.SelectionRegion")));
		    this.imageView1.ShowToolbar = true;
		    this.imageView1.Size = new System.Drawing.Size(871, 623);
		    this.imageView1.TabIndex = 4;
		    // 
		    // PortraitSelectDlg
		    // 
		    this.AcceptButton = this.btnAccept;
		    this.AutoScaleDimensions = new System.Drawing.SizeF(120F, 120F);
		    this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
		    this.CancelButton = this.btnCancel;
		    this.ClientSize = new System.Drawing.Size(871, 681);
		    this.Controls.Add(this.imageView1);
		    this.Controls.Add(this.panel1);
		    this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
		    this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
		    this.Margin = new System.Windows.Forms.Padding(4);
		    this.MaximizeBox = false;
		    this.MinimizeBox = false;
		    this.Name = "PortraitSelectDlg";
		    this.ShowInTaskbar = false;
		    this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
		    this.Text = "PortraitSelectDlg";
		    this.panel1.ResumeLayout(false);
		    this.ResumeLayout(false);
		}
	}
}
