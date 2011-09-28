using System;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;

namespace GKUI
{
	partial class TfmBase
	{
		private System.Windows.Forms.ImageList ImageList1;
		private System.ComponentModel.IContainer components;

		private void InitializeComponent()
		{
			this.components = new System.ComponentModel.Container();
			this.ImageList1 = new System.Windows.Forms.ImageList(this.components);
			this.PageRecords = new System.Windows.Forms.TabControl();
			this.SuspendLayout();
			this.ImageList1.ImageSize = new System.Drawing.Size(16, 16);
			this.ImageList1.TransparentColor = System.Drawing.Color.Transparent;
			this.PageRecords.Dock = System.Windows.Forms.DockStyle.Fill;
			this.PageRecords.Location = new System.Drawing.Point(0, 0);
			this.PageRecords.Name = "PageRecords";
			this.PageRecords.SelectedIndex = 0;
			this.PageRecords.Size = new System.Drawing.Size(762, 290);
			this.PageRecords.TabIndex = 0;
			this.PageRecords.SelectedIndexChanged += new System.EventHandler(this.PageRecords_SelectedIndexChanged);
			this.AutoScaleBaseSize = new System.Drawing.Size(5, 14);
			this.ClientSize = new System.Drawing.Size(762, 290);
			this.Controls.Add(this.PageRecords);
			this.Font = new System.Drawing.Font("Tahoma", 8.25f, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, 204);
			this.Name = "TfmBase";
			this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
			this.Text = "GEDKeeper2";
			this.Closing += new System.ComponentModel.CancelEventHandler(this.TfmBase_Closing);
			this.Activated += new System.EventHandler(this.FormActivate);
			this.Deactivate += new System.EventHandler(this.FormDeactivate);
			this.ResumeLayout(false);
		}
	}
}