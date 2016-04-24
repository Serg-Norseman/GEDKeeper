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
		private System.Windows.Forms.TabControl PageRecords;

		private void InitializeComponent()
		{
			this.components = new System.ComponentModel.Container();
			this.ImageList1 = new System.Windows.Forms.ImageList(this.components);
			this.PageRecords = new System.Windows.Forms.TabControl();
			this.contextMenu = new System.Windows.Forms.ContextMenuStrip(this.components);
			this.miRecordEdit = new System.Windows.Forms.ToolStripMenuItem();
			this.miRecordDelete = new System.Windows.Forms.ToolStripMenuItem();
			this.miRecordDuplicate = new System.Windows.Forms.ToolStripMenuItem();
			this.contextMenu.SuspendLayout();
			this.SuspendLayout();
			// 
			// ImageList1
			// 
			this.ImageList1.ColorDepth = System.Windows.Forms.ColorDepth.Depth8Bit;
			this.ImageList1.ImageSize = new System.Drawing.Size(16, 16);
			this.ImageList1.TransparentColor = System.Drawing.Color.Transparent;
			// 
			// PageRecords
			// 
			this.PageRecords.Dock = System.Windows.Forms.DockStyle.Fill;
			this.PageRecords.Location = new System.Drawing.Point(0, 0);
			this.PageRecords.Name = "PageRecords";
			this.PageRecords.SelectedIndex = 0;
			this.PageRecords.Size = new System.Drawing.Size(762, 290);
			this.PageRecords.TabIndex = 0;
			this.PageRecords.SelectedIndexChanged += new System.EventHandler(this.PageRecords_SelectedIndexChanged);
			// 
			// contextMenu
			// 
			this.contextMenu.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
									this.miRecordEdit,
									this.miRecordDelete,
									this.miRecordDuplicate});
			this.contextMenu.Name = "contextMenu";
			this.contextMenu.Size = new System.Drawing.Size(212, 98);
			this.contextMenu.Opening += new System.ComponentModel.CancelEventHandler(this.contextMenu_Opening);
			// 
			// miRecordEdit
			// 
			this.miRecordEdit.Name = "miRecordEdit";
			this.miRecordEdit.Size = new System.Drawing.Size(211, 24);
			this.miRecordEdit.Text = "toolStripMenuItem1";
			this.miRecordEdit.Click += new System.EventHandler(this.miRecordEdit_Click);
			// 
			// miRecordDelete
			// 
			this.miRecordDelete.Name = "miRecordDelete";
			this.miRecordDelete.Size = new System.Drawing.Size(211, 24);
			this.miRecordDelete.Text = "toolStripMenuItem1";
			this.miRecordDelete.Click += new System.EventHandler(this.miRecordDelete_Click);
			// 
			// miRecordDuplicate
			// 
			this.miRecordDuplicate.Name = "miRecordDuplicate";
			this.miRecordDuplicate.Size = new System.Drawing.Size(211, 24);
			this.miRecordDuplicate.Text = "toolStripMenuItem1";
			this.miRecordDuplicate.Click += new System.EventHandler(this.miRecordDuplicate_Click);
			// 
			// TfmBase
			// 
			this.AutoScaleBaseSize = new System.Drawing.Size(7, 17);
			this.ClientSize = new System.Drawing.Size(762, 290);
			this.Controls.Add(this.PageRecords);
			this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
			this.KeyPreview = true;
			this.Name = "TfmBase";
			this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
			this.Text = "GEDKeeper2";
			this.Activated += new System.EventHandler(this.Form_Activated);
			this.Closing += new System.ComponentModel.CancelEventHandler(this.Form_Closing);
			this.Deactivate += new System.EventHandler(this.Form_Deactivate);
			this.Load += new System.EventHandler(this.Form_Load);
			this.KeyDown += new System.Windows.Forms.KeyEventHandler(this.Form_KeyDown);
			this.contextMenu.ResumeLayout(false);
			this.ResumeLayout(false);
		}
		private System.Windows.Forms.ToolStripMenuItem miRecordDuplicate;
		private System.Windows.Forms.ToolStripMenuItem miRecordDelete;
		private System.Windows.Forms.ToolStripMenuItem miRecordEdit;
		private System.Windows.Forms.ContextMenuStrip contextMenu;
	}
}