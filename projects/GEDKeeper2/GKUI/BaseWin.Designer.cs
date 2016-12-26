using System;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;

namespace GKUI
{
    partial class BaseWin
    {
        private System.ComponentModel.IContainer components;
        private System.Windows.Forms.TabControl tabsRecords;
        private System.Windows.Forms.ToolStripMenuItem miRecordDuplicate;
        private System.Windows.Forms.ToolStripMenuItem miRecordDelete;
        private System.Windows.Forms.ToolStripMenuItem miRecordEdit;
        private System.Windows.Forms.ContextMenuStrip contextMenu;
        private System.Windows.Forms.ToolStripMenuItem miRecordAdd;

        private void InitializeComponent()
        {
            this.components = new System.ComponentModel.Container();
            this.tabsRecords = new System.Windows.Forms.TabControl();
            this.contextMenu = new System.Windows.Forms.ContextMenuStrip(this.components);
            this.miRecordAdd = new System.Windows.Forms.ToolStripMenuItem();
            this.miRecordEdit = new System.Windows.Forms.ToolStripMenuItem();
            this.miRecordDelete = new System.Windows.Forms.ToolStripMenuItem();
            this.miRecordDuplicate = new System.Windows.Forms.ToolStripMenuItem();
            this.contextMenu.SuspendLayout();
            this.SuspendLayout();
            // 
            // tabsRecords
            // 
            this.tabsRecords.Dock = System.Windows.Forms.DockStyle.Fill;
            this.tabsRecords.Location = new System.Drawing.Point(0, 0);
            this.tabsRecords.Margin = new System.Windows.Forms.Padding(2);
            this.tabsRecords.Name = "tabsRecords";
            this.tabsRecords.SelectedIndex = 0;
            this.tabsRecords.Size = new System.Drawing.Size(781, 370);
            this.tabsRecords.TabIndex = 0;
            this.tabsRecords.SelectedIndexChanged += new System.EventHandler(this.PageRecords_SelectedIndexChanged);
            // 
            // contextMenu
            // 
            this.contextMenu.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
                                                this.miRecordAdd,
                                                this.miRecordEdit,
                                                this.miRecordDelete,
                                                this.miRecordDuplicate});
            this.contextMenu.Name = "contextMenu";
            this.contextMenu.Size = new System.Drawing.Size(176, 92);
            this.contextMenu.Opening += new System.ComponentModel.CancelEventHandler(this.contextMenu_Opening);
            // 
            // miRecordAdd
            // 
            this.miRecordAdd.Name = "miRecordAdd";
            this.miRecordAdd.Size = new System.Drawing.Size(175, 22);
            this.miRecordAdd.Text = "miRecordAdd";
            this.miRecordAdd.Click += new System.EventHandler(this.miRecordAdd_Click);
            // 
            // miRecordEdit
            // 
            this.miRecordEdit.Name = "miRecordEdit";
            this.miRecordEdit.Size = new System.Drawing.Size(175, 22);
            this.miRecordEdit.Text = "miRecordEdit";
            this.miRecordEdit.Click += new System.EventHandler(this.miRecordEdit_Click);
            // 
            // miRecordDelete
            // 
            this.miRecordDelete.Name = "miRecordDelete";
            this.miRecordDelete.Size = new System.Drawing.Size(175, 22);
            this.miRecordDelete.Text = "miRecordDelete";
            this.miRecordDelete.Click += new System.EventHandler(this.miRecordDelete_Click);
            // 
            // miRecordDuplicate
            // 
            this.miRecordDuplicate.Name = "miRecordDuplicate";
            this.miRecordDuplicate.Size = new System.Drawing.Size(175, 22);
            this.miRecordDuplicate.Text = "miRecordDuplicate";
            this.miRecordDuplicate.Click += new System.EventHandler(this.miRecordDuplicate_Click);
            // 
            // BaseWin
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(96F, 96F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
            this.ClientSize = new System.Drawing.Size(781, 370);
            this.Controls.Add(this.tabsRecords);
            this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
            this.KeyPreview = true;
            this.Margin = new System.Windows.Forms.Padding(2);
            this.Name = "BaseWin";
            this.StartPosition = System.Windows.Forms.FormStartPosition.Manual;
            this.Text = "BaseWin";
            this.Activated += new System.EventHandler(this.Form_Activated);
            this.Closing += new System.ComponentModel.CancelEventHandler(this.Form_Closing);
            this.Deactivate += new System.EventHandler(this.Form_Deactivate);
            this.Load += new System.EventHandler(this.Form_Load);
            this.KeyDown += new System.Windows.Forms.KeyEventHandler(this.Form_KeyDown);
            this.contextMenu.ResumeLayout(false);
            this.ResumeLayout(false);
        }
    }
}
