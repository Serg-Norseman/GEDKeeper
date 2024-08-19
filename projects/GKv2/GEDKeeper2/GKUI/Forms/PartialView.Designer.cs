namespace GKUI.Forms
{
    partial class PartialView
    {
        private System.ComponentModel.IContainer components;
        private System.Windows.Forms.StatusStrip StatusBar;
        private System.Windows.Forms.ToolStrip ToolBar1;
        private System.Windows.Forms.ToolStripButton tbFileSave;
        private System.Windows.Forms.ToolStripSeparator TBS1;
        private System.Windows.Forms.ToolStripButton tbRecordAdd;
        private System.Windows.Forms.ToolStripButton tbRecordEdit;
        private System.Windows.Forms.ToolStripButton tbRecordDelete;
        private System.Windows.Forms.ToolStripSeparator TBS2;
        private System.Windows.Forms.ToolStripButton tbFilter;
        private System.Windows.Forms.ToolStripButton tbTreeAncestors;
        private System.Windows.Forms.ToolStripButton tbTreeDescendants;
        private System.Windows.Forms.ToolStripSeparator TBS4;
        private System.Windows.Forms.ToolStripButton tbPrev;
        private System.Windows.Forms.ToolStripButton tbNext;
        private System.Windows.Forms.ToolStripSeparator TBS7;
        private System.Windows.Forms.ToolStripButton tbTreeBoth;
        private System.Windows.Forms.ToolStripStatusLabel StatusBarPanel1;
        private System.Windows.Forms.ToolStripMenuItem miContRecordDuplicate;
        private System.Windows.Forms.ToolStripMenuItem miContRecordMerge;
        private System.Windows.Forms.ToolStripMenuItem miContRecordDelete;
        private System.Windows.Forms.ToolStripMenuItem miContRecordEdit;
        private System.Windows.Forms.ContextMenuStrip contextMenu;
        private System.Windows.Forms.ToolStripMenuItem miContRecordAdd;
        private System.Windows.Forms.ContextMenuStrip summaryMenu;
        private System.Windows.Forms.ToolStripMenuItem miCopyContent;

        private void InitializeComponent()
        {
            this.components = new System.ComponentModel.Container();
            this.StatusBar = new System.Windows.Forms.StatusStrip();
            this.StatusBarPanel1 = new System.Windows.Forms.ToolStripStatusLabel();
            this.ToolBar1 = new System.Windows.Forms.ToolStrip();
            this.tbFileSave = new System.Windows.Forms.ToolStripButton();
            this.TBS1 = new System.Windows.Forms.ToolStripSeparator();
            this.tbRecordAdd = new System.Windows.Forms.ToolStripButton();
            this.tbRecordEdit = new System.Windows.Forms.ToolStripButton();
            this.tbRecordDelete = new System.Windows.Forms.ToolStripButton();
            this.TBS2 = new System.Windows.Forms.ToolStripSeparator();
            this.tbFilter = new System.Windows.Forms.ToolStripButton();
            this.TBS4 = new System.Windows.Forms.ToolStripSeparator();
            this.tbTreeAncestors = new System.Windows.Forms.ToolStripButton();
            this.tbTreeDescendants = new System.Windows.Forms.ToolStripButton();
            this.tbTreeBoth = new System.Windows.Forms.ToolStripButton();
            this.TBS7 = new System.Windows.Forms.ToolStripSeparator();
            this.tbPrev = new System.Windows.Forms.ToolStripButton();
            this.tbNext = new System.Windows.Forms.ToolStripButton();
            this.contextMenu = new System.Windows.Forms.ContextMenuStrip(this.components);
            this.miContRecordAdd = new System.Windows.Forms.ToolStripMenuItem();
            this.miContRecordEdit = new System.Windows.Forms.ToolStripMenuItem();
            this.miContRecordDelete = new System.Windows.Forms.ToolStripMenuItem();
            this.miContRecordDuplicate = new System.Windows.Forms.ToolStripMenuItem();
            this.miContRecordMerge = new System.Windows.Forms.ToolStripMenuItem();
            this.summaryMenu = new System.Windows.Forms.ContextMenuStrip(this.components);
            this.miCopyContent = new System.Windows.Forms.ToolStripMenuItem();
            this.StatusBar.SuspendLayout();
            this.ToolBar1.SuspendLayout();
            this.contextMenu.SuspendLayout();
            this.summaryMenu.SuspendLayout();
            this.SuspendLayout();
            // 
            // StatusBar
            // 
            this.StatusBar.ImageScalingSize = new System.Drawing.Size(20, 20);
            this.StatusBar.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.StatusBarPanel1});
            this.StatusBar.Location = new System.Drawing.Point(0, 485);
            this.StatusBar.Name = "StatusBar";
            this.StatusBar.Padding = new System.Windows.Forms.Padding(1, 0, 19, 0);
            this.StatusBar.Size = new System.Drawing.Size(704, 29);
            this.StatusBar.TabIndex = 0;
            this.StatusBar.Text = "StatusBar";
            // 
            // StatusBarPanel1
            // 
            this.StatusBarPanel1.BorderSides = ((System.Windows.Forms.ToolStripStatusLabelBorderSides)((((System.Windows.Forms.ToolStripStatusLabelBorderSides.Left | System.Windows.Forms.ToolStripStatusLabelBorderSides.Top)
            | System.Windows.Forms.ToolStripStatusLabelBorderSides.Right)
            | System.Windows.Forms.ToolStripStatusLabelBorderSides.Bottom)));
            this.StatusBarPanel1.BorderStyle = System.Windows.Forms.Border3DStyle.Sunken;
            this.StatusBarPanel1.Name = "StatusBarPanel1";
            this.StatusBarPanel1.Size = new System.Drawing.Size(644, 24);
            this.StatusBarPanel1.Spring = true;
            this.StatusBarPanel1.Text = "StatusBarPanel1";
            this.StatusBarPanel1.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
            // 
            // ToolBar1
            // 
            this.ToolBar1.GripStyle = System.Windows.Forms.ToolStripGripStyle.Hidden;
            this.ToolBar1.ImageScalingSize = new System.Drawing.Size(20, 20);
            this.ToolBar1.ImeMode = System.Windows.Forms.ImeMode.NoControl;
            this.ToolBar1.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
                                    this.tbFileSave,
                                    this.TBS1,
                                    this.tbRecordAdd,
                                    this.tbRecordEdit,
                                    this.tbRecordDelete,
                                    this.TBS2,
                                    this.tbFilter,
                                    this.TBS4,
                                    this.tbTreeAncestors,
                                    this.tbTreeDescendants,
                                    this.tbTreeBoth,
                                    this.TBS7,
                                    this.tbPrev,
                                    this.tbNext});
            this.ToolBar1.Location = new System.Drawing.Point(0, 28);
            this.ToolBar1.Name = "ToolBar1";
            this.ToolBar1.Size = new System.Drawing.Size(976, 25);
            this.ToolBar1.TabIndex = 0;
            // 
            // tbFileSave
            // 
            this.tbFileSave.Name = "tbFileSave";
            this.tbFileSave.Size = new System.Drawing.Size(23, 22);
            this.tbFileSave.Click += new System.EventHandler(this.miFileSave_Click);
            // 
            // TBS1
            // 
            this.TBS1.Name = "TBS1";
            this.TBS1.Size = new System.Drawing.Size(6, 25);
            // 
            // tbRecordAdd
            // 
            this.tbRecordAdd.Name = "tbRecordAdd";
            this.tbRecordAdd.Size = new System.Drawing.Size(23, 22);
            this.tbRecordAdd.Click += new System.EventHandler(this.miRecordAdd_Click);
            // 
            // tbRecordEdit
            // 
            this.tbRecordEdit.Name = "tbRecordEdit";
            this.tbRecordEdit.Size = new System.Drawing.Size(23, 22);
            this.tbRecordEdit.Click += new System.EventHandler(this.miRecordEdit_Click);
            // 
            // tbRecordDelete
            // 
            this.tbRecordDelete.Name = "tbRecordDelete";
            this.tbRecordDelete.Size = new System.Drawing.Size(23, 22);
            this.tbRecordDelete.Click += new System.EventHandler(this.miRecordDelete_Click);
            // 
            // TBS2
            // 
            this.TBS2.Name = "TBS2";
            this.TBS2.Size = new System.Drawing.Size(6, 25);
            // 
            // tbFilter
            // 
            this.tbFilter.ImageTransparentColor = System.Drawing.Color.White;
            this.tbFilter.Name = "tbFilter";
            this.tbFilter.Size = new System.Drawing.Size(23, 22);
            this.tbFilter.Click += new System.EventHandler(this.miFilter_Click);
            // 
            // TBS4
            // 
            this.TBS4.Name = "TBS4";
            this.TBS4.Size = new System.Drawing.Size(6, 25);
            // 
            // tbTreeAncestors
            // 
            this.tbTreeAncestors.Name = "tbTreeAncestors";
            this.tbTreeAncestors.Size = new System.Drawing.Size(23, 22);
            this.tbTreeAncestors.Click += new System.EventHandler(this.miTreeAncestors_Click);
            // 
            // tbTreeDescendants
            // 
            this.tbTreeDescendants.Name = "tbTreeDescendants";
            this.tbTreeDescendants.Size = new System.Drawing.Size(23, 22);
            this.tbTreeDescendants.Click += new System.EventHandler(this.miTreeDescendants_Click);
            // 
            // tbTreeBoth
            // 
            this.tbTreeBoth.Name = "tbTreeBoth";
            this.tbTreeBoth.Size = new System.Drawing.Size(23, 22);
            this.tbTreeBoth.Click += new System.EventHandler(this.miTreeBoth_Click);
            // 
            // TBS7
            // 
            this.TBS7.Name = "TBS7";
            this.TBS7.Size = new System.Drawing.Size(6, 25);
            // 
            // tbPrev
            // 
            this.tbPrev.Enabled = false;
            this.tbPrev.Name = "tbPrev";
            this.tbPrev.Size = new System.Drawing.Size(23, 22);
            this.tbPrev.Click += new System.EventHandler(this.tbPrev_Click);
            // 
            // tbNext
            // 
            this.tbNext.Enabled = false;
            this.tbNext.Name = "tbNext";
            this.tbNext.Size = new System.Drawing.Size(23, 22);
            this.tbNext.Click += new System.EventHandler(this.tbNext_Click);
            // 
            // contextMenu
            // 
            this.contextMenu.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
                                    this.miContRecordAdd,
                                    this.miContRecordEdit,
                                    this.miContRecordDelete,
                                    this.miContRecordDuplicate,
                                    this.miContRecordMerge,});
            this.contextMenu.Name = "contextMenu";
            this.contextMenu.Size = new System.Drawing.Size(218, 100);
            this.contextMenu.Opening += new System.ComponentModel.CancelEventHandler(this.contextMenu_Opening);
            // 
            // miContRecordAdd
            // 
            this.miContRecordAdd.Name = "miContRecordAdd";
            this.miContRecordAdd.Size = new System.Drawing.Size(217, 24);
            this.miContRecordAdd.Text = "miContRecordAdd";
            this.miContRecordAdd.Click += new System.EventHandler(this.miRecordAdd_Click);
            // 
            // miContRecordEdit
            // 
            this.miContRecordEdit.Name = "miContRecordEdit";
            this.miContRecordEdit.Size = new System.Drawing.Size(217, 24);
            this.miContRecordEdit.Text = "miContRecordEdit";
            this.miContRecordEdit.Click += new System.EventHandler(this.miRecordEdit_Click);
            // 
            // miContRecordDelete
            // 
            this.miContRecordDelete.Name = "miContRecordDelete";
            this.miContRecordDelete.Size = new System.Drawing.Size(217, 24);
            this.miContRecordDelete.Text = "miContRecordDelete";
            this.miContRecordDelete.Click += new System.EventHandler(this.miRecordDelete_Click);
            // 
            // miRecordDuplicate
            // 
            this.miContRecordDuplicate.Name = "miContRecordDuplicate";
            this.miContRecordDuplicate.Size = new System.Drawing.Size(217, 24);
            this.miContRecordDuplicate.Text = "miContRecordDuplicate";
            this.miContRecordDuplicate.Click += new System.EventHandler(this.miRecordDuplicate_Click);
            // 
            // miRecordDuplicate
            // 
            this.miContRecordMerge.Name = "miContRecordMerge";
            this.miContRecordMerge.Size = new System.Drawing.Size(217, 24);
            this.miContRecordMerge.Text = "miContRecordMerge";
            this.miContRecordMerge.Click += new System.EventHandler(this.miRecordMerge_Click);
            // 
            // summaryMenu
            // 
            this.summaryMenu.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
                                    this.miCopyContent});
            this.summaryMenu.Name = "summaryMenu";
            this.summaryMenu.Size = new System.Drawing.Size(218, 100);
            // 
            // miCopyContent
            // 
            this.miCopyContent.Name = "miCopyContent";
            this.miCopyContent.Size = new System.Drawing.Size(217, 24);
            this.miCopyContent.Text = "miCopyContent";
            this.miCopyContent.Click += new System.EventHandler(this.miCopyContent_Click);
            // 
            // PartialView
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(120F, 120F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
            this.ClientSize = new System.Drawing.Size(976, 462);
            this.Controls.Add(this.StatusBar);
            this.Controls.Add(this.ToolBar1);
            this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
            this.KeyPreview = true;
            this.Margin = new System.Windows.Forms.Padding(2);
            this.Name = "PartialView";
            this.StartPosition = System.Windows.Forms.FormStartPosition.Manual;
            this.Text = "PartialView";
            this.FormClosing += new System.Windows.Forms.FormClosingEventHandler(this.Form_Closing);
            this.FormClosed += new System.Windows.Forms.FormClosedEventHandler(this.Form_Closed);
            this.Load += new System.EventHandler(this.Form_Load);
            this.KeyDown += new System.Windows.Forms.KeyEventHandler(this.Form_KeyDown);
            this.StatusBar.ResumeLayout(false);
            this.StatusBar.PerformLayout();
            this.ToolBar1.ResumeLayout(false);
            this.ToolBar1.PerformLayout();
            this.contextMenu.ResumeLayout(false);
            this.summaryMenu.ResumeLayout(false);
            this.ResumeLayout(false);
            this.PerformLayout();
        }
    }
}
