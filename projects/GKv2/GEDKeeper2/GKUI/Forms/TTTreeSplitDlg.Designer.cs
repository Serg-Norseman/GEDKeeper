namespace GKUI.Forms
{
    partial class TTTreeSplitDlg
    {
        private GKUI.Components.GKTabControl tabsTools;
        private System.Windows.Forms.Button btnClose;
        private System.Windows.Forms.TabPage pageTreeSplit;
        private System.Windows.Forms.Button btnSelectAll;
        private GKUI.Components.GKListView ListSelected;
        private GKUI.Components.GKListView ListSkipped;
        private System.Windows.Forms.Button btnSelectFamily;
        private System.Windows.Forms.Button btnSelectAncestors;
        private System.Windows.Forms.Button btnSelectDescendants;
        private System.Windows.Forms.Button btnDelete;
        private System.Windows.Forms.Button btnSave;
        private System.Windows.Forms.Button btnSelectList;

        private void InitializeComponent()
        {
            this.tabsTools = new GKUI.Components.GKTabControl();
            this.pageTreeSplit = new System.Windows.Forms.TabPage();
            this.btnSelectAll = new System.Windows.Forms.Button();
            this.ListSelected = new GKUI.Components.GKListView();
            this.ListSkipped = new GKUI.Components.GKListView();
            this.btnSelectFamily = new System.Windows.Forms.Button();
            this.btnSelectAncestors = new System.Windows.Forms.Button();
            this.btnSelectDescendants = new System.Windows.Forms.Button();
            this.btnDelete = new System.Windows.Forms.Button();
            this.btnSave = new System.Windows.Forms.Button();
            this.btnClose = new System.Windows.Forms.Button();
            this.btnSelectList = new System.Windows.Forms.Button();
            this.tabsTools.SuspendLayout();
            this.pageTreeSplit.SuspendLayout();
            this.SuspendLayout();
            // 
            // tabsTools
            // 
            this.tabsTools.Controls.Add(this.pageTreeSplit);
            this.tabsTools.Location = new System.Drawing.Point(11, 10);
            this.tabsTools.Name = "tabsTools";
            this.tabsTools.SelectedIndex = 0;
            this.tabsTools.Size = new System.Drawing.Size(1010, 545);
            this.tabsTools.TabIndex = 0;
            // 
            // pageTreeSplit
            // 
            this.pageTreeSplit.Controls.Add(this.btnSelectAll);
            this.pageTreeSplit.Controls.Add(this.ListSelected);
            this.pageTreeSplit.Controls.Add(this.ListSkipped);
            this.pageTreeSplit.Controls.Add(this.btnSelectFamily);
            this.pageTreeSplit.Controls.Add(this.btnSelectAncestors);
            this.pageTreeSplit.Controls.Add(this.btnSelectDescendants);
            this.pageTreeSplit.Controls.Add(this.btnDelete);
            this.pageTreeSplit.Controls.Add(this.btnSave);
            this.pageTreeSplit.Controls.Add(this.btnSelectList);
            this.pageTreeSplit.Location = new System.Drawing.Point(4, 26);
            this.pageTreeSplit.Name = "pageTreeSplit";
            this.pageTreeSplit.Size = new System.Drawing.Size(1002, 515);
            this.pageTreeSplit.TabIndex = 2;
            this.pageTreeSplit.Text = "pageTreeSplit";
            // 
            // btnSelectAll
            // 
            this.btnSelectAll.Location = new System.Drawing.Point(11, 427);
            this.btnSelectAll.Name = "btnSelectAll";
            this.btnSelectAll.Size = new System.Drawing.Size(168, 31);
            this.btnSelectAll.TabIndex = 0;
            this.btnSelectAll.Text = "btnSelectAll";
            this.btnSelectAll.Click += new System.EventHandler(this.btnSelectAll_Click);
            // 
            // ListSelected
            // 
            this.ListSelected.Location = new System.Drawing.Point(11, 10);
            this.ListSelected.Name = "ListSelected";
            this.ListSelected.Size = new System.Drawing.Size(483, 395);
            this.ListSelected.View = System.Windows.Forms.View.Details;
            this.ListSelected.TabIndex = 1;
            // 
            // ListSkipped
            // 
            this.ListSkipped.Location = new System.Drawing.Point(504, 10);
            this.ListSkipped.Name = "ListSkipped";
            this.ListSkipped.Size = new System.Drawing.Size(483, 395);
            this.ListSkipped.View = System.Windows.Forms.View.Details;
            this.ListSkipped.TabIndex = 2;
            // 
            // btnSelectFamily
            // 
            this.btnSelectFamily.Location = new System.Drawing.Point(190, 427);
            this.btnSelectFamily.Name = "btnSelectFamily";
            this.btnSelectFamily.Size = new System.Drawing.Size(168, 31);
            this.btnSelectFamily.TabIndex = 3;
            this.btnSelectFamily.Text = "btnSelectFamily";
            this.btnSelectFamily.Click += new System.EventHandler(this.btnSelectFamily_Click);
            // 
            // btnSelectAncestors
            // 
            this.btnSelectAncestors.Location = new System.Drawing.Point(11, 466);
            this.btnSelectAncestors.Name = "btnSelectAncestors";
            this.btnSelectAncestors.Size = new System.Drawing.Size(168, 31);
            this.btnSelectAncestors.TabIndex = 4;
            this.btnSelectAncestors.Text = "btnSelectAncestors";
            this.btnSelectAncestors.Click += new System.EventHandler(this.btnSelectAncestors_Click);
            // 
            // btnSelectDescendants
            // 
            this.btnSelectDescendants.Location = new System.Drawing.Point(190, 466);
            this.btnSelectDescendants.Name = "btnSelectDescendants";
            this.btnSelectDescendants.Size = new System.Drawing.Size(168, 31);
            this.btnSelectDescendants.TabIndex = 5;
            this.btnSelectDescendants.Text = "btnSelectDescendants";
            this.btnSelectDescendants.Click += new System.EventHandler(this.btnSelectDescendants_Click);
            // 
            // btnSelectList
            // 
            this.btnSelectList.Location = new System.Drawing.Point(391, 466);
            this.btnSelectList.Name = "btnSelectList";
            this.btnSelectList.Size = new System.Drawing.Size(168, 31);
            this.btnSelectList.TabIndex = 5;
            this.btnSelectList.Text = "btnSelectList";
            this.btnSelectList.Click += new System.EventHandler(this.btnSelectList_Click);
            // 
            // btnDelete
            // 
            this.btnDelete.Location = new System.Drawing.Point(840, 427);
            this.btnDelete.Name = "btnDelete";
            this.btnDelete.Size = new System.Drawing.Size(147, 31);
            this.btnDelete.TabIndex = 6;
            this.btnDelete.Text = "btnDelete";
            this.btnDelete.Click += new System.EventHandler(this.btnDelete_Click);
            // 
            // btnSave
            // 
            this.btnSave.Location = new System.Drawing.Point(840, 466);
            this.btnSave.Name = "btnSave";
            this.btnSave.Size = new System.Drawing.Size(147, 31);
            this.btnSave.TabIndex = 7;
            this.btnSave.Text = "btnSave";
            this.btnSave.Click += new System.EventHandler(this.btnSave_Click);
            // 
            // btnClose
            // 
            this.btnClose.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.btnClose.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.btnClose.Location = new System.Drawing.Point(907, 583);
            this.btnClose.Name = "btnClose";
            this.btnClose.Size = new System.Drawing.Size(114, 30);
            this.btnClose.TabIndex = 1;
            this.btnClose.Text = "btnClose";
            this.btnClose.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            // 
            // TTTreeSplitDlg
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(120F, 120F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
            this.CancelButton = this.btnClose;
            this.ClientSize = new System.Drawing.Size(1034, 625);
            this.Controls.Add(this.tabsTools);
            this.Controls.Add(this.btnClose);
            this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
            this.KeyPreview = true;
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.Name = "TTTreeSplitDlg";
            this.ShowInTaskbar = false;
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
            this.Text = "TreeToolsWin";
            this.tabsTools.ResumeLayout(false);
            this.pageTreeSplit.ResumeLayout(false);
            this.ResumeLayout(false);

        }
    }
}
