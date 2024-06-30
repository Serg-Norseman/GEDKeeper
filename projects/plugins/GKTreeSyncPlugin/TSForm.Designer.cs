namespace GKTreeSyncPlugin
{
    partial class TSForm
    {
        private System.ComponentModel.IContainer components = null;
        private System.Windows.Forms.Panel panel1;
        private System.Windows.Forms.GroupBox groupBox1;
        private System.Windows.Forms.Button btnSelectFile;
        private System.Windows.Forms.Label lblFile;
        private System.Windows.Forms.TextBox txtFile;
        private System.Windows.Forms.ComboBox cmbRecordTypes;
        private System.Windows.Forms.RadioButton rbSyncSelected;
        private System.Windows.Forms.RadioButton rbSyncAll;
        private System.Windows.Forms.SplitContainer splitContainer1;
        private GKUI.Components.GKListView gkListView1;
        private GKUI.Components.GKListView gkListView2;
        private System.Windows.Forms.ColumnHeader columnHeader1;
        private System.Windows.Forms.ColumnHeader columnHeader2;

        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null)) {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        private void InitializeComponent()
        {
            this.panel1 = new System.Windows.Forms.Panel();
            this.btnSelectFile = new System.Windows.Forms.Button();
            this.lblFile = new System.Windows.Forms.Label();
            this.txtFile = new System.Windows.Forms.TextBox();
            this.groupBox1 = new System.Windows.Forms.GroupBox();
            this.cmbRecordTypes = new System.Windows.Forms.ComboBox();
            this.rbSyncSelected = new System.Windows.Forms.RadioButton();
            this.rbSyncAll = new System.Windows.Forms.RadioButton();
            this.splitContainer1 = new System.Windows.Forms.SplitContainer();
            this.gkListView1 = new GKUI.Components.GKListView();
            this.columnHeader1 = ((System.Windows.Forms.ColumnHeader)(new System.Windows.Forms.ColumnHeader()));
            this.gkListView2 = new GKUI.Components.GKListView();
            this.columnHeader2 = ((System.Windows.Forms.ColumnHeader)(new System.Windows.Forms.ColumnHeader()));
            this.panel1.SuspendLayout();
            this.groupBox1.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.splitContainer1)).BeginInit();
            this.splitContainer1.Panel1.SuspendLayout();
            this.splitContainer1.Panel2.SuspendLayout();
            this.splitContainer1.SuspendLayout();
            this.SuspendLayout();
            // 
            // panel1
            // 
            this.panel1.Controls.Add(this.btnSelectFile);
            this.panel1.Controls.Add(this.lblFile);
            this.panel1.Controls.Add(this.txtFile);
            this.panel1.Controls.Add(this.groupBox1);
            this.panel1.Dock = System.Windows.Forms.DockStyle.Top;
            this.panel1.Location = new System.Drawing.Point(0, 0);
            this.panel1.Name = "panel1";
            this.panel1.Size = new System.Drawing.Size(1047, 92);
            this.panel1.TabIndex = 0;
            // 
            // btnSelectFile
            // 
            this.btnSelectFile.Location = new System.Drawing.Point(913, 10);
            this.btnSelectFile.Name = "btnSelectFile";
            this.btnSelectFile.Size = new System.Drawing.Size(122, 23);
            this.btnSelectFile.TabIndex = 3;
            this.btnSelectFile.Text = "Select file...";
            this.btnSelectFile.UseVisualStyleBackColor = true;
            this.btnSelectFile.Click += new System.EventHandler(this.btnSelectFile_ClickAsync);
            // 
            // lblFile
            // 
            this.lblFile.AutoSize = true;
            this.lblFile.Location = new System.Drawing.Point(378, 15);
            this.lblFile.Name = "lblFile";
            this.lblFile.Size = new System.Drawing.Size(23, 13);
            this.lblFile.TabIndex = 2;
            this.lblFile.Text = "File";
            // 
            // txtFile
            // 
            this.txtFile.Location = new System.Drawing.Point(458, 12);
            this.txtFile.Name = "txtFile";
            this.txtFile.ReadOnly = true;
            this.txtFile.Size = new System.Drawing.Size(449, 20);
            this.txtFile.TabIndex = 1;
            // 
            // groupBox1
            // 
            this.groupBox1.Controls.Add(this.cmbRecordTypes);
            this.groupBox1.Controls.Add(this.rbSyncSelected);
            this.groupBox1.Controls.Add(this.rbSyncAll);
            this.groupBox1.Location = new System.Drawing.Point(12, 12);
            this.groupBox1.Name = "groupBox1";
            this.groupBox1.Size = new System.Drawing.Size(347, 70);
            this.groupBox1.TabIndex = 0;
            this.groupBox1.TabStop = false;
            this.groupBox1.Text = "Synchronize records";
            // 
            // cmbRecordTypes
            // 
            this.cmbRecordTypes.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cmbRecordTypes.FormattingEnabled = true;
            this.cmbRecordTypes.Location = new System.Drawing.Point(133, 41);
            this.cmbRecordTypes.Name = "cmbRecordTypes";
            this.cmbRecordTypes.Size = new System.Drawing.Size(208, 21);
            this.cmbRecordTypes.TabIndex = 2;
            // 
            // rbSyncSelected
            // 
            this.rbSyncSelected.AutoSize = true;
            this.rbSyncSelected.Location = new System.Drawing.Point(9, 42);
            this.rbSyncSelected.Name = "rbSyncSelected";
            this.rbSyncSelected.Size = new System.Drawing.Size(67, 17);
            this.rbSyncSelected.TabIndex = 1;
            this.rbSyncSelected.TabStop = true;
            this.rbSyncSelected.Text = "Selected";
            this.rbSyncSelected.UseVisualStyleBackColor = true;
            this.rbSyncSelected.CheckedChanged += new System.EventHandler(this.rbSyncRecords_CheckedChanged);
            // 
            // rbSyncAll
            // 
            this.rbSyncAll.AutoSize = true;
            this.rbSyncAll.Location = new System.Drawing.Point(9, 19);
            this.rbSyncAll.Name = "rbSyncAll";
            this.rbSyncAll.Size = new System.Drawing.Size(36, 17);
            this.rbSyncAll.TabIndex = 0;
            this.rbSyncAll.TabStop = true;
            this.rbSyncAll.Text = "All";
            this.rbSyncAll.UseVisualStyleBackColor = true;
            this.rbSyncAll.CheckedChanged += new System.EventHandler(this.rbSyncRecords_CheckedChanged);
            // 
            // splitContainer1
            // 
            this.splitContainer1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.splitContainer1.Location = new System.Drawing.Point(0, 92);
            this.splitContainer1.Name = "splitContainer1";
            // 
            // splitContainer1.Panel1
            // 
            this.splitContainer1.Panel1.Controls.Add(this.gkListView1);
            // 
            // splitContainer1.Panel2
            // 
            this.splitContainer1.Panel2.Controls.Add(this.gkListView2);
            this.splitContainer1.Size = new System.Drawing.Size(1047, 536);
            this.splitContainer1.SplitterDistance = 349;
            this.splitContainer1.TabIndex = 1;
            // 
            // gkListView1
            // 
            this.gkListView1.Columns.AddRange(new System.Windows.Forms.ColumnHeader[] {
            this.columnHeader1});
            this.gkListView1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.gkListView1.FullRowSelect = true;
            this.gkListView1.HideSelection = false;
            this.gkListView1.ListMan = null;
            this.gkListView1.Location = new System.Drawing.Point(0, 0);
            this.gkListView1.Name = "gkListView1";
            this.gkListView1.OwnerDraw = true;
            this.gkListView1.SelectedIndex = -1;
            this.gkListView1.Size = new System.Drawing.Size(349, 536);
            this.gkListView1.SortColumn = 0;
            this.gkListView1.SortOrder = GKCore.Design.BSDTypes.SortOrder.None;
            this.gkListView1.TabIndex = 0;
            this.gkListView1.UseCompatibleStateImageBehavior = false;
            this.gkListView1.View = System.Windows.Forms.View.Details;
            // 
            // columnHeader1
            // 
            this.columnHeader1.Text = "Record";
            this.columnHeader1.Width = 400;
            // 
            // gkListView2
            // 
            this.gkListView2.Columns.AddRange(new System.Windows.Forms.ColumnHeader[] {
            this.columnHeader2});
            this.gkListView2.Dock = System.Windows.Forms.DockStyle.Fill;
            this.gkListView2.FullRowSelect = true;
            this.gkListView2.HideSelection = false;
            this.gkListView2.ListMan = null;
            this.gkListView2.Location = new System.Drawing.Point(0, 0);
            this.gkListView2.Name = "gkListView2";
            this.gkListView2.OwnerDraw = true;
            this.gkListView2.SelectedIndex = -1;
            this.gkListView2.Size = new System.Drawing.Size(694, 536);
            this.gkListView2.SortColumn = 0;
            this.gkListView2.SortOrder = GKCore.Design.BSDTypes.SortOrder.None;
            this.gkListView2.TabIndex = 0;
            this.gkListView2.UseCompatibleStateImageBehavior = false;
            this.gkListView2.View = System.Windows.Forms.View.Details;
            // 
            // columnHeader2
            // 
            this.columnHeader2.Text = "Record";
            this.columnHeader2.Width = 400;
            // 
            // TSForm
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(1047, 628);
            this.Controls.Add(this.splitContainer1);
            this.Controls.Add(this.panel1);
            this.Name = "TSForm";
            this.Text = "TSForm";
            this.Load += new System.EventHandler(this.TSForm_Load);
            this.Resize += new System.EventHandler(this.TSForm_Resize);
            this.panel1.ResumeLayout(false);
            this.panel1.PerformLayout();
            this.groupBox1.ResumeLayout(false);
            this.groupBox1.PerformLayout();
            this.splitContainer1.Panel1.ResumeLayout(false);
            this.splitContainer1.Panel2.ResumeLayout(false);
            ((System.ComponentModel.ISupportInitialize)(this.splitContainer1)).EndInit();
            this.splitContainer1.ResumeLayout(false);
            this.ResumeLayout(false);
        }
    }
}
