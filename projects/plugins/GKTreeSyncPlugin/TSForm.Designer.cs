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
        private GKUI.Components.GKListView lvRecords;
        private System.Windows.Forms.CheckBox chkOnlyModified;

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
            this.lvRecords = new GKUI.Components.GKListView();
            this.chkOnlyModified = new System.Windows.Forms.CheckBox();
            this.panel1.SuspendLayout();
            this.groupBox1.SuspendLayout();
            this.SuspendLayout();
            // 
            // panel1
            // 
            this.panel1.Controls.Add(this.chkOnlyModified);
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
            this.rbSyncAll.Checked = true;
            this.rbSyncAll.Location = new System.Drawing.Point(9, 19);
            this.rbSyncAll.Name = "rbSyncAll";
            this.rbSyncAll.Size = new System.Drawing.Size(36, 17);
            this.rbSyncAll.TabIndex = 0;
            this.rbSyncAll.TabStop = true;
            this.rbSyncAll.Text = "All";
            this.rbSyncAll.UseVisualStyleBackColor = true;
            this.rbSyncAll.CheckedChanged += new System.EventHandler(this.rbSyncRecords_CheckedChanged);
            // 
            // lvRecords
            // 
            this.lvRecords.Dock = System.Windows.Forms.DockStyle.Fill;
            this.lvRecords.FullRowSelect = true;
            this.lvRecords.HideSelection = false;
            this.lvRecords.ListMan = null;
            this.lvRecords.Location = new System.Drawing.Point(0, 92);
            this.lvRecords.Name = "lvRecords";
            this.lvRecords.OwnerDraw = true;
            this.lvRecords.SelectedIndex = -1;
            this.lvRecords.Size = new System.Drawing.Size(1047, 536);
            this.lvRecords.SortColumn = 0;
            this.lvRecords.SortOrder = GKCore.Design.BSDTypes.SortOrder.None;
            this.lvRecords.TabIndex = 0;
            this.lvRecords.UseCompatibleStateImageBehavior = false;
            this.lvRecords.View = System.Windows.Forms.View.Details;
            // 
            // chkOnlyModified
            // 
            this.chkOnlyModified.AutoSize = true;
            this.chkOnlyModified.CheckAlign = System.Drawing.ContentAlignment.MiddleRight;
            this.chkOnlyModified.Location = new System.Drawing.Point(381, 55);
            this.chkOnlyModified.Name = "chkOnlyModified";
            this.chkOnlyModified.Size = new System.Drawing.Size(89, 17);
            this.chkOnlyModified.TabIndex = 4;
            this.chkOnlyModified.Text = "Only modified";
            this.chkOnlyModified.UseVisualStyleBackColor = true;
            this.chkOnlyModified.CheckStateChanged += chkOnlyModified_CheckStateChanged;
            // 
            // TSForm
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(1047, 628);
            this.Controls.Add(this.lvRecords);
            this.Controls.Add(this.panel1);
            this.Name = "TSForm";
            this.Text = "TSForm";
            this.panel1.ResumeLayout(false);
            this.panel1.PerformLayout();
            this.groupBox1.ResumeLayout(false);
            this.groupBox1.PerformLayout();
            this.ResumeLayout(false);
        }
    }
}
