namespace GKFoldersPlugin
{
    partial class FoldersWidget
    {

        private void InitializeComponent()
        {
            this.tabControl1 = new System.Windows.Forms.TabControl();
            this.tabFilter = new System.Windows.Forms.TabPage();
            this.btnSetFilter = new System.Windows.Forms.Button();
            this.cmbFilterFolders = new System.Windows.Forms.ComboBox();
            this.tabSet = new System.Windows.Forms.TabPage();
            this.btnSetAll = new System.Windows.Forms.Button();
            this.btnSetSelected = new System.Windows.Forms.Button();
            this.btnSetCurrent = new System.Windows.Forms.Button();
            this.cmbSelectFolder = new System.Windows.Forms.ComboBox();
            this.tabControl1.SuspendLayout();
            this.tabFilter.SuspendLayout();
            this.tabSet.SuspendLayout();
            this.SuspendLayout();
            // 
            // tabControl1
            // 
            this.tabControl1.Controls.Add(this.tabFilter);
            this.tabControl1.Controls.Add(this.tabSet);
            this.tabControl1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.tabControl1.Location = new System.Drawing.Point(0, 0);
            this.tabControl1.Name = "tabControl1";
            this.tabControl1.SelectedIndex = 0;
            this.tabControl1.Size = new System.Drawing.Size(623, 79);
            this.tabControl1.TabIndex = 0;
            // 
            // tabFilter
            // 
            this.tabFilter.BackColor = System.Drawing.SystemColors.Control;
            this.tabFilter.Controls.Add(this.btnSetFilter);
            this.tabFilter.Controls.Add(this.cmbFilterFolders);
            this.tabFilter.Location = new System.Drawing.Point(4, 26);
            this.tabFilter.Name = "tabFilter";
            this.tabFilter.Padding = new System.Windows.Forms.Padding(8);
            this.tabFilter.Size = new System.Drawing.Size(615, 49);
            this.tabFilter.TabIndex = 0;
            this.tabFilter.Text = "Filter";
            // 
            // btnSetFilter
            // 
            this.btnSetFilter.Location = new System.Drawing.Point(529, 11);
            this.btnSetFilter.Name = "btnSetFilter";
            this.btnSetFilter.Size = new System.Drawing.Size(75, 25);
            this.btnSetFilter.TabIndex = 1;
            this.btnSetFilter.Text = "Set filter";
            this.btnSetFilter.UseVisualStyleBackColor = true;
            this.btnSetFilter.Click += new System.EventHandler(this.btnSetFilter_Click);
            // 
            // cmbFilterFolders
            // 
            this.cmbFilterFolders.FormattingEnabled = true;
            this.cmbFilterFolders.Location = new System.Drawing.Point(11, 11);
            this.cmbFilterFolders.Name = "cmbFilterFolders";
            this.cmbFilterFolders.Size = new System.Drawing.Size(512, 25);
            this.cmbFilterFolders.TabIndex = 0;
            // 
            // tabSet
            // 
            this.tabSet.BackColor = System.Drawing.SystemColors.Control;
            this.tabSet.Controls.Add(this.btnSetAll);
            this.tabSet.Controls.Add(this.btnSetSelected);
            this.tabSet.Controls.Add(this.btnSetCurrent);
            this.tabSet.Controls.Add(this.cmbSelectFolder);
            this.tabSet.Location = new System.Drawing.Point(4, 26);
            this.tabSet.Name = "tabSet";
            this.tabSet.Padding = new System.Windows.Forms.Padding(3);
            this.tabSet.Size = new System.Drawing.Size(615, 49);
            this.tabSet.TabIndex = 1;
            this.tabSet.Text = "Set";
            // 
            // btnSetAll
            // 
            this.btnSetAll.Location = new System.Drawing.Point(532, 12);
            this.btnSetAll.Name = "btnSetAll";
            this.btnSetAll.Size = new System.Drawing.Size(75, 25);
            this.btnSetAll.TabIndex = 3;
            this.btnSetAll.Text = "SetAll";
            this.btnSetAll.UseVisualStyleBackColor = true;
            this.btnSetAll.Click += new System.EventHandler(this.btnSetAll_Click);
            // 
            // btnSetSelected
            // 
            this.btnSetSelected.Enabled = false;
            this.btnSetSelected.Location = new System.Drawing.Point(451, 12);
            this.btnSetSelected.Name = "btnSetSelected";
            this.btnSetSelected.Size = new System.Drawing.Size(75, 25);
            this.btnSetSelected.TabIndex = 3;
            this.btnSetSelected.Text = "SetSelected";
            this.btnSetSelected.UseVisualStyleBackColor = true;
            this.btnSetSelected.Click += new System.EventHandler(this.btnSetSelected_Click);
            // 
            // btnSetCurrent
            // 
            this.btnSetCurrent.Location = new System.Drawing.Point(370, 12);
            this.btnSetCurrent.Name = "btnSetCurrent";
            this.btnSetCurrent.Size = new System.Drawing.Size(75, 25);
            this.btnSetCurrent.TabIndex = 3;
            this.btnSetCurrent.Text = "SetCurrent";
            this.btnSetCurrent.UseVisualStyleBackColor = true;
            this.btnSetCurrent.Click += new System.EventHandler(this.btnSetCurrent_Click);
            // 
            // cmbSelectFolder
            // 
            this.cmbSelectFolder.FormattingEnabled = true;
            this.cmbSelectFolder.Location = new System.Drawing.Point(11, 12);
            this.cmbSelectFolder.Name = "cmbSelectFolder";
            this.cmbSelectFolder.Size = new System.Drawing.Size(353, 25);
            this.cmbSelectFolder.TabIndex = 2;
            // 
            // FoldersWidget
            // 
            this.AutoScaleBaseSize = new System.Drawing.Size(7, 17);
            this.ClientSize = new System.Drawing.Size(623, 79);
            this.Controls.Add(this.tabControl1);
            this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedToolWindow;
            this.Name = "FoldersWidget";
            this.ShowInTaskbar = false;
            this.StartPosition = System.Windows.Forms.FormStartPosition.Manual;
            this.Text = "FoldersWidget";
            this.TopMost = true;
            this.Closed += new System.EventHandler(this.FoldersWidget_Closed);
            this.Load += new System.EventHandler(this.FoldersWidget_Load);
            this.tabControl1.ResumeLayout(false);
            this.tabFilter.ResumeLayout(false);
            this.tabSet.ResumeLayout(false);
            this.ResumeLayout(false);

        }
        private System.Windows.Forms.TabControl tabControl1;
        private System.Windows.Forms.TabPage tabFilter;
        private System.Windows.Forms.TabPage tabSet;
        private System.Windows.Forms.ComboBox cmbFilterFolders;
        private System.Windows.Forms.Button btnSetFilter;
        private System.Windows.Forms.Button btnSetAll;
        private System.Windows.Forms.Button btnSetSelected;
        private System.Windows.Forms.Button btnSetCurrent;
        private System.Windows.Forms.ComboBox cmbSelectFolder;
    }
}