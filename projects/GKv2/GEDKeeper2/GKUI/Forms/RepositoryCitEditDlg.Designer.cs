namespace GKUI.Forms
{
    partial class RepositoryCitEditDlg
    {
        private System.Windows.Forms.GroupBox GroupBox1;
        private System.Windows.Forms.ComboBox cmbRepository;
        private System.Windows.Forms.Label lblRepository;
        private GKUI.Components.GKTabControl tabsData;
        private System.Windows.Forms.TabPage pageCallNumbers;
        private System.Windows.Forms.Button btnAccept;
        private System.Windows.Forms.Button btnCancel;
        private System.Windows.Forms.Button btnRepositoryAdd;

        private void InitializeComponent()
        {
            this.GroupBox1 = new System.Windows.Forms.GroupBox();
            this.lblRepository = new System.Windows.Forms.Label();
            this.cmbRepository = new System.Windows.Forms.ComboBox();
            this.btnAccept = new System.Windows.Forms.Button();
            this.btnCancel = new System.Windows.Forms.Button();
            this.tabsData = new GKUI.Components.GKTabControl();
            this.pageCallNumbers = new System.Windows.Forms.TabPage();
            this.btnRepositoryAdd = new System.Windows.Forms.Button();
            this.GroupBox1.SuspendLayout();
            this.tabsData.SuspendLayout();
            this.SuspendLayout();
            // 
            // GroupBox1
            // 
            this.GroupBox1.Controls.Add(this.lblRepository);
            this.GroupBox1.Controls.Add(this.cmbRepository);
            this.GroupBox1.Controls.Add(this.btnRepositoryAdd);
            this.GroupBox1.Dock = System.Windows.Forms.DockStyle.Top;
            this.GroupBox1.Location = new System.Drawing.Point(0, 0);
            this.GroupBox1.Name = "GroupBox1";
            this.GroupBox1.Size = new System.Drawing.Size(673, 60);
            this.GroupBox1.TabIndex = 0;
            this.GroupBox1.TabStop = false;
            // 
            // lblRepository
            // 
            this.lblRepository.AutoSize = true;
            this.lblRepository.Location = new System.Drawing.Point(12, 22);
            this.lblRepository.Name = "lblRepository";
            this.lblRepository.Size = new System.Drawing.Size(67, 17);
            this.lblRepository.TabIndex = 0;
            this.lblRepository.Text = "lblRepository";
            // 
            // cmbRepository
            // 
            this.cmbRepository.Location = new System.Drawing.Point(101, 19);
            this.cmbRepository.Name = "cmbRepository";
            this.cmbRepository.Size = new System.Drawing.Size(517, 24);
            this.cmbRepository.TabIndex = 0;
            this.cmbRepository.KeyUp += new System.Windows.Forms.KeyEventHandler(this.cmbRepository_KeyUp);
            // 
            // btnRepositoryAdd
            // 
            this.btnRepositoryAdd.Location = new System.Drawing.Point(625, 19);
            this.btnRepositoryAdd.Margin = new System.Windows.Forms.Padding(4);
            this.btnRepositoryAdd.Name = "btnRepositoryAdd";
            this.btnRepositoryAdd.Size = new System.Drawing.Size(35, 35);
            this.btnRepositoryAdd.TabIndex = 2;
            this.btnRepositoryAdd.TabStop = false;
            this.btnRepositoryAdd.Click += new System.EventHandler(this.btnRepositoryAdd_Click);
            // 
            // btnAccept
            // 
            this.btnAccept.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.btnAccept.Location = new System.Drawing.Point(426, 315);
            this.btnAccept.Name = "btnAccept";
            this.btnAccept.Size = new System.Drawing.Size(113, 30);
            this.btnAccept.TabIndex = 2;
            this.btnAccept.Text = "btnAccept";
            this.btnAccept.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            this.btnAccept.Click += new System.EventHandler(this.AcceptClickHandler);
            // 
            // btnCancel
            // 
            this.btnCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.btnCancel.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.btnCancel.Location = new System.Drawing.Point(549, 315);
            this.btnCancel.Name = "btnCancel";
            this.btnCancel.Size = new System.Drawing.Size(113, 30);
            this.btnCancel.TabIndex = 3;
            this.btnCancel.Text = "btnCancel";
            this.btnCancel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            this.btnCancel.Click += new System.EventHandler(this.CancelClickHandler);
            // 
            // tabsData
            // 
            this.tabsData.Controls.Add(this.pageCallNumbers);
            this.tabsData.Dock = System.Windows.Forms.DockStyle.Top;
            this.tabsData.Location = new System.Drawing.Point(0, 60);
            this.tabsData.Name = "tabsData";
            this.tabsData.SelectedIndex = 0;
            this.tabsData.Size = new System.Drawing.Size(673, 237);
            this.tabsData.TabIndex = 1;
            // 
            // pageCallNumbers
            // 
            this.pageCallNumbers.Location = new System.Drawing.Point(4, 26);
            this.pageCallNumbers.Name = "pageCallNumbers";
            this.pageCallNumbers.Size = new System.Drawing.Size(665, 207);
            this.pageCallNumbers.TabIndex = 0;
            this.pageCallNumbers.Text = "pageCallNumbers";
            // 
            // RepositoryCitEditDlg
            // 
            this.AcceptButton = this.btnAccept;
            this.AutoScaleDimensions = new System.Drawing.SizeF(120F, 120F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
            this.CancelButton = this.btnCancel;
            this.ClientSize = new System.Drawing.Size(673, 360);
            this.Controls.Add(this.tabsData);
            this.Controls.Add(this.btnCancel);
            this.Controls.Add(this.btnAccept);
            this.Controls.Add(this.GroupBox1);
            this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.Name = "RepositoryCitEditDlg";
            this.ShowInTaskbar = false;
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
            this.Text = "RepositoryCitEditDlg";
            this.GroupBox1.ResumeLayout(false);
            this.GroupBox1.PerformLayout();
            this.tabsData.ResumeLayout(false);
            this.ResumeLayout(false);
        }
    }
}
