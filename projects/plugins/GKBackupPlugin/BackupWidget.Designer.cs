namespace GKBackupPlugin
{
	partial class BackupWidget
	{
		private System.Windows.Forms.CheckBox chkEnabled;
		private System.Windows.Forms.Label lblFolder;
		private System.Windows.Forms.TextBox txtFolder;
		private System.Windows.Forms.Button btnFolderChoose;

		private void InitializeComponent()
		{
		    this.chkEnabled = new System.Windows.Forms.CheckBox();
		    this.lblFolder = new System.Windows.Forms.Label();
		    this.txtFolder = new System.Windows.Forms.TextBox();
		    this.btnFolderChoose = new System.Windows.Forms.Button();
		    this.SuspendLayout();
		    // 
		    // chkEnabled
		    // 
		    this.chkEnabled.Location = new System.Drawing.Point(12, 12);
		    this.chkEnabled.Name = "chkEnabled";
		    this.chkEnabled.Size = new System.Drawing.Size(259, 30);
		    this.chkEnabled.TabIndex = 3;
		    this.chkEnabled.Text = "Enabled";
		    this.chkEnabled.CheckedChanged += new System.EventHandler(this.chkEnabled_CheckedChanged);
		    // 
		    // Folder
		    // 
		    this.lblFolder.AutoSize = true;
		    this.lblFolder.Location = new System.Drawing.Point(12, 45);
		    this.lblFolder.Name = "Folder";
		    this.lblFolder.Size = new System.Drawing.Size(57, 17);
		    this.lblFolder.TabIndex = 7;
		    this.lblFolder.Text = "lblFolder";
		    // 
		    // txtFolder
		    // 
		    this.txtFolder.Location = new System.Drawing.Point(12, 65);
		    this.txtFolder.Name = "txtFolder";
		    this.txtFolder.ReadOnly = true;
		    this.txtFolder.Size = new System.Drawing.Size(344, 24);
		    this.txtFolder.TabIndex = 8;
		    // 
		    // btnFolderChoose
		    // 
		    this.btnFolderChoose.Enabled = false;
		    this.btnFolderChoose.Location = new System.Drawing.Point(243, 95);
		    this.btnFolderChoose.Name = "btnFolderChoose";
		    this.btnFolderChoose.Size = new System.Drawing.Size(113, 30);
		    this.btnFolderChoose.TabIndex = 9;
		    this.btnFolderChoose.Text = "Folder choose...";
		    this.btnFolderChoose.Click += new System.EventHandler(this.btnFolderChoose_Click);
		    // 
		    // BackupWidget
		    // 
		    this.AutoScaleDimensions = new System.Drawing.SizeF(8F, 17F);
		    this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
		    this.ClientSize = new System.Drawing.Size(368, 138);
		    this.Controls.Add(this.lblFolder);
		    this.Controls.Add(this.txtFolder);
		    this.Controls.Add(this.btnFolderChoose);
		    this.Controls.Add(this.chkEnabled);
		    this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
		    this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedToolWindow;
		    this.Name = "BackupWidget";
		    this.ShowInTaskbar = false;
		    this.StartPosition = System.Windows.Forms.FormStartPosition.Manual;
		    this.Text = "Backup";
		    this.TopMost = true;
		    this.Closed += new System.EventHandler(this.Form_Closed);
		    this.Load += new System.EventHandler(this.Form_Load);
		    this.ResumeLayout(false);
		    this.PerformLayout();

		}
	}
}
