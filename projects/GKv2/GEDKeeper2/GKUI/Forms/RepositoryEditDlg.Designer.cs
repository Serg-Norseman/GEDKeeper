namespace GKUI.Forms
{
	partial class RepositoryEditDlg
	{
		private System.Windows.Forms.Button btnAccept;
		private System.Windows.Forms.Button btnCancel;
		private System.Windows.Forms.GroupBox GroupBox1;
		private System.Windows.Forms.Label lblName;
		private System.Windows.Forms.TextBox txtName;
		private GKUI.Components.GKTabControl tabsData;
		private System.Windows.Forms.TabPage pageNotes;
		private System.Windows.Forms.Button btnAddress;
        private System.Windows.Forms.TabPage pageUserRefs;

        private void InitializeComponent()
		{
			this.btnAccept = new System.Windows.Forms.Button();
			this.btnCancel = new System.Windows.Forms.Button();
			this.GroupBox1 = new System.Windows.Forms.GroupBox();
			this.lblName = new System.Windows.Forms.Label();
			this.txtName = new System.Windows.Forms.TextBox();
			this.btnAddress = new System.Windows.Forms.Button();
			this.tabsData = new GKUI.Components.GKTabControl();
			this.pageNotes = new System.Windows.Forms.TabPage();
            this.pageUserRefs = new System.Windows.Forms.TabPage();
            this.GroupBox1.SuspendLayout();
			this.tabsData.SuspendLayout();
			this.SuspendLayout();
			// 
			// btnAccept
			// 
			this.btnAccept.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
			this.btnAccept.Location = new System.Drawing.Point(336, 408);
			this.btnAccept.Name = "btnAccept";
			this.btnAccept.Size = new System.Drawing.Size(113, 30);
			this.btnAccept.TabIndex = 3;
			this.btnAccept.Text = "btnAccept";
			this.btnAccept.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
			this.btnAccept.Click += new System.EventHandler(this.AcceptClickHandler);
			// 
			// btnCancel
			// 
			this.btnCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
			this.btnCancel.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
			this.btnCancel.Location = new System.Drawing.Point(459, 408);
			this.btnCancel.Name = "btnCancel";
			this.btnCancel.Size = new System.Drawing.Size(114, 30);
			this.btnCancel.TabIndex = 4;
			this.btnCancel.Text = "btnCancel";
			this.btnCancel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
			this.btnCancel.Click += new System.EventHandler(this.CancelClickHandler);
			// 
			// GroupBox1
			// 
			this.GroupBox1.Controls.Add(this.lblName);
			this.GroupBox1.Controls.Add(this.txtName);
			this.GroupBox1.Dock = System.Windows.Forms.DockStyle.Top;
			this.GroupBox1.Location = new System.Drawing.Point(0, 0);
			this.GroupBox1.Name = "GroupBox1";
			this.GroupBox1.Size = new System.Drawing.Size(586, 50);
			this.GroupBox1.TabIndex = 0;
			this.GroupBox1.TabStop = false;
			// 
			// lblName
			// 
			this.lblName.AutoSize = true;
			this.lblName.Location = new System.Drawing.Point(12, 18);
			this.lblName.Name = "lblName";
			this.lblName.Size = new System.Drawing.Size(67, 17);
			this.lblName.TabIndex = 0;
			this.lblName.Text = "lblName";
			// 
			// txtName
			// 
			this.txtName.Location = new System.Drawing.Point(101, 15);
			this.txtName.Name = "txtName";
			this.txtName.Size = new System.Drawing.Size(472, 24);
			this.txtName.TabIndex = 1;
			// 
			// btnAddress
			// 
			this.btnAddress.Location = new System.Drawing.Point(11, 408);
			this.btnAddress.Name = "btnAddress";
			this.btnAddress.Size = new System.Drawing.Size(114, 30);
			this.btnAddress.TabIndex = 2;
			this.btnAddress.Text = "btnAddress";
			this.btnAddress.Click += new System.EventHandler(this.btnAddress_Click);
			// 
			// PagesData
			// 
			this.tabsData.Controls.Add(this.pageNotes);
            this.tabsData.Controls.Add(this.pageUserRefs);
            this.tabsData.Dock = System.Windows.Forms.DockStyle.Top;
			this.tabsData.Location = new System.Drawing.Point(0, 50);
			this.tabsData.Name = "PagesData";
			this.tabsData.SelectedIndex = 0;
			this.tabsData.Size = new System.Drawing.Size(586, 340);
			this.tabsData.TabIndex = 1;
			// 
			// pageNotes
			// 
			this.pageNotes.Location = new System.Drawing.Point(4, 26);
			this.pageNotes.Name = "pageNotes";
			this.pageNotes.Size = new System.Drawing.Size(578, 310);
			this.pageNotes.TabIndex = 0;
			this.pageNotes.Text = "pageNotes";
            // 
            // pageUserRefs
            // 
            this.pageUserRefs.Location = new System.Drawing.Point(4, 22);
            this.pageUserRefs.Margin = new System.Windows.Forms.Padding(2);
            this.pageUserRefs.Name = "pageUserRefs";
            this.pageUserRefs.Size = new System.Drawing.Size(691, 230);
            this.pageUserRefs.TabIndex = 1;
            this.pageUserRefs.Text = "pageUserRefs";
            // 
            // RepositoryEditDlg
            // 
            this.AcceptButton = this.btnAccept;
			this.AutoScaleDimensions = new System.Drawing.SizeF(120F, 120F);
			this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
			this.CancelButton = this.btnCancel;
			this.ClientSize = new System.Drawing.Size(586, 455);
			this.Controls.Add(this.tabsData);
			this.Controls.Add(this.btnAccept);
			this.Controls.Add(this.btnCancel);
			this.Controls.Add(this.GroupBox1);
			this.Controls.Add(this.btnAddress);
			this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
			this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
			this.MaximizeBox = false;
			this.MinimizeBox = false;
			this.Name = "RepositoryEditDlg";
			this.ShowInTaskbar = false;
			this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
			this.Text = "RepositoryEditDlg";
			this.GroupBox1.ResumeLayout(false);
			this.GroupBox1.PerformLayout();
			this.tabsData.ResumeLayout(false);
			this.ResumeLayout(false);
		}
	}
}
