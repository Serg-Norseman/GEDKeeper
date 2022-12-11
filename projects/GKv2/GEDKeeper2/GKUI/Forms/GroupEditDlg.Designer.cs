namespace GKUI.Forms
{
	partial class GroupEditDlg
	{
		private System.Windows.Forms.GroupBox GroupBox1;
		private System.Windows.Forms.TextBox edName;
		private System.Windows.Forms.Label lblName;
		private GKUI.Components.GKTabControl tabsGroupData;
		private System.Windows.Forms.TabPage pageNotes;
		private System.Windows.Forms.TabPage pageMultimedia;
		private System.Windows.Forms.TabPage pageMembers;
		private System.Windows.Forms.Button btnAccept;
		private System.Windows.Forms.Button btnCancel;

		private void InitializeComponent()
		{
			this.GroupBox1 = new System.Windows.Forms.GroupBox();
			this.lblName = new System.Windows.Forms.Label();
			this.edName = new System.Windows.Forms.TextBox();
			this.btnAccept = new System.Windows.Forms.Button();
			this.btnCancel = new System.Windows.Forms.Button();
			this.tabsGroupData = new GKUI.Components.GKTabControl();
			this.pageMembers = new System.Windows.Forms.TabPage();
			this.pageNotes = new System.Windows.Forms.TabPage();
			this.pageMultimedia = new System.Windows.Forms.TabPage();
			this.GroupBox1.SuspendLayout();
			this.tabsGroupData.SuspendLayout();
			this.SuspendLayout();
			// 
			// GroupBox1
			// 
			this.GroupBox1.Controls.Add(this.lblName);
			this.GroupBox1.Controls.Add(this.edName);
			this.GroupBox1.Dock = System.Windows.Forms.DockStyle.Top;
			this.GroupBox1.Location = new System.Drawing.Point(0, 0);
			this.GroupBox1.Name = "GroupBox1";
			this.GroupBox1.Size = new System.Drawing.Size(673, 60);
			this.GroupBox1.TabIndex = 0;
			this.GroupBox1.TabStop = false;
			// 
			// Label1
			// 
			this.lblName.AutoSize = true;
			this.lblName.Location = new System.Drawing.Point(12, 22);
			this.lblName.Name = "lblName";
			this.lblName.Size = new System.Drawing.Size(67, 17);
			this.lblName.TabIndex = 0;
			this.lblName.Text = "lblName";
			// 
			// edName
			// 
			this.edName.Location = new System.Drawing.Point(101, 19);
			this.edName.Name = "edName";
			this.edName.Size = new System.Drawing.Size(561, 24);
			this.edName.TabIndex = 0;
			// 
			// btnAccept
			// 
			this.btnAccept.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
			this.btnAccept.Location = new System.Drawing.Point(426, 515);
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
			this.btnCancel.Location = new System.Drawing.Point(549, 515);
			this.btnCancel.Name = "btnCancel";
			this.btnCancel.Size = new System.Drawing.Size(113, 30);
			this.btnCancel.TabIndex = 3;
			this.btnCancel.Text = "btnCancel";
			this.btnCancel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
			this.btnCancel.Click += new System.EventHandler(this.CancelClickHandler);
			// 
			// PagesGroupData
			// 
			this.tabsGroupData.Controls.Add(this.pageMembers);
			this.tabsGroupData.Controls.Add(this.pageNotes);
			this.tabsGroupData.Controls.Add(this.pageMultimedia);
			this.tabsGroupData.Dock = System.Windows.Forms.DockStyle.Top;
			this.tabsGroupData.Location = new System.Drawing.Point(0, 60);
			this.tabsGroupData.Name = "PagesGroupData";
			this.tabsGroupData.SelectedIndex = 0;
			this.tabsGroupData.Size = new System.Drawing.Size(673, 437);
			this.tabsGroupData.TabIndex = 1;
			// 
			// pageMembers
			// 
			this.pageMembers.Location = new System.Drawing.Point(4, 26);
			this.pageMembers.Name = "pageMembers";
			this.pageMembers.Size = new System.Drawing.Size(665, 407);
			this.pageMembers.TabIndex = 0;
			this.pageMembers.Text = "pageMembers";
			// 
			// pageNotes
			// 
			this.pageNotes.Location = new System.Drawing.Point(4, 26);
			this.pageNotes.Name = "pageNotes";
			this.pageNotes.Size = new System.Drawing.Size(665, 407);
			this.pageNotes.TabIndex = 1;
			this.pageNotes.Text = "pageNotes";
			// 
			// pageMultimedia
			// 
			this.pageMultimedia.Location = new System.Drawing.Point(4, 26);
			this.pageMultimedia.Name = "pageMultimedia";
			this.pageMultimedia.Size = new System.Drawing.Size(665, 407);
			this.pageMultimedia.TabIndex = 2;
			this.pageMultimedia.Text = "pageMultimedia";
			// 
			// GroupEditDlg
			// 
			this.AcceptButton = this.btnAccept;
			this.AutoScaleDimensions = new System.Drawing.SizeF(120F, 120F);
			this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
			this.CancelButton = this.btnCancel;
			this.ClientSize = new System.Drawing.Size(673, 560);
			this.Controls.Add(this.tabsGroupData);
			this.Controls.Add(this.btnCancel);
			this.Controls.Add(this.btnAccept);
			this.Controls.Add(this.GroupBox1);
			this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
			this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
			this.MaximizeBox = false;
			this.MinimizeBox = false;
			this.Name = "GroupEditDlg";
			this.ShowInTaskbar = false;
			this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
			this.Text = "GroupEditDlg";
			this.GroupBox1.ResumeLayout(false);
			this.GroupBox1.PerformLayout();
			this.tabsGroupData.ResumeLayout(false);
			this.ResumeLayout(false);
		}
	}
}
