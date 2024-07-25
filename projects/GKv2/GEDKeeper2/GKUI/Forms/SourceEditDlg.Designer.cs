namespace GKUI.Forms
{
	partial class SourceEditDlg
	{
		private System.Windows.Forms.Button btnAccept;
		private System.Windows.Forms.Button btnCancel;
		private GKUI.Components.GKTabControl tabsData;
		private System.Windows.Forms.TabPage pageNotes;
		private System.Windows.Forms.TabPage pageMultimedia;
		private System.Windows.Forms.TabPage pageRepositories;
		private System.Windows.Forms.TabPage pageText;
		private System.Windows.Forms.TextBox txtText;
		private System.Windows.Forms.TabPage pageCommon;
		private System.Windows.Forms.Label lblShortTitle;
		private System.Windows.Forms.TextBox txtShortTitle;
		private System.Windows.Forms.Label lblAuthor;
		private System.Windows.Forms.TextBox txtAuthor;
		private System.Windows.Forms.Label lblTitle;
		private System.Windows.Forms.TextBox txtTitle;
		private System.Windows.Forms.Label lblPublication;
		private System.Windows.Forms.TextBox txtPublication;
        private System.Windows.Forms.Label lblDate;
        private Components.GKDateControl dateCtl;
        private System.Windows.Forms.TabPage pageUserRefs;

        private void InitializeComponent()
		{
		    this.btnAccept = new System.Windows.Forms.Button();
		    this.btnCancel = new System.Windows.Forms.Button();
		    this.tabsData = new GKUI.Components.GKTabControl();
		    this.pageCommon = new System.Windows.Forms.TabPage();
		    this.lblShortTitle = new System.Windows.Forms.Label();
		    this.lblAuthor = new System.Windows.Forms.Label();
		    this.lblTitle = new System.Windows.Forms.Label();
		    this.lblPublication = new System.Windows.Forms.Label();
		    this.txtShortTitle = new System.Windows.Forms.TextBox();
		    this.txtAuthor = new System.Windows.Forms.TextBox();
		    this.txtTitle = new System.Windows.Forms.TextBox();
		    this.txtPublication = new System.Windows.Forms.TextBox();
		    this.pageText = new System.Windows.Forms.TabPage();
		    this.txtText = new System.Windows.Forms.TextBox();
		    this.pageRepositories = new System.Windows.Forms.TabPage();
		    this.pageNotes = new System.Windows.Forms.TabPage();
		    this.pageMultimedia = new System.Windows.Forms.TabPage();
            this.lblDate = new System.Windows.Forms.Label();
            this.dateCtl = new GKUI.Components.GKDateControl();
            this.pageUserRefs = new System.Windows.Forms.TabPage();
            this.tabsData.SuspendLayout();
		    this.pageCommon.SuspendLayout();
		    this.pageText.SuspendLayout();
		    this.SuspendLayout();
		    // 
		    // btnAccept
		    // 
		    this.btnAccept.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
		    this.btnAccept.Location = new System.Drawing.Point(504, 505);
		    this.btnAccept.Name = "btnAccept";
		    this.btnAccept.Size = new System.Drawing.Size(113, 31);
		    this.btnAccept.TabIndex = 1;
		    this.btnAccept.Text = "btnAccept";
		    this.btnAccept.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
		    this.btnAccept.Click += new System.EventHandler(this.AcceptClickHandler);
		    // 
		    // btnCancel
		    // 
		    this.btnCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
		    this.btnCancel.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
		    this.btnCancel.Location = new System.Drawing.Point(627, 505);
		    this.btnCancel.Name = "btnCancel";
		    this.btnCancel.Size = new System.Drawing.Size(114, 31);
		    this.btnCancel.TabIndex = 2;
		    this.btnCancel.Text = "btnCancel";
		    this.btnCancel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
		    this.btnCancel.Click += new System.EventHandler(this.CancelClickHandler);
		    // 
		    // tabsData
		    // 
		    this.tabsData.Controls.Add(this.pageCommon);
		    this.tabsData.Controls.Add(this.pageText);
		    this.tabsData.Controls.Add(this.pageRepositories);
		    this.tabsData.Controls.Add(this.pageNotes);
		    this.tabsData.Controls.Add(this.pageMultimedia);
            this.tabsData.Controls.Add(this.pageUserRefs);
            this.tabsData.Dock = System.Windows.Forms.DockStyle.Top;
		    this.tabsData.Location = new System.Drawing.Point(0, 0);
		    this.tabsData.Name = "tabsData";
		    this.tabsData.SelectedIndex = 0;
		    this.tabsData.Size = new System.Drawing.Size(752, 487);
		    this.tabsData.TabIndex = 0;
		    // 
		    // pageCommon
		    // 
		    this.pageCommon.Controls.Add(this.lblShortTitle);
		    this.pageCommon.Controls.Add(this.lblAuthor);
		    this.pageCommon.Controls.Add(this.lblTitle);
		    this.pageCommon.Controls.Add(this.lblPublication);
		    this.pageCommon.Controls.Add(this.txtShortTitle);
		    this.pageCommon.Controls.Add(this.txtAuthor);
		    this.pageCommon.Controls.Add(this.txtTitle);
		    this.pageCommon.Controls.Add(this.txtPublication);
            this.pageCommon.Controls.Add(this.lblDate);
            this.pageCommon.Controls.Add(this.dateCtl);
            this.pageCommon.Location = new System.Drawing.Point(4, 26);
		    this.pageCommon.Name = "pageCommon";
		    this.pageCommon.Size = new System.Drawing.Size(744, 457);
		    this.pageCommon.TabIndex = 0;
		    this.pageCommon.Text = "pageCommon";
		    // 
		    // lblShortTitle
		    // 
		    this.lblShortTitle.AutoSize = true;
		    this.lblShortTitle.Location = new System.Drawing.Point(11, 10);
		    this.lblShortTitle.Name = "lblShortTitle";
		    this.lblShortTitle.Size = new System.Drawing.Size(78, 17);
		    this.lblShortTitle.TabIndex = 0;
		    this.lblShortTitle.Text = "lblShortTitle";
		    // 
		    // lblAuthor
		    // 
		    this.lblAuthor.AutoSize = true;
		    this.lblAuthor.Location = new System.Drawing.Point(11, 104);
		    this.lblAuthor.Name = "lblAuthor";
		    this.lblAuthor.Size = new System.Drawing.Size(62, 17);
		    this.lblAuthor.TabIndex = 2;
		    this.lblAuthor.Text = "lblAuthor";
		    // 
		    // lblTitle
		    // 
		    this.lblTitle.AutoSize = true;
		    this.lblTitle.Location = new System.Drawing.Point(11, 210);
		    this.lblTitle.Name = "lblTitle";
		    this.lblTitle.Size = new System.Drawing.Size(44, 17);
		    this.lblTitle.TabIndex = 4;
		    this.lblTitle.Text = "lblTitle";
		    // 
		    // lblPublication
		    // 
		    this.lblPublication.AutoSize = true;
		    this.lblPublication.Location = new System.Drawing.Point(11, 316);
		    this.lblPublication.Name = "lblPublication";
		    this.lblPublication.Size = new System.Drawing.Size(85, 17);
		    this.lblPublication.TabIndex = 6;
		    this.lblPublication.Text = "lblPublication";
		    // 
		    // txtShortTitle
		    // 
		    this.txtShortTitle.Location = new System.Drawing.Point(157, 10);
		    this.txtShortTitle.Name = "txtShortTitle";
		    this.txtShortTitle.Size = new System.Drawing.Size(572, 24);
		    this.txtShortTitle.TabIndex = 1;
		    this.txtShortTitle.TextChanged += new System.EventHandler(this.EditShortTitle_TextChanged);
		    // 
		    // txtAuthor
		    // 
		    this.txtAuthor.Location = new System.Drawing.Point(157, 104);
		    this.txtAuthor.Multiline = true;
		    this.txtAuthor.Name = "txtAuthor";
		    this.txtAuthor.ScrollBars = System.Windows.Forms.ScrollBars.Vertical;
		    this.txtAuthor.Size = new System.Drawing.Size(572, 127);
		    this.txtAuthor.TabIndex = 3;
		    // 
		    // txtTitle
		    // 
		    this.txtTitle.Location = new System.Drawing.Point(157, 210);
		    this.txtTitle.Multiline = true;
		    this.txtTitle.Name = "txtTitle";
		    this.txtTitle.ScrollBars = System.Windows.Forms.ScrollBars.Vertical;
		    this.txtTitle.Size = new System.Drawing.Size(572, 127);
		    this.txtTitle.TabIndex = 5;
		    // 
		    // txtPublication
		    // 
		    this.txtPublication.Location = new System.Drawing.Point(157, 316);
		    this.txtPublication.Multiline = true;
		    this.txtPublication.Name = "txtPublication";
		    this.txtPublication.ScrollBars = System.Windows.Forms.ScrollBars.Vertical;
		    this.txtPublication.Size = new System.Drawing.Size(572, 127);
		    this.txtPublication.TabIndex = 7;
            // 
            // lblDate
            // 
            this.lblDate.AutoSize = true;
            this.lblDate.Location = new System.Drawing.Point(11, 39);
            this.lblDate.Name = "lblDate";
            this.lblDate.Size = new System.Drawing.Size(49, 17);
            this.lblDate.TabIndex = 10;
            this.lblDate.Text = "lblDate";
            // 
            // dateCtl
            // 
            this.dateCtl.Location = new System.Drawing.Point(155, 39);
            this.dateCtl.Margin = new System.Windows.Forms.Padding(2);
            this.dateCtl.Name = "dateCtl";
            this.dateCtl.Size = new System.Drawing.Size(474, 63);
            this.dateCtl.TabIndex = 10;
            // 
            // pageText
            // 
            this.pageText.Controls.Add(this.txtText);
		    this.pageText.Location = new System.Drawing.Point(4, 26);
		    this.pageText.Name = "pageText";
		    this.pageText.Size = new System.Drawing.Size(744, 457);
		    this.pageText.TabIndex = 1;
		    this.pageText.Text = "pageText";
		    // 
		    // txtText
		    // 
		    this.txtText.Dock = System.Windows.Forms.DockStyle.Fill;
		    this.txtText.Location = new System.Drawing.Point(0, 0);
		    this.txtText.Multiline = true;
		    this.txtText.Name = "txtText";
		    this.txtText.ScrollBars = System.Windows.Forms.ScrollBars.Both;
		    this.txtText.Size = new System.Drawing.Size(744, 457);
		    this.txtText.TabIndex = 0;
		    // 
		    // pageRepositories
		    // 
		    this.pageRepositories.Location = new System.Drawing.Point(4, 26);
		    this.pageRepositories.Name = "pageRepositories";
		    this.pageRepositories.Size = new System.Drawing.Size(744, 457);
		    this.pageRepositories.TabIndex = 2;
		    this.pageRepositories.Text = "pageRepositories";
		    // 
		    // pageNotes
		    // 
		    this.pageNotes.Location = new System.Drawing.Point(4, 26);
		    this.pageNotes.Name = "pageNotes";
		    this.pageNotes.Size = new System.Drawing.Size(744, 457);
		    this.pageNotes.TabIndex = 3;
		    this.pageNotes.Text = "pageNotes";
		    // 
		    // pageMultimedia
		    // 
		    this.pageMultimedia.Location = new System.Drawing.Point(4, 26);
		    this.pageMultimedia.Name = "pageMultimedia";
		    this.pageMultimedia.Size = new System.Drawing.Size(744, 457);
		    this.pageMultimedia.TabIndex = 4;
		    this.pageMultimedia.Text = "pageMultimedia";
            // 
            // pageUserRefs
            // 
            this.pageUserRefs.Location = new System.Drawing.Point(4, 22);
            this.pageUserRefs.Margin = new System.Windows.Forms.Padding(2);
            this.pageUserRefs.Name = "pageUserRefs";
            this.pageUserRefs.Size = new System.Drawing.Size(691, 230);
            this.pageUserRefs.TabIndex = 5;
            this.pageUserRefs.Text = "pageUserRefs";
            // 
            // SourceEditDlg
            // 
            this.AcceptButton = this.btnAccept;
		    this.AutoScaleDimensions = new System.Drawing.SizeF(120F, 120F);
		    this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
		    this.CancelButton = this.btnCancel;
		    this.ClientSize = new System.Drawing.Size(752, 549);
		    this.Controls.Add(this.btnAccept);
		    this.Controls.Add(this.btnCancel);
		    this.Controls.Add(this.tabsData);
		    this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
		    this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
		    this.MaximizeBox = false;
		    this.MinimizeBox = false;
		    this.Name = "SourceEditDlg";
		    this.ShowInTaskbar = false;
		    this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
		    this.Text = "SourceEditDlg";
		    this.tabsData.ResumeLayout(false);
		    this.pageCommon.ResumeLayout(false);
		    this.pageCommon.PerformLayout();
		    this.pageText.ResumeLayout(false);
		    this.pageText.PerformLayout();
		    this.ResumeLayout(false);
		}
	}
}
