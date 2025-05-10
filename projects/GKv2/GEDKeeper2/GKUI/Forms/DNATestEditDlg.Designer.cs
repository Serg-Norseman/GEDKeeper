namespace GKUI.Forms
{
    partial class DNATestEditDlg
    {
        private System.Windows.Forms.Button btnAccept;
        private System.Windows.Forms.Button btnCancel;
        private GKUI.Components.GKTabControl tabsData;
        private System.Windows.Forms.TabPage pageNotes;
        private System.Windows.Forms.TabPage pageMultimedia;
        private System.Windows.Forms.TabPage pageCommon;
        private System.Windows.Forms.Label lblTestName;
        private System.Windows.Forms.TextBox txtTestName;
        private System.Windows.Forms.Label lblAgency;
        private System.Windows.Forms.ComboBox cmbAgency;
        private System.Windows.Forms.Label lblDate;
        private Components.GKDateBox dateCtl;
        private System.Windows.Forms.Label lblFileRef;
        private System.Windows.Forms.TextBox txtFileRef;
        private System.Windows.Forms.Label lblStoreType;
        private System.Windows.Forms.ComboBox cmbStoreType;
        private System.Windows.Forms.Button btnFileSelect;
        private System.Windows.Forms.Label lblFileFormat;
        private System.Windows.Forms.ComboBox cmbFileFormat;
        private System.Windows.Forms.Label lblYHaplogroup;
        private System.Windows.Forms.ComboBox cmbYHaplogroup;
        private System.Windows.Forms.Label lblMHaplogroup;
        private System.Windows.Forms.ComboBox cmbMHaplogroup;
        private System.Windows.Forms.Label lblRestriction;
        private System.Windows.Forms.ComboBox cmbRestriction;

        private void InitializeComponent()
        {
            this.btnAccept = new System.Windows.Forms.Button();
            this.btnCancel = new System.Windows.Forms.Button();
            this.tabsData = new GKUI.Components.GKTabControl();
            this.pageCommon = new System.Windows.Forms.TabPage();
            this.dateCtl = new GKUI.Components.GKDateBox();
            this.lblTestName = new System.Windows.Forms.Label();
            this.lblFileRef = new System.Windows.Forms.Label();
            this.lblDate = new System.Windows.Forms.Label();
            this.lblStoreType = new System.Windows.Forms.Label();
            this.lblAgency = new System.Windows.Forms.Label();
            this.txtTestName = new System.Windows.Forms.TextBox();
            this.txtFileRef = new System.Windows.Forms.TextBox();
            this.cmbStoreType = new System.Windows.Forms.ComboBox();
            this.cmbAgency = new System.Windows.Forms.ComboBox();
            this.pageNotes = new System.Windows.Forms.TabPage();
            this.pageMultimedia = new System.Windows.Forms.TabPage();
            this.btnFileSelect = new System.Windows.Forms.Button();
            this.lblFileFormat = new System.Windows.Forms.Label();
            this.cmbFileFormat = new System.Windows.Forms.ComboBox();
            this.lblMHaplogroup = new System.Windows.Forms.Label();
            this.cmbMHaplogroup = new System.Windows.Forms.ComboBox();
            this.lblYHaplogroup = new System.Windows.Forms.Label();
            this.cmbYHaplogroup = new System.Windows.Forms.ComboBox();
            this.lblRestriction = new System.Windows.Forms.Label();
            this.cmbRestriction = new System.Windows.Forms.ComboBox();
            this.tabsData.SuspendLayout();
            this.pageCommon.SuspendLayout();
            this.SuspendLayout();
            // 
            // btnAccept
            // 
            this.btnAccept.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.btnAccept.Location = new System.Drawing.Point(297, 358);
            this.btnAccept.Margin = new System.Windows.Forms.Padding(2, 2, 2, 2);
            this.btnAccept.Name = "btnAccept";
            this.btnAccept.Size = new System.Drawing.Size(90, 24);
            this.btnAccept.TabIndex = 2;
            this.btnAccept.Text = "btnAccept";
            this.btnAccept.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            this.btnAccept.Click += new System.EventHandler(this.AcceptClickHandler);
            // 
            // btnCancel
            // 
            this.btnCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.btnCancel.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.btnCancel.Location = new System.Drawing.Point(395, 358);
            this.btnCancel.Margin = new System.Windows.Forms.Padding(2, 2, 2, 2);
            this.btnCancel.Name = "btnCancel";
            this.btnCancel.Size = new System.Drawing.Size(90, 24);
            this.btnCancel.TabIndex = 3;
            this.btnCancel.Text = "btnCancel";
            this.btnCancel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            this.btnCancel.Click += new System.EventHandler(this.CancelClickHandler);
            // 
            // tabsData
            // 
            this.tabsData.CloseableTabs = false;
            this.tabsData.Controls.Add(this.pageCommon);
            this.tabsData.Controls.Add(this.pageNotes);
            this.tabsData.Controls.Add(this.pageMultimedia);
            this.tabsData.Dock = System.Windows.Forms.DockStyle.Top;
            this.tabsData.Location = new System.Drawing.Point(0, 0);
            this.tabsData.Margin = new System.Windows.Forms.Padding(2, 2, 2, 2);
            this.tabsData.Name = "tabsData";
            this.tabsData.SelectedIndex = 0;
            this.tabsData.Size = new System.Drawing.Size(496, 343);
            this.tabsData.TabIndex = 0;
            // 
            // pageCommon
            // 
            this.pageCommon.Controls.Add(this.lblYHaplogroup);
            this.pageCommon.Controls.Add(this.cmbYHaplogroup);
            this.pageCommon.Controls.Add(this.lblMHaplogroup);
            this.pageCommon.Controls.Add(this.cmbMHaplogroup);
            this.pageCommon.Controls.Add(this.btnFileSelect);
            this.pageCommon.Controls.Add(this.dateCtl);
            this.pageCommon.Controls.Add(this.lblTestName);
            this.pageCommon.Controls.Add(this.lblFileRef);
            this.pageCommon.Controls.Add(this.lblDate);
            this.pageCommon.Controls.Add(this.lblFileFormat);
            this.pageCommon.Controls.Add(this.lblStoreType);
            this.pageCommon.Controls.Add(this.lblAgency);
            this.pageCommon.Controls.Add(this.txtTestName);
            this.pageCommon.Controls.Add(this.txtFileRef);
            this.pageCommon.Controls.Add(this.cmbFileFormat);
            this.pageCommon.Controls.Add(this.cmbStoreType);
            this.pageCommon.Controls.Add(this.cmbAgency);
            this.pageCommon.Location = new System.Drawing.Point(4, 22);
            this.pageCommon.Margin = new System.Windows.Forms.Padding(2, 2, 2, 2);
            this.pageCommon.Name = "pageCommon";
            this.pageCommon.Size = new System.Drawing.Size(488, 317);
            this.pageCommon.TabIndex = 0;
            this.pageCommon.Text = "pageCommon";
            // 
            // dateCtl
            // 
            this.dateCtl.Location = new System.Drawing.Point(363, 23);
            this.dateCtl.Margin = new System.Windows.Forms.Padding(8, 2, 2, 2);
            this.dateCtl.Name = "dateCtl";
            this.dateCtl.Size = new System.Drawing.Size(115, 21);
            this.dateCtl.TabIndex = 10;
            // 
            // lblTestName
            // 
            this.lblTestName.AutoSize = true;
            this.lblTestName.Location = new System.Drawing.Point(9, 8);
            this.lblTestName.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.lblTestName.Name = "lblTestName";
            this.lblTestName.Size = new System.Drawing.Size(65, 13);
            this.lblTestName.TabIndex = 0;
            this.lblTestName.Text = "lblTestName";
            // 
            // lblFileRef
            // 
            this.lblFileRef.AutoSize = true;
            this.lblFileRef.Location = new System.Drawing.Point(9, 100);
            this.lblFileRef.Margin = new System.Windows.Forms.Padding(2, 8, 2, 0);
            this.lblFileRef.Name = "lblFileRef";
            this.lblFileRef.Size = new System.Drawing.Size(50, 13);
            this.lblFileRef.TabIndex = 5;
            this.lblFileRef.Text = "lblFileRef";
            // 
            // lblDate
            // 
            this.lblDate.AutoSize = true;
            this.lblDate.Location = new System.Drawing.Point(360, 8);
            this.lblDate.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.lblDate.Name = "lblDate";
            this.lblDate.Size = new System.Drawing.Size(40, 13);
            this.lblDate.TabIndex = 10;
            this.lblDate.Text = "lblDate";
            // 
            // lblStoreType
            // 
            this.lblStoreType.AutoSize = true;
            this.lblStoreType.Location = new System.Drawing.Point(9, 146);
            this.lblStoreType.Margin = new System.Windows.Forms.Padding(2, 8, 2, 0);
            this.lblStoreType.Name = "lblStoreType";
            this.lblStoreType.Size = new System.Drawing.Size(67, 13);
            this.lblStoreType.TabIndex = 18;
            this.lblStoreType.Text = "lblStoreType";
            // 
            // lblAgency
            // 
            this.lblAgency.AutoSize = true;
            this.lblAgency.Location = new System.Drawing.Point(9, 54);
            this.lblAgency.Margin = new System.Windows.Forms.Padding(2, 8, 2, 0);
            this.lblAgency.Name = "lblAgency";
            this.lblAgency.Size = new System.Drawing.Size(53, 13);
            this.lblAgency.TabIndex = 20;
            this.lblAgency.Text = "lblAgency";
            // 
            // txtTestName
            // 
            this.txtTestName.Location = new System.Drawing.Point(9, 23);
            this.txtTestName.Margin = new System.Windows.Forms.Padding(2, 2, 2, 2);
            this.txtTestName.Name = "txtTestName";
            this.txtTestName.Size = new System.Drawing.Size(344, 21);
            this.txtTestName.TabIndex = 1;
            // 
            // txtFileRef
            // 
            this.txtFileRef.Location = new System.Drawing.Point(9, 115);
            this.txtFileRef.Margin = new System.Windows.Forms.Padding(2, 2, 2, 2);
            this.txtFileRef.Name = "txtFileRef";
            this.txtFileRef.Size = new System.Drawing.Size(411, 21);
            this.txtFileRef.TabIndex = 6;
            // 
            // cmbStoreType
            //
            this.cmbStoreType.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cmbStoreType.Location = new System.Drawing.Point(9, 161);
            this.cmbStoreType.Margin = new System.Windows.Forms.Padding(2, 2, 2, 2);
            this.cmbStoreType.Name = "cmbStoreType";
            this.cmbStoreType.Size = new System.Drawing.Size(187, 21);
            this.cmbStoreType.TabIndex = 19;
            // 
            // cmbAgency
            // 
            this.cmbAgency.Location = new System.Drawing.Point(9, 69);
            this.cmbAgency.Margin = new System.Windows.Forms.Padding(2, 2, 2, 2);
            this.cmbAgency.Name = "cmbAgency";
            this.cmbAgency.Size = new System.Drawing.Size(469, 21);
            this.cmbAgency.TabIndex = 21;
            // 
            // pageNotes
            // 
            this.pageNotes.Location = new System.Drawing.Point(4, 22);
            this.pageNotes.Margin = new System.Windows.Forms.Padding(2, 2, 2, 2);
            this.pageNotes.Name = "pageNotes";
            this.pageNotes.Size = new System.Drawing.Size(488, 317);
            this.pageNotes.TabIndex = 1;
            this.pageNotes.Text = "pageNotes";
            // 
            // pageMultimedia
            // 
            this.pageMultimedia.Location = new System.Drawing.Point(4, 22);
            this.pageMultimedia.Margin = new System.Windows.Forms.Padding(2, 2, 2, 2);
            this.pageMultimedia.Name = "pageMultimedia";
            this.pageMultimedia.Size = new System.Drawing.Size(488, 317);
            this.pageMultimedia.TabIndex = 2;
            this.pageMultimedia.Text = "pageMultimedia";
            // 
            // btnFileSelect
            // 
            this.btnFileSelect.Location = new System.Drawing.Point(430, 115);
            this.btnFileSelect.Margin = new System.Windows.Forms.Padding(8, 2, 2, 2);
            this.btnFileSelect.Name = "btnFileSelect";
            this.btnFileSelect.Size = new System.Drawing.Size(48, 21);
            this.btnFileSelect.TabIndex = 22;
            this.btnFileSelect.Text = "...";
            this.btnFileSelect.Click += new System.EventHandler(this.btnFileSelect_Click);
            // 
            // lblFileFormat
            // 
            this.lblFileFormat.AutoSize = true;
            this.lblFileFormat.Location = new System.Drawing.Point(203, 146);
            this.lblFileFormat.Margin = new System.Windows.Forms.Padding(2, 8, 2, 0);
            this.lblFileFormat.Name = "lblFileFormat";
            this.lblFileFormat.Size = new System.Drawing.Size(67, 13);
            this.lblFileFormat.TabIndex = 18;
            this.lblFileFormat.Text = "lblFileFormat";
            // 
            // cmbFileFormat
            // 
            this.cmbFileFormat.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cmbFileFormat.Location = new System.Drawing.Point(206, 161);
            this.cmbFileFormat.Margin = new System.Windows.Forms.Padding(8, 2, 2, 2);
            this.cmbFileFormat.Name = "cmbFileFormat";
            this.cmbFileFormat.Size = new System.Drawing.Size(157, 21);
            this.cmbFileFormat.TabIndex = 19;
            // 
            // lblMHaplogroup
            // 
            this.lblMHaplogroup.AutoSize = true;
            this.lblMHaplogroup.Location = new System.Drawing.Point(9, 192);
            this.lblMHaplogroup.Margin = new System.Windows.Forms.Padding(2, 8, 2, 0);
            this.lblMHaplogroup.Name = "lblMHaplogroup";
            this.lblMHaplogroup.Size = new System.Drawing.Size(80, 13);
            this.lblMHaplogroup.TabIndex = 23;
            this.lblMHaplogroup.Text = "lblMHaplogroup";
            // 
            // cmbMHaplogroup
            // 
            this.cmbMHaplogroup.Location = new System.Drawing.Point(9, 207);
            this.cmbMHaplogroup.Margin = new System.Windows.Forms.Padding(2);
            this.cmbMHaplogroup.Name = "cmbMHaplogroup";
            this.cmbMHaplogroup.Size = new System.Drawing.Size(469, 21);
            this.cmbMHaplogroup.TabIndex = 24;
            // 
            // lblYHaplogroup
            // 
            this.lblYHaplogroup.AutoSize = true;
            this.lblYHaplogroup.Location = new System.Drawing.Point(9, 238);
            this.lblYHaplogroup.Margin = new System.Windows.Forms.Padding(2, 8, 2, 0);
            this.lblYHaplogroup.Name = "lblYHaplogroup";
            this.lblYHaplogroup.Size = new System.Drawing.Size(78, 13);
            this.lblYHaplogroup.TabIndex = 25;
            this.lblYHaplogroup.Text = "lblYHaplogroup";
            // 
            // cmbYHaplogroup
            // 
            this.cmbYHaplogroup.Location = new System.Drawing.Point(9, 253);
            this.cmbYHaplogroup.Margin = new System.Windows.Forms.Padding(2);
            this.cmbYHaplogroup.Name = "cmbYHaplogroup";
            this.cmbYHaplogroup.Size = new System.Drawing.Size(469, 21);
            this.cmbYHaplogroup.TabIndex = 26;
            // 
            // lblRestriction
            // 
            this.lblRestriction.AutoSize = true;
            this.lblRestriction.Location = new System.Drawing.Point(12, 363);
            this.lblRestriction.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.lblRestriction.Name = "lblRestriction";
            this.lblRestriction.Size = new System.Drawing.Size(84, 17);
            this.lblRestriction.TabIndex = 2;
            this.lblRestriction.Text = "lblRestriction";
            // 
            // cmbRestriction
            // 
            this.cmbRestriction.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cmbRestriction.Location = new System.Drawing.Point(204, 360);
            this.cmbRestriction.Margin = new System.Windows.Forms.Padding(2);
            this.cmbRestriction.Name = "cmbRestriction";
            this.cmbRestriction.Size = new System.Drawing.Size(140, 25);
            this.cmbRestriction.TabIndex = 3;
            this.cmbRestriction.SelectedIndexChanged += new System.EventHandler(this.cbRestriction_SelectedIndexChanged);
            // 
            // DNATestEditDlg
            // 
            this.AcceptButton = this.btnAccept;
            this.AutoScaleDimensions = new System.Drawing.SizeF(96F, 96F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
            this.CancelButton = this.btnCancel;
            this.ClientSize = new System.Drawing.Size(496, 394);
            this.Controls.Add(this.btnAccept);
            this.Controls.Add(this.btnCancel);
            this.Controls.Add(this.tabsData);
            this.Controls.Add(this.lblRestriction);
            this.Controls.Add(this.cmbRestriction);
            this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
            this.Margin = new System.Windows.Forms.Padding(2, 2, 2, 2);
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.Name = "DNATestEditDlg";
            this.ShowInTaskbar = false;
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
            this.Text = "DNATestEditDlg";
            this.tabsData.ResumeLayout(false);
            this.pageCommon.ResumeLayout(false);
            this.pageCommon.PerformLayout();
            this.ResumeLayout(false);
        }
    }
}
