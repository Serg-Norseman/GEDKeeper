namespace GKUI.Forms
{
	partial class SourceCitEditDlg
	{
		private System.Windows.Forms.Button btnAccept;
		private System.Windows.Forms.Button btnCancel;
		private System.Windows.Forms.Label lblPage;
		private System.Windows.Forms.TextBox txtPage;
		private System.Windows.Forms.Label lblSource;
		private System.Windows.Forms.Button btnSourceAdd;
		private System.Windows.Forms.Label lblCertainty;
		private System.Windows.Forms.ComboBox txtCertainty;
		private System.Windows.Forms.ComboBox cmbSource;
		private GKUI.Components.GKTabControl tabsData;
		private System.Windows.Forms.TabPage pageCommon;
		private System.Windows.Forms.TabPage pageOther;
		private System.Windows.Forms.TextBox txtText;
		private GKUI.Components.GKDateControl dateCtl;

		private void InitializeComponent()
		{
		    this.btnAccept = new System.Windows.Forms.Button();
		    this.btnCancel = new System.Windows.Forms.Button();
		    this.tabsData = new GKUI.Components.GKTabControl();
		    this.pageCommon = new System.Windows.Forms.TabPage();
		    this.lblPage = new System.Windows.Forms.Label();
		    this.txtPage = new System.Windows.Forms.TextBox();
		    this.lblSource = new System.Windows.Forms.Label();
		    this.btnSourceAdd = new System.Windows.Forms.Button();
		    this.lblCertainty = new System.Windows.Forms.Label();
		    this.txtCertainty = new System.Windows.Forms.ComboBox();
		    this.cmbSource = new System.Windows.Forms.ComboBox();
		    this.pageOther = new System.Windows.Forms.TabPage();
		    this.txtText = new System.Windows.Forms.TextBox();
		    this.dateCtl = new GKUI.Components.GKDateControl();
		    this.tabsData.SuspendLayout();
		    this.pageCommon.SuspendLayout();
		    this.pageOther.SuspendLayout();
		    this.SuspendLayout();
		    // 
		    // btnAccept
		    // 
		    this.btnAccept.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
		    this.btnAccept.Location = new System.Drawing.Point(308, 319);
		    this.btnAccept.Margin = new System.Windows.Forms.Padding(4);
		    this.btnAccept.Name = "btnAccept";
		    this.btnAccept.Size = new System.Drawing.Size(112, 30);
            this.btnAccept.TabIndex = 1;
		    this.btnAccept.Text = "btnAccept";
		    this.btnAccept.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
		    this.btnAccept.Click += new System.EventHandler(this.AcceptClickHandler);
		    // 
		    // btnCancel
		    // 
		    this.btnCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
		    this.btnCancel.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
		    this.btnCancel.Location = new System.Drawing.Point(434, 319);
		    this.btnCancel.Margin = new System.Windows.Forms.Padding(4);
		    this.btnCancel.Name = "btnCancel";
		    this.btnCancel.Size = new System.Drawing.Size(112, 30);
            this.btnCancel.TabIndex = 2;
		    this.btnCancel.Text = "btnCancel";
		    this.btnCancel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
		    this.btnCancel.Click += new System.EventHandler(this.CancelClickHandler);
		    // 
		    // tabsData
		    // 
		    this.tabsData.Controls.Add(this.pageCommon);
		    this.tabsData.Controls.Add(this.pageOther);
		    this.tabsData.Dock = System.Windows.Forms.DockStyle.Top;
		    this.tabsData.Location = new System.Drawing.Point(0, 0);
		    this.tabsData.Name = "tabsData";
		    this.tabsData.SelectedIndex = 0;
		    this.tabsData.Size = new System.Drawing.Size(559, 307);
		    this.tabsData.TabIndex = 0;
		    // 
		    // pageCommon
		    // 
		    this.pageCommon.Controls.Add(this.lblPage);
		    this.pageCommon.Controls.Add(this.txtPage);
		    this.pageCommon.Controls.Add(this.lblSource);
		    this.pageCommon.Controls.Add(this.btnSourceAdd);
		    this.pageCommon.Controls.Add(this.lblCertainty);
		    this.pageCommon.Controls.Add(this.txtCertainty);
		    this.pageCommon.Controls.Add(this.cmbSource);
		    this.pageCommon.Location = new System.Drawing.Point(4, 26);
		    this.pageCommon.Name = "pageCommon";
		    this.pageCommon.Size = new System.Drawing.Size(551, 277);
		    this.pageCommon.TabIndex = 0;
		    this.pageCommon.Text = "pageCommon";
		    // 
		    // lblPage
		    // 
		    this.lblPage.AutoSize = true;
		    this.lblPage.Location = new System.Drawing.Point(16, 77);
		    this.lblPage.Margin = new System.Windows.Forms.Padding(4, 0, 4, 0);
		    this.lblPage.Name = "lblPage";
		    this.lblPage.Size = new System.Drawing.Size(50, 17);
		    this.lblPage.TabIndex = 3;
		    this.lblPage.Text = "lblPage";
		    // 
		    // txtPage
		    // 
		    this.txtPage.Location = new System.Drawing.Point(16, 97);
		    this.txtPage.Margin = new System.Windows.Forms.Padding(4);
		    this.txtPage.Name = "txtPage";
		    this.txtPage.Size = new System.Drawing.Size(410, 24);
		    this.txtPage.TabIndex = 4;
		    // 
		    // lblSource
		    // 
		    this.lblSource.AutoSize = true;
		    this.lblSource.Location = new System.Drawing.Point(16, 17);
		    this.lblSource.Margin = new System.Windows.Forms.Padding(4, 0, 4, 0);
		    this.lblSource.Name = "lblSource";
		    this.lblSource.Size = new System.Drawing.Size(63, 17);
		    this.lblSource.TabIndex = 0;
		    this.lblSource.Text = "lblSource";
		    // 
		    // btnSourceAdd
		    // 
		    this.btnSourceAdd.Location = new System.Drawing.Point(392, 31);
		    this.btnSourceAdd.Margin = new System.Windows.Forms.Padding(4);
		    this.btnSourceAdd.Name = "btnSourceAdd";
		    this.btnSourceAdd.Size = new System.Drawing.Size(35, 35);
		    this.btnSourceAdd.TabIndex = 2;
		    this.btnSourceAdd.TabStop = false;
		    this.btnSourceAdd.Click += new System.EventHandler(this.btnSourceAdd_Click);
		    // 
		    // lblCertainty
		    // 
		    this.lblCertainty.AutoSize = true;
		    this.lblCertainty.Location = new System.Drawing.Point(16, 137);
		    this.lblCertainty.Margin = new System.Windows.Forms.Padding(4, 0, 4, 0);
		    this.lblCertainty.Name = "lblCertainty";
		    this.lblCertainty.Size = new System.Drawing.Size(76, 17);
		    this.lblCertainty.TabIndex = 5;
		    this.lblCertainty.Text = "lblCertainty";
		    // 
		    // txtCertainty
		    // 
		    this.txtCertainty.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
		    this.txtCertainty.Location = new System.Drawing.Point(16, 157);
		    this.txtCertainty.Margin = new System.Windows.Forms.Padding(4);
		    this.txtCertainty.Name = "txtCertainty";
		    this.txtCertainty.Size = new System.Drawing.Size(410, 25);
		    this.txtCertainty.TabIndex = 6;
		    // 
		    // cmbSource
		    // 
		    this.cmbSource.Location = new System.Drawing.Point(16, 37);
		    this.cmbSource.Margin = new System.Windows.Forms.Padding(4);
		    this.cmbSource.Name = "cmbSource";
		    this.cmbSource.Size = new System.Drawing.Size(368, 25);
		    this.cmbSource.Sorted = true;
		    this.cmbSource.TabIndex = 1;
		    this.cmbSource.KeyUp += new System.Windows.Forms.KeyEventHandler(this.cbSource_KeyUp);
		    // 
		    // pageOther
		    // 
		    this.pageOther.Controls.Add(this.txtText);
		    this.pageOther.Controls.Add(this.dateCtl);
		    this.pageOther.Location = new System.Drawing.Point(4, 26);
		    this.pageOther.Name = "pageOther";
		    this.pageOther.Size = new System.Drawing.Size(551, 277);
		    this.pageOther.TabIndex = 3;
		    this.pageOther.Text = "pageOther";
		    // 
		    // txtText
		    // 
		    this.txtText.Dock = System.Windows.Forms.DockStyle.Fill;
		    this.txtText.Location = new System.Drawing.Point(0, 61);
		    this.txtText.Multiline = true;
		    this.txtText.Name = "txtText";
		    this.txtText.ScrollBars = System.Windows.Forms.ScrollBars.Both;
		    this.txtText.Size = new System.Drawing.Size(551, 216);
		    this.txtText.TabIndex = 3;
		    // 
		    // dateCtl
		    // 
		    this.dateCtl.AutoSize = true;
		    this.dateCtl.Date = null;
		    this.dateCtl.Dock = System.Windows.Forms.DockStyle.Top;
		    this.dateCtl.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
		    this.dateCtl.Location = new System.Drawing.Point(0, 0);
		    this.dateCtl.Name = "dateCtl";
		    this.dateCtl.Size = new System.Drawing.Size(551, 61);
		    this.dateCtl.TabIndex = 2;
		    // 
		    // SourceCitEditDlg
		    // 
		    this.AcceptButton = this.btnAccept;
		    this.AutoScaleDimensions = new System.Drawing.SizeF(120F, 120F);
		    this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
		    this.CancelButton = this.btnCancel;
		    this.ClientSize = new System.Drawing.Size(559, 363);
		    this.Controls.Add(this.tabsData);
		    this.Controls.Add(this.btnAccept);
		    this.Controls.Add(this.btnCancel);
		    this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
		    this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
		    this.Margin = new System.Windows.Forms.Padding(4);
		    this.MaximizeBox = false;
		    this.MinimizeBox = false;
		    this.Name = "SourceCitEditDlg";
		    this.ShowInTaskbar = false;
		    this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
		    this.Text = "SourceCitEditDlg";
		    this.Title = "SourceCitEditDlg";
		    this.tabsData.ResumeLayout(false);
		    this.pageCommon.ResumeLayout(false);
		    this.pageCommon.PerformLayout();
		    this.pageOther.ResumeLayout(false);
		    this.pageOther.PerformLayout();
		    this.ResumeLayout(false);

		}
	}
}
