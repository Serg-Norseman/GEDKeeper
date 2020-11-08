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

		private void InitializeComponent()
		{
		    this.btnAccept = new System.Windows.Forms.Button();
		    this.btnCancel = new System.Windows.Forms.Button();
		    this.lblPage = new System.Windows.Forms.Label();
		    this.txtPage = new System.Windows.Forms.TextBox();
		    this.lblSource = new System.Windows.Forms.Label();
		    this.btnSourceAdd = new System.Windows.Forms.Button();
		    this.lblCertainty = new System.Windows.Forms.Label();
		    this.txtCertainty = new System.Windows.Forms.ComboBox();
		    this.cmbSource = new System.Windows.Forms.ComboBox();
		    this.SuspendLayout();
		    // 
		    // btnAccept
		    // 
		    this.btnAccept.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
		    this.btnAccept.Location = new System.Drawing.Point(216, 204);
		    this.btnAccept.Margin = new System.Windows.Forms.Padding(4);
		    this.btnAccept.Name = "btnAccept";
		    this.btnAccept.Size = new System.Drawing.Size(101, 31);
		    this.btnAccept.TabIndex = 7;
		    this.btnAccept.Text = "btnAccept";
		    this.btnAccept.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
		    this.btnAccept.Click += new System.EventHandler(this.btnAccept_Click);
		    // 
		    // btnCancel
		    // 
		    this.btnCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
		    this.btnCancel.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
		    this.btnCancel.Location = new System.Drawing.Point(325, 204);
		    this.btnCancel.Margin = new System.Windows.Forms.Padding(4);
		    this.btnCancel.Name = "btnCancel";
		    this.btnCancel.Size = new System.Drawing.Size(101, 31);
		    this.btnCancel.TabIndex = 8;
		    this.btnCancel.Text = "btnCancel";
		    this.btnCancel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            this.btnCancel.Click += new System.EventHandler(this.btnCancel_Click);
		    // 
		    // lblPage
		    // 
		    this.lblPage.AutoSize = true;
		    this.lblPage.Location = new System.Drawing.Point(15, 70);
		    this.lblPage.Margin = new System.Windows.Forms.Padding(4, 0, 4, 0);
		    this.lblPage.Name = "lblPage";
		    this.lblPage.Size = new System.Drawing.Size(50, 17);
		    this.lblPage.TabIndex = 3;
		    this.lblPage.Text = "lblPage";
		    // 
		    // txtPage
		    // 
		    this.txtPage.Location = new System.Drawing.Point(15, 90);
		    this.txtPage.Margin = new System.Windows.Forms.Padding(4);
		    this.txtPage.Name = "txtPage";
		    this.txtPage.Size = new System.Drawing.Size(410, 24);
		    this.txtPage.TabIndex = 4;
		    // 
		    // lblSource
		    // 
		    this.lblSource.AutoSize = true;
		    this.lblSource.Location = new System.Drawing.Point(15, 10);
		    this.lblSource.Margin = new System.Windows.Forms.Padding(4, 0, 4, 0);
		    this.lblSource.Name = "lblSource";
		    this.lblSource.Size = new System.Drawing.Size(63, 17);
		    this.lblSource.TabIndex = 0;
		    this.lblSource.Text = "lblSource";
		    // 
		    // btnSourceAdd
		    // 
		    this.btnSourceAdd.Location = new System.Drawing.Point(391, 24);
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
		    this.lblCertainty.Location = new System.Drawing.Point(15, 130);
		    this.lblCertainty.Margin = new System.Windows.Forms.Padding(4, 0, 4, 0);
		    this.lblCertainty.Name = "lblCertainty";
		    this.lblCertainty.Size = new System.Drawing.Size(76, 17);
		    this.lblCertainty.TabIndex = 5;
		    this.lblCertainty.Text = "lblCertainty";
		    // 
		    // txtCertainty
		    // 
		    this.txtCertainty.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
		    this.txtCertainty.Location = new System.Drawing.Point(15, 150);
		    this.txtCertainty.Margin = new System.Windows.Forms.Padding(4);
		    this.txtCertainty.Name = "txtCertainty";
		    this.txtCertainty.Size = new System.Drawing.Size(410, 25);
		    this.txtCertainty.TabIndex = 6;
		    // 
		    // cmbSource
		    // 
		    this.cmbSource.Location = new System.Drawing.Point(15, 30);
		    this.cmbSource.Margin = new System.Windows.Forms.Padding(4);
		    this.cmbSource.Name = "cmbSource";
		    this.cmbSource.Size = new System.Drawing.Size(368, 25);
		    this.cmbSource.Sorted = true;
		    this.cmbSource.TabIndex = 1;
		    this.cmbSource.KeyDown += new System.Windows.Forms.KeyEventHandler(this.cbSource_KeyDown);
		    this.cmbSource.KeyUp += new System.Windows.Forms.KeyEventHandler(this.cbSource_KeyUp);
		    // 
		    // SourceCitEditDlg
		    // 
		    this.AcceptButton = this.btnAccept;
		    this.AutoScaleDimensions = new System.Drawing.SizeF(120F, 120F);
		    this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
		    this.CancelButton = this.btnCancel;
		    this.ClientSize = new System.Drawing.Size(441, 250);
		    this.Controls.Add(this.btnAccept);
		    this.Controls.Add(this.btnCancel);
		    this.Controls.Add(this.lblPage);
		    this.Controls.Add(this.txtPage);
		    this.Controls.Add(this.lblSource);
		    this.Controls.Add(this.btnSourceAdd);
		    this.Controls.Add(this.lblCertainty);
		    this.Controls.Add(this.txtCertainty);
		    this.Controls.Add(this.cmbSource);
		    this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
		    this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
		    this.Margin = new System.Windows.Forms.Padding(4);
		    this.MaximizeBox = false;
		    this.MinimizeBox = false;
		    this.Name = "SourceCitEditDlg";
		    this.ShowInTaskbar = false;
		    this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
		    this.Text = "SourceCitEditDlg";
		    this.ResumeLayout(false);
		    this.PerformLayout();
		}
	}
}
