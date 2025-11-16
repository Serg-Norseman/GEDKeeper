namespace GKUI.Forms
{
    partial class MediaFileEditDlg
    {
        private System.Windows.Forms.Button btnAccept;
        private System.Windows.Forms.Button btnCancel;
        private System.Windows.Forms.Button btnView;
        private System.Windows.Forms.Label lblName;
        private System.Windows.Forms.TextBox txtName;
        private System.Windows.Forms.Label lblType;
        private System.Windows.Forms.ComboBox cmbMediaType;
        private System.Windows.Forms.Label lblStoreType;
        private System.Windows.Forms.ComboBox cmbStoreType;
        private System.Windows.Forms.Label lblFile;
        private System.Windows.Forms.TextBox txtFile;
        private System.Windows.Forms.Button btnFileSelect;

        private void InitializeComponent()
        {
            this.lblName = new System.Windows.Forms.Label();
            this.lblType = new System.Windows.Forms.Label();
            this.lblStoreType = new System.Windows.Forms.Label();
            this.lblFile = new System.Windows.Forms.Label();
            this.txtName = new System.Windows.Forms.TextBox();
            this.cmbMediaType = new System.Windows.Forms.ComboBox();
            this.cmbStoreType = new System.Windows.Forms.ComboBox();
            this.txtFile = new System.Windows.Forms.TextBox();
            this.btnFileSelect = new System.Windows.Forms.Button();
            this.btnAccept = new System.Windows.Forms.Button();
            this.btnCancel = new System.Windows.Forms.Button();
            this.btnView = new System.Windows.Forms.Button();
            this.SuspendLayout();
            // 
            // lblName
            // 
            this.lblName.AutoSize = true;
            this.lblName.Location = new System.Drawing.Point(11, 10);
            this.lblName.Name = "lblName";
            this.lblName.Size = new System.Drawing.Size(55, 17);
            this.lblName.TabIndex = 0;
            this.lblName.Text = "lblName";
            // 
            // lblType
            // 
            this.lblType.AutoSize = true;
            this.lblType.Location = new System.Drawing.Point(11, 115);
            this.lblType.Margin = new System.Windows.Forms.Padding(3, 6, 3, 0);
            this.lblType.Name = "lblType";
            this.lblType.Size = new System.Drawing.Size(51, 17);
            this.lblType.TabIndex = 5;
            this.lblType.Text = "lblType";
            // 
            // lblStoreType
            // 
            this.lblStoreType.AutoSize = true;
            this.lblStoreType.Location = new System.Drawing.Point(269, 115);
            this.lblStoreType.Margin = new System.Windows.Forms.Padding(3, 6, 3, 0);
            this.lblStoreType.Name = "lblStoreType";
            this.lblStoreType.Size = new System.Drawing.Size(84, 17);
            this.lblStoreType.TabIndex = 7;
            this.lblStoreType.Text = "lblStoreType";
            // 
            // lblFile
            // 
            this.lblFile.AutoSize = true;
            this.lblFile.Location = new System.Drawing.Point(11, 62);
            this.lblFile.Margin = new System.Windows.Forms.Padding(3, 6, 3, 0);
            this.lblFile.Name = "lblFile";
            this.lblFile.Size = new System.Drawing.Size(38, 17);
            this.lblFile.TabIndex = 2;
            this.lblFile.Text = "lblFile";
            // 
            // txtName
            // 
            this.txtName.Location = new System.Drawing.Point(11, 29);
            this.txtName.Name = "txtName";
            this.txtName.Size = new System.Drawing.Size(696, 24);
            this.txtName.TabIndex = 1;
            this.txtName.TextChanged += new System.EventHandler(this.edName_TextChanged);
            // 
            // cmbMediaType
            // 
            this.cmbMediaType.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cmbMediaType.DropDownWidth = 15;
            this.cmbMediaType.Location = new System.Drawing.Point(11, 135);
            this.cmbMediaType.Name = "cmbMediaType";
            this.cmbMediaType.Size = new System.Drawing.Size(237, 25);
            this.cmbMediaType.TabIndex = 6;
            // 
            // cmbStoreType
            // 
            this.cmbStoreType.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cmbStoreType.Location = new System.Drawing.Point(269, 135);
            this.cmbStoreType.Name = "cmbStoreType";
            this.cmbStoreType.Size = new System.Drawing.Size(281, 25);
            this.cmbStoreType.TabIndex = 8;
            this.cmbStoreType.SelectedIndexChanged += new System.EventHandler(cmbStoreType_SelectedIndexChanged);
            // 
            // txtFile
            // 
            this.txtFile.Location = new System.Drawing.Point(11, 82);
            this.txtFile.Name = "txtFile";
            this.txtFile.ReadOnly = true;
            this.txtFile.Size = new System.Drawing.Size(629, 24);
            this.txtFile.TabIndex = 3;
            // 
            // btnFileSelect
            // 
            this.btnFileSelect.Location = new System.Drawing.Point(650, 82);
            this.btnFileSelect.Name = "btnFileSelect";
            this.btnFileSelect.Size = new System.Drawing.Size(60, 25);
            this.btnFileSelect.TabIndex = 4;
            this.btnFileSelect.Text = "...";
            this.btnFileSelect.Click += new System.EventHandler(this.btnFileSelect_Click);
            // 
            // btnAccept
            // 
            this.btnAccept.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.btnAccept.Location = new System.Drawing.Point(482, 191);
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
            this.btnCancel.Location = new System.Drawing.Point(605, 191);
            this.btnCancel.Name = "btnCancel";
            this.btnCancel.Size = new System.Drawing.Size(113, 30);
            this.btnCancel.TabIndex = 3;
            this.btnCancel.Text = "btnCancel";
            this.btnCancel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            this.btnCancel.Click += new System.EventHandler(this.CancelClickHandler);
            // 
            // btnView
            // 
            this.btnView.Location = new System.Drawing.Point(11, 191);
            this.btnView.Name = "btnView";
            this.btnView.Size = new System.Drawing.Size(114, 30);
            this.btnView.TabIndex = 1;
            this.btnView.Text = "btnView";
            this.btnView.Click += new System.EventHandler(this.btnView_Click);
            // 
            // MediaEditDlg
            // 
            this.AcceptButton = this.btnAccept;
            this.AutoScaleDimensions = new System.Drawing.SizeF(120F, 120F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
            this.CancelButton = this.btnCancel;
            this.ClientSize = new System.Drawing.Size(728, 235);
            this.Controls.Add(this.lblName);
            this.Controls.Add(this.lblType);
            this.Controls.Add(this.lblStoreType);
            this.Controls.Add(this.lblFile);
            this.Controls.Add(this.txtName);
            this.Controls.Add(this.cmbMediaType);
            this.Controls.Add(this.cmbStoreType);
            this.Controls.Add(this.txtFile);
            this.Controls.Add(this.btnFileSelect);
            this.Controls.Add(this.btnAccept);
            this.Controls.Add(this.btnCancel);
            this.Controls.Add(this.btnView);
            this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.Name = "MediaFileEditDlg";
            this.ShowInTaskbar = false;
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
            this.Text = "MediaFileEditDlg";
            this.ResumeLayout(false);
        }
    }
}
