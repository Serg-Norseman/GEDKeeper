namespace GKUI.Forms
{
    partial class ParentsEditDlg
    {
        private System.Windows.Forms.Button btnAccept;
        private System.Windows.Forms.Button btnCancel;
        private System.Windows.Forms.GroupBox GroupBox1;
        private System.Windows.Forms.Label lblChildName;
        private System.Windows.Forms.TextBox txtChildName;
        private System.Windows.Forms.Label lblParents;
        private System.Windows.Forms.TextBox txtFather;
        private System.Windows.Forms.TextBox txtMother;
        private System.Windows.Forms.Button btnParentsEdit;
        private System.Windows.Forms.Button btnFatherAdd;
        private System.Windows.Forms.Button btnFatherDelete;
        private System.Windows.Forms.Button btnMotherAdd;
        private System.Windows.Forms.Button btnMotherDelete;
        private System.Windows.Forms.Label lblLinkageType;
        private System.Windows.Forms.ComboBox cmbLinkageType;
        
        private void InitializeComponent()
        {
            this.btnAccept = new System.Windows.Forms.Button();
            this.btnCancel = new System.Windows.Forms.Button();
            this.GroupBox1 = new System.Windows.Forms.GroupBox();
            this.lblChildName = new System.Windows.Forms.Label();
            this.txtChildName = new System.Windows.Forms.TextBox();
            this.lblLinkageType = new System.Windows.Forms.Label();
            this.cmbLinkageType = new System.Windows.Forms.ComboBox();
            this.txtMother = new System.Windows.Forms.TextBox();
            this.lblParents = new System.Windows.Forms.Label();
            this.btnParentsEdit = new System.Windows.Forms.Button();
            this.btnFatherAdd = new System.Windows.Forms.Button();
            this.btnFatherDelete = new System.Windows.Forms.Button();
            this.btnMotherAdd = new System.Windows.Forms.Button();
            this.btnMotherDelete = new System.Windows.Forms.Button();
            this.txtFather = new System.Windows.Forms.TextBox();
            this.GroupBox1.SuspendLayout();
            this.SuspendLayout();
            // 
            // btnAccept
            // 
            this.btnAccept.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.btnAccept.Location = new System.Drawing.Point(628, 197);
            this.btnAccept.Margin = new System.Windows.Forms.Padding(2);
            this.btnAccept.Name = "btnAccept";
            this.btnAccept.Size = new System.Drawing.Size(112, 30);
            this.btnAccept.TabIndex = 5;
            this.btnAccept.Text = "btnAccept";
            this.btnAccept.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            this.btnAccept.Click += new System.EventHandler(this.AcceptClickHandler);
            // 
            // btnCancel
            // 
            this.btnCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.btnCancel.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.btnCancel.Location = new System.Drawing.Point(751, 197);
            this.btnCancel.Margin = new System.Windows.Forms.Padding(2);
            this.btnCancel.Name = "btnCancel";
            this.btnCancel.Size = new System.Drawing.Size(112, 30);
            this.btnCancel.TabIndex = 6;
            this.btnCancel.Text = "btnCancel";
            this.btnCancel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            this.btnCancel.Click += new System.EventHandler(this.CancelClickHandler);
            // 
            // GroupBox1
            // 
            this.GroupBox1.Controls.Add(this.lblChildName);
            this.GroupBox1.Controls.Add(this.txtChildName);
            this.GroupBox1.Controls.Add(this.lblLinkageType);
            this.GroupBox1.Controls.Add(this.cmbLinkageType);
            this.GroupBox1.Controls.Add(this.txtMother);
            this.GroupBox1.Controls.Add(this.lblParents);
            this.GroupBox1.Controls.Add(this.btnParentsEdit);
            this.GroupBox1.Controls.Add(this.btnFatherAdd);
            this.GroupBox1.Controls.Add(this.btnFatherDelete);
            this.GroupBox1.Controls.Add(this.btnMotherAdd);
            this.GroupBox1.Controls.Add(this.btnMotherDelete);
            this.GroupBox1.Controls.Add(this.txtFather);
            this.GroupBox1.Dock = System.Windows.Forms.DockStyle.Top;
            this.GroupBox1.Location = new System.Drawing.Point(0, 0);
            this.GroupBox1.Margin = new System.Windows.Forms.Padding(2);
            this.GroupBox1.Name = "GroupBox1";
            this.GroupBox1.Padding = new System.Windows.Forms.Padding(2);
            this.GroupBox1.Size = new System.Drawing.Size(874, 182);
            this.GroupBox1.TabIndex = 0;
            this.GroupBox1.TabStop = false;
            // 
            // lblChildName
            // 
            this.lblChildName.AutoSize = true;
            this.lblChildName.Location = new System.Drawing.Point(11, 24);
            this.lblChildName.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.lblChildName.Name = "lblChildName";
            this.lblChildName.Size = new System.Drawing.Size(84, 17);
            this.lblChildName.TabIndex = 0;
            this.lblChildName.Text = "lblChildName";
            // 
            // txtChildName
            // 
            this.txtChildName.Location = new System.Drawing.Point(117, 21);
            this.txtChildName.Margin = new System.Windows.Forms.Padding(2, 2, 20, 2);
            this.txtChildName.Name = "txtChildName";
            this.txtChildName.ReadOnly = true;
            this.txtChildName.Size = new System.Drawing.Size(746, 24);
            this.txtChildName.TabIndex = 1;
            // 
            // lblLinkageType
            // 
            this.lblLinkageType.AutoSize = true;
            this.lblLinkageType.Location = new System.Drawing.Point(11, 63);
            this.lblLinkageType.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.lblLinkageType.Name = "lblLinkageType";
            this.lblLinkageType.Size = new System.Drawing.Size(97, 17);
            this.lblLinkageType.TabIndex = 12;
            this.lblLinkageType.Text = "lblLinkageType";
            // 
            // cmbLinkageType
            // 
            this.cmbLinkageType.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cmbLinkageType.Location = new System.Drawing.Point(117, 60);
            this.cmbLinkageType.Margin = new System.Windows.Forms.Padding(2);
            this.cmbLinkageType.Name = "cmbLinkageType";
            this.cmbLinkageType.Size = new System.Drawing.Size(313, 25);
            this.cmbLinkageType.TabIndex = 13;
            // 
            // txtMother
            // 
            this.txtMother.ForeColor = System.Drawing.SystemColors.Control;
            this.txtMother.Location = new System.Drawing.Point(443, 100);
            this.txtMother.Margin = new System.Windows.Forms.Padding(2);
            this.txtMother.Name = "txtMother";
            this.txtMother.ReadOnly = true;
            this.txtMother.Size = new System.Drawing.Size(314, 24);
            this.txtMother.TabIndex = 2;
            // 
            // lblParents
            // 
            this.lblParents.AutoSize = true;
            this.lblParents.Location = new System.Drawing.Point(11, 103);
            this.lblParents.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.lblParents.Name = "lblParents";
            this.lblParents.Size = new System.Drawing.Size(66, 17);
            this.lblParents.TabIndex = 0;
            this.lblParents.Text = "lblParents";
            // 
            // btnParentsEdit
            // 
            this.btnParentsEdit.Location = new System.Drawing.Point(827, 93);
            this.btnParentsEdit.Margin = new System.Windows.Forms.Padding(2);
            this.btnParentsEdit.Name = "btnParentsEdit";
            this.btnParentsEdit.Size = new System.Drawing.Size(36, 36);
            this.btnParentsEdit.TabIndex = 4;
            this.btnParentsEdit.Click += new System.EventHandler(this.btnParentsEdit_Click);
            // 
            // btnFatherAdd
            // 
            this.btnFatherAdd.Location = new System.Drawing.Point(350, 128);
            this.btnFatherAdd.Margin = new System.Windows.Forms.Padding(2);
            this.btnFatherAdd.Name = "btnFatherAdd";
            this.btnFatherAdd.Size = new System.Drawing.Size(36, 36);
            this.btnFatherAdd.TabIndex = 6;
            this.btnFatherAdd.Click += new System.EventHandler(this.btnFatherAdd_Click);
            // 
            // btnFatherDelete
            // 
            this.btnFatherDelete.Location = new System.Drawing.Point(394, 128);
            this.btnFatherDelete.Margin = new System.Windows.Forms.Padding(2);
            this.btnFatherDelete.Name = "btnFatherDelete";
            this.btnFatherDelete.Size = new System.Drawing.Size(36, 36);
            this.btnFatherDelete.TabIndex = 7;
            this.btnFatherDelete.Click += new System.EventHandler(this.btnFatherDelete_Click);
            // 
            // btnMotherAdd
            // 
            this.btnMotherAdd.Location = new System.Drawing.Point(675, 128);
            this.btnMotherAdd.Margin = new System.Windows.Forms.Padding(2);
            this.btnMotherAdd.Name = "btnMotherAdd";
            this.btnMotherAdd.Size = new System.Drawing.Size(36, 36);
            this.btnMotherAdd.TabIndex = 9;
            this.btnMotherAdd.Click += new System.EventHandler(this.btnMotherAdd_Click);
            // 
            // btnMotherDelete
            // 
            this.btnMotherDelete.Location = new System.Drawing.Point(721, 128);
            this.btnMotherDelete.Margin = new System.Windows.Forms.Padding(2);
            this.btnMotherDelete.Name = "btnMotherDelete";
            this.btnMotherDelete.Size = new System.Drawing.Size(36, 36);
            this.btnMotherDelete.TabIndex = 10;
            this.btnMotherDelete.Click += new System.EventHandler(this.btnMotherDelete_Click);
            // 
            // txtFather
            // 
            this.txtFather.ForeColor = System.Drawing.SystemColors.Control;
            this.txtFather.Location = new System.Drawing.Point(117, 100);
            this.txtFather.Margin = new System.Windows.Forms.Padding(2);
            this.txtFather.Name = "txtFather";
            this.txtFather.ReadOnly = true;
            this.txtFather.Size = new System.Drawing.Size(313, 24);
            this.txtFather.TabIndex = 1;
            // 
            // ParentsEditDlg
            // 
            this.AcceptButton = this.btnAccept;
            this.AutoScaleDimensions = new System.Drawing.SizeF(120F, 120F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
            this.CancelButton = this.btnCancel;
            this.Title = "ParentsEditDlg";
            this.ClientSize = new System.Drawing.Size(874, 239);
            this.Controls.Add(this.btnAccept);
            this.Controls.Add(this.btnCancel);
            this.Controls.Add(this.GroupBox1);
            this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
            this.Margin = new System.Windows.Forms.Padding(2);
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.Name = "ParentsEditDlg";
            this.ShowInTaskbar = false;
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
            this.Text = "ParentsEditDlg";
            this.GroupBox1.ResumeLayout(false);
            this.GroupBox1.PerformLayout();
            this.ResumeLayout(false);

        }
    }
}
