namespace GKUI.Forms
{
    partial class LocationLinkEditDlg
    {
        private System.Windows.Forms.Button btnAccept;
        private System.Windows.Forms.Button btnCancel;
        private System.Windows.Forms.Label lblLocation;
        private System.Windows.Forms.TextBox txtTopLevel;
        private System.Windows.Forms.Label lblDate;
        private Components.GKDateControl dateCtl;
        private System.Windows.Forms.Button btnLocationAdd;

        private void InitializeComponent()
        {
            this.btnAccept = new System.Windows.Forms.Button();
            this.btnCancel = new System.Windows.Forms.Button();
            this.lblLocation = new System.Windows.Forms.Label();
            this.txtTopLevel = new System.Windows.Forms.TextBox();
            this.lblDate = new System.Windows.Forms.Label();
            this.dateCtl = new GKUI.Components.GKDateControl();
            this.btnLocationAdd = new System.Windows.Forms.Button();
            this.SuspendLayout();
            // 
            // btnAccept
            // 
            this.btnAccept.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.btnAccept.Location = new System.Drawing.Point(504, 135);
            this.btnAccept.Name = "btnAccept";
            this.btnAccept.Size = new System.Drawing.Size(113, 31);
            this.btnAccept.TabIndex = 5;
            this.btnAccept.Text = "btnAccept";
            this.btnAccept.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            this.btnAccept.Click += new System.EventHandler(this.AcceptClickHandler);
            // 
            // btnCancel
            // 
            this.btnCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.btnCancel.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.btnCancel.Location = new System.Drawing.Point(627, 135);
            this.btnCancel.Name = "btnCancel";
            this.btnCancel.Size = new System.Drawing.Size(114, 31);
            this.btnCancel.TabIndex = 6;
            this.btnCancel.Text = "btnCancel";
            this.btnCancel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            this.btnCancel.Click += new System.EventHandler(this.CancelClickHandler);
            // 
            // btnLocationAdd
            // 
            this.btnLocationAdd.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            this.btnLocationAdd.Location = new System.Drawing.Point(592, 10);
            this.btnLocationAdd.Name = "btnLocationAdd";
            this.btnLocationAdd.Size = new System.Drawing.Size(39, 34);
            this.btnLocationAdd.TabIndex = 2;
            this.btnLocationAdd.Click += new System.EventHandler(this.btnLocationAdd_Click);
            // 
            // lblTitle
            // 
            this.lblLocation.AutoSize = true;
            this.lblLocation.Location = new System.Drawing.Point(11, 10);
            this.lblLocation.Name = "lblTitle";
            this.lblLocation.Size = new System.Drawing.Size(44, 17);
            this.lblLocation.TabIndex = 0;
            this.lblLocation.Text = "lblTitle";
            // 
            // txtTitle
            // 
            this.txtTopLevel.Location = new System.Drawing.Point(157, 10);
            this.txtTopLevel.Name = "txtTopLevel";
            this.txtTopLevel.ReadOnly = true;
            this.txtTopLevel.Size = new System.Drawing.Size(428, 127);
            this.txtTopLevel.TabIndex = 1;
            // 
            // lblDate
            // 
            this.lblDate.AutoSize = true;
            this.lblDate.Location = new System.Drawing.Point(11, 50);
            this.lblDate.Name = "lblDate";
            this.lblDate.Size = new System.Drawing.Size(49, 17);
            this.lblDate.TabIndex = 3;
            this.lblDate.Text = "lblDate";
            // 
            // dateCtl
            // 
            this.dateCtl.Location = new System.Drawing.Point(157, 50);
            this.dateCtl.Margin = new System.Windows.Forms.Padding(2);
            this.dateCtl.Name = "dateCtl";
            this.dateCtl.Size = new System.Drawing.Size(474, 63);
            this.dateCtl.TabIndex = 4;
            // 
            // LocationLinkEditDlg
            // 
            this.AcceptButton = this.btnAccept;
            this.AutoScaleDimensions = new System.Drawing.SizeF(120F, 120F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
            this.CancelButton = this.btnCancel;
            this.ClientSize = new System.Drawing.Size(752, 179);
            this.Controls.Add(this.lblLocation);
            this.Controls.Add(this.txtTopLevel);
            this.Controls.Add(this.lblDate);
            this.Controls.Add(this.dateCtl);
            this.Controls.Add(this.btnAccept);
            this.Controls.Add(this.btnCancel);
            this.Controls.Add(this.btnLocationAdd);
            this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.Name = "LocationLinkEditDlg";
            this.ShowInTaskbar = false;
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
            this.Text = "LocationLinkEditDlg";
            this.ResumeLayout(false);
        }
    }
}
