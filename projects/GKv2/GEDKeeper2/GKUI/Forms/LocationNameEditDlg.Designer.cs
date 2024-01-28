namespace GKUI.Forms
{
    partial class LocationNameEditDlg
    {
        private System.Windows.Forms.Button btnAccept;
        private System.Windows.Forms.Button btnCancel;
        private System.Windows.Forms.Label lblTitle;
        private System.Windows.Forms.TextBox txtTitle;
        private System.Windows.Forms.Label lblShortTitle;
        private System.Windows.Forms.TextBox txtShortTitle;
        private System.Windows.Forms.Label lblDate;
        private Components.GKDateControl dateCtl;

        private void InitializeComponent()
        {
            this.btnAccept = new System.Windows.Forms.Button();
            this.btnCancel = new System.Windows.Forms.Button();
            this.lblTitle = new System.Windows.Forms.Label();
            this.txtTitle = new System.Windows.Forms.TextBox();
            this.lblShortTitle = new System.Windows.Forms.Label();
            this.txtShortTitle = new System.Windows.Forms.TextBox();
            this.lblDate = new System.Windows.Forms.Label();
            this.dateCtl = new GKUI.Components.GKDateControl();
            this.SuspendLayout();
            // 
            // btnAccept
            // 
            this.btnAccept.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.btnAccept.Location = new System.Drawing.Point(504, 155);
            this.btnAccept.Name = "btnAccept";
            this.btnAccept.Size = new System.Drawing.Size(113, 31);
            this.btnAccept.TabIndex = 6;
            this.btnAccept.Text = "btnAccept";
            this.btnAccept.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            this.btnAccept.Click += new System.EventHandler(this.AcceptClickHandler);
            // 
            // btnCancel
            // 
            this.btnCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.btnCancel.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.btnCancel.Location = new System.Drawing.Point(627, 155);
            this.btnCancel.Name = "btnCancel";
            this.btnCancel.Size = new System.Drawing.Size(114, 31);
            this.btnCancel.TabIndex = 7;
            this.btnCancel.Text = "btnCancel";
            this.btnCancel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            this.btnCancel.Click += new System.EventHandler(this.CancelClickHandler);
            // 
            // lblTitle
            // 
            this.lblTitle.AutoSize = true;
            this.lblTitle.Location = new System.Drawing.Point(11, 10);
            this.lblTitle.Name = "lblTitle";
            this.lblTitle.Size = new System.Drawing.Size(44, 17);
            this.lblTitle.TabIndex = 0;
            this.lblTitle.Text = "lblTitle";
            // 
            // txtTitle
            // 
            this.txtTitle.Location = new System.Drawing.Point(157, 10);
            this.txtTitle.Name = "txtTitle";
            this.txtTitle.ScrollBars = System.Windows.Forms.ScrollBars.Vertical;
            this.txtTitle.Size = new System.Drawing.Size(572, 127);
            this.txtTitle.TabIndex = 1;
            // 
            // lblShortTitle
            // 
            this.lblShortTitle.AutoSize = true;
            this.lblShortTitle.Location = new System.Drawing.Point(11, 40);
            this.lblShortTitle.Name = "lblShortTitle";
            this.lblShortTitle.Size = new System.Drawing.Size(78, 17);
            this.lblShortTitle.TabIndex = 2;
            this.lblShortTitle.Text = "lblShortTitle";
            // 
            // txtShortTitle
            // 
            this.txtShortTitle.Location = new System.Drawing.Point(157, 40);
            this.txtShortTitle.Name = "txtShortTitle";
            this.txtShortTitle.Size = new System.Drawing.Size(572, 24);
            this.txtShortTitle.TabIndex = 3;
            // 
            // lblDate
            // 
            this.lblDate.AutoSize = true;
            this.lblDate.Location = new System.Drawing.Point(11, 70);
            this.lblDate.Name = "lblDate";
            this.lblDate.Size = new System.Drawing.Size(49, 17);
            this.lblDate.TabIndex = 4;
            this.lblDate.Text = "lblDate";
            // 
            // dateCtl
            // 
            this.dateCtl.Location = new System.Drawing.Point(157, 70);
            this.dateCtl.Margin = new System.Windows.Forms.Padding(2);
            this.dateCtl.Name = "dateCtl";
            this.dateCtl.Size = new System.Drawing.Size(474, 63);
            this.dateCtl.TabIndex = 5;
            // 
            // LocationNameEditDlg
            // 
            this.AcceptButton = this.btnAccept;
            this.AutoScaleDimensions = new System.Drawing.SizeF(120F, 120F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
            this.CancelButton = this.btnCancel;
            this.ClientSize = new System.Drawing.Size(752, 199);
            this.Controls.Add(this.lblTitle);
            this.Controls.Add(this.txtTitle);
            this.Controls.Add(this.lblShortTitle);
            this.Controls.Add(this.txtShortTitle);
            this.Controls.Add(this.lblDate);
            this.Controls.Add(this.dateCtl);
            this.Controls.Add(this.btnAccept);
            this.Controls.Add(this.btnCancel);
            this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.Name = "LocationNameEditDlg";
            this.ShowInTaskbar = false;
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
            this.Text = "LocationNameEditDlg";
            this.ResumeLayout(false);
        }
    }
}
