namespace GKUI.Forms
{
    partial class RecordSelectDlg
    {
        private System.Windows.Forms.Button btnSelect;
        private System.Windows.Forms.Button btnCreate;
        private System.Windows.Forms.Button btnCancel;
        private System.Windows.Forms.Panel panList;
        private System.Windows.Forms.Panel panFilter;
        public System.Windows.Forms.ComboBox txtFastFilter;
        private GKUI.Components.GKFilterControl fltCtl;

        private void InitializeComponent()
        {
            this.btnSelect = new System.Windows.Forms.Button();
            this.btnCreate = new System.Windows.Forms.Button();
            this.btnCancel = new System.Windows.Forms.Button();
            this.panFilter = new System.Windows.Forms.Panel();
            this.txtFastFilter = new System.Windows.Forms.ComboBox();
            this.panList = new System.Windows.Forms.Panel();
            this.fltCtl = new GKUI.Components.GKFilterControl();
            this.panFilter.SuspendLayout();
            this.SuspendLayout();
            // 
            // btnSelect
            // 
            this.btnSelect.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.btnSelect.Location = new System.Drawing.Point(280, 466);
            this.btnSelect.Name = "btnSelect";
            this.btnSelect.Size = new System.Drawing.Size(113, 31);
            this.btnSelect.TabIndex = 2;
            this.btnSelect.Text = "btnSelect";
            this.btnSelect.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            this.btnSelect.Click += new System.EventHandler(this.btnSelect_Click);
            // 
            // btnCreate
            //
            this.btnCreate.Location = new System.Drawing.Point(146, 466);
            this.btnCreate.Name = "btnCreate";
            this.btnCreate.Size = new System.Drawing.Size(113, 31);
            this.btnCreate.TabIndex = 3;
            this.btnCreate.Text = "btnCreate";
            this.btnCreate.Click += new System.EventHandler(this.btnCreate_Click);
            // 
            // btnCancel
            // 
            this.btnCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.btnCancel.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.btnCancel.Location = new System.Drawing.Point(414, 466);
            this.btnCancel.Name = "btnCancel";
            this.btnCancel.Size = new System.Drawing.Size(114, 31);
            this.btnCancel.TabIndex = 4;
            this.btnCancel.Text = "btnCancel";
            this.btnCancel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            // 
            // panFilter
            // 
            this.panFilter.Controls.Add(this.txtFastFilter);
            this.panFilter.Controls.Add(this.fltCtl);
            this.panFilter.Dock = System.Windows.Forms.DockStyle.Top;
            this.panFilter.Location = new System.Drawing.Point(0, 0);
            this.panFilter.Name = "panFilter";
            this.panFilter.Size = new System.Drawing.Size(540, 50);
            this.panFilter.TabIndex = 0;
            // 
            // txtFastFilter
            // 
            this.txtFastFilter.Location = new System.Drawing.Point(11, 10);
            this.txtFastFilter.Name = "txtFastFilter";
            this.txtFastFilter.Size = new System.Drawing.Size(472, 24);
            this.txtFastFilter.TabIndex = 0;
            this.txtFastFilter.KeyDown += new System.Windows.Forms.KeyEventHandler(txtFastFilter_KeyDown);
            this.txtFastFilter.TextChanged += new System.EventHandler(this.txtFastFilter_TextChanged);
            // 
            // fltCtl
            // 
            this.fltCtl.Location = new System.Drawing.Point(491, 8);
            this.fltCtl.Name = "fltCtl";
            this.fltCtl.Size = new System.Drawing.Size(36, 32);
            this.fltCtl.TabIndex = 1;
            this.fltCtl.TabStop = false;
            // 
            // panList
            // 
            this.panList.Dock = System.Windows.Forms.DockStyle.Top;
            this.panList.Location = new System.Drawing.Point(0, 50);
            this.panList.Name = "panList";
            this.panList.Size = new System.Drawing.Size(540, 399);
            this.panList.TabIndex = 1;
            // 
            // RecordSelectDlg
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(120F, 120F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
            this.CancelButton = this.btnCancel;
            this.ClientSize = new System.Drawing.Size(540, 511);
            this.Controls.Add(this.panList);
            this.Controls.Add(this.panFilter);
            this.Controls.Add(this.btnSelect);
            this.Controls.Add(this.btnCreate);
            this.Controls.Add(this.btnCancel);
            this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
            this.KeyPreview = true;
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.Name = "RecordSelectDlg";
            this.ShowInTaskbar = false;
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
            this.Text = "RecordSelectDlg";
            this.panFilter.ResumeLayout(false);
            this.panFilter.PerformLayout();
            this.ResumeLayout(false);
        }
    }
}
