namespace GKUI.Forms
{
    partial class RecordInfoDlg
    {
        private GKUI.Components.HyperView hyperView1;

        private void InitializeComponent()
        {
            this.hyperView1 = new GKUI.Components.HyperView();
            this.SuspendLayout();
            // 
            // hyperView1
            // 
            this.hyperView1.AutoScroll = true;
            this.hyperView1.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
            this.hyperView1.BorderWidth = 0;
            this.hyperView1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.hyperView1.LinkColor = System.Drawing.Color.Blue;
            this.hyperView1.Location = new System.Drawing.Point(0, 0);
            this.hyperView1.Name = "hyperView1";
            this.hyperView1.OnLink += HyperViewLink;
            this.hyperView1.Size = new System.Drawing.Size(630, 405);
            this.hyperView1.TabIndex = 0;
            this.hyperView1.TabStop = true;
            this.hyperView1.WordWrap = true;
            // 
            // RecordInfoDlg
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(120F, 120F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
            this.Title = "RecordInfoDlg";
            this.ClientSize = new System.Drawing.Size(630, 405);
            this.Controls.Add(this.hyperView1);
            this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
            this.Margin = new System.Windows.Forms.Padding(2);
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.Name = "RecordInfoDlg";
            this.ShowInTaskbar = false;
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
            this.Text = "RecordInfoDlg";
            this.ResumeLayout(false);

        }
    }
}
