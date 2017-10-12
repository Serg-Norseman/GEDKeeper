namespace GKUI.Forms
{
	partial class ProgressDlg
	{
		private System.Windows.Forms.Label lblTimePassed;
		private System.Windows.Forms.Label lblTimeRemain;
		private System.Windows.Forms.Label lblPassedVal;
		private System.Windows.Forms.Label lblRemainVal;
		private System.Windows.Forms.Label lblTimeTotal;
		private System.Windows.Forms.Label lblTotalVal;
		private System.Windows.Forms.ProgressBar ProgressBar1;
		private System.Windows.Forms.Label lblTitle;

		private void InitializeComponent()
		{
		    this.ProgressBar1 = new System.Windows.Forms.ProgressBar();
		    this.lblTitle = new System.Windows.Forms.Label();
		    this.lblTimePassed = new System.Windows.Forms.Label();
		    this.lblTimeRemain = new System.Windows.Forms.Label();
		    this.lblPassedVal = new System.Windows.Forms.Label();
		    this.lblRemainVal = new System.Windows.Forms.Label();
		    this.lblTimeTotal = new System.Windows.Forms.Label();
		    this.lblTotalVal = new System.Windows.Forms.Label();
		    this.SuspendLayout();
		    // 
		    // ProgressBar1
		    // 
		    this.ProgressBar1.Location = new System.Drawing.Point(16, 37);
		    this.ProgressBar1.Margin = new System.Windows.Forms.Padding(4);
		    this.ProgressBar1.Name = "ProgressBar1";
		    this.ProgressBar1.Size = new System.Drawing.Size(497, 20);
		    this.ProgressBar1.Step = 1;
		    this.ProgressBar1.Style = System.Windows.Forms.ProgressBarStyle.Continuous;
		    this.ProgressBar1.TabIndex = 0;
		    // 
		    // lblTitle
		    // 
		    this.lblTitle.AutoSize = true;
		    this.lblTitle.Location = new System.Drawing.Point(16, 16);
		    this.lblTitle.Margin = new System.Windows.Forms.Padding(8, 8, 8, 0);
		    this.lblTitle.Name = "lblTitle";
		    this.lblTitle.Size = new System.Drawing.Size(47, 17);
		    this.lblTitle.TabIndex = 1;
		    this.lblTitle.Text = "lblTitle";
		    // 
		    // lblTimePassed
		    // 
		    this.lblTimePassed.AutoSize = true;
		    this.lblTimePassed.Location = new System.Drawing.Point(16, 69);
		    this.lblTimePassed.Margin = new System.Windows.Forms.Padding(4);
		    this.lblTimePassed.Name = "lblTimePassed";
		    this.lblTimePassed.Size = new System.Drawing.Size(117, 17);
		    this.lblTimePassed.TabIndex = 2;
		    this.lblTimePassed.Text = "lblTimePassed";
		    // 
		    // lblTimeRemain
		    // 
		    this.lblTimeRemain.AutoSize = true;
		    this.lblTimeRemain.Location = new System.Drawing.Point(16, 94);
		    this.lblTimeRemain.Margin = new System.Windows.Forms.Padding(4);
		    this.lblTimeRemain.Name = "lblTimeRemain";
		    this.lblTimeRemain.Size = new System.Drawing.Size(127, 17);
		    this.lblTimeRemain.TabIndex = 3;
		    this.lblTimeRemain.Text = "lblTimeRemain";
		    // 
		    // lblPassedVal
		    // 
		    this.lblPassedVal.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
		    this.lblPassedVal.Location = new System.Drawing.Point(232, 69);
		    this.lblPassedVal.Margin = new System.Windows.Forms.Padding(4);
		    this.lblPassedVal.Name = "lblPassedVal";
		    this.lblPassedVal.Size = new System.Drawing.Size(281, 20);
		    this.lblPassedVal.TabIndex = 4;
		    this.lblPassedVal.Text = "lblPassedVal";
		    this.lblPassedVal.TextAlign = System.Drawing.ContentAlignment.TopRight;
		    // 
		    // lblRemainVal
		    // 
		    this.lblRemainVal.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
		    this.lblRemainVal.Location = new System.Drawing.Point(232, 94);
		    this.lblRemainVal.Margin = new System.Windows.Forms.Padding(4);
		    this.lblRemainVal.Name = "lblRemainVal";
		    this.lblRemainVal.Size = new System.Drawing.Size(281, 20);
		    this.lblRemainVal.TabIndex = 5;
		    this.lblRemainVal.Text = "lblRemainVal";
		    this.lblRemainVal.TextAlign = System.Drawing.ContentAlignment.TopRight;
		    // 
		    // lblTimeTotal
		    // 
		    this.lblTimeTotal.AutoSize = true;
		    this.lblTimeTotal.Location = new System.Drawing.Point(16, 119);
		    this.lblTimeTotal.Margin = new System.Windows.Forms.Padding(4);
		    this.lblTimeTotal.Name = "lblTimeTotal";
		    this.lblTimeTotal.Size = new System.Drawing.Size(102, 17);
		    this.lblTimeTotal.TabIndex = 6;
		    this.lblTimeTotal.Text = "lblTimeTotal";
		    // 
		    // lblTotalVal
		    // 
		    this.lblTotalVal.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
		    this.lblTotalVal.Location = new System.Drawing.Point(232, 119);
		    this.lblTotalVal.Margin = new System.Windows.Forms.Padding(4);
		    this.lblTotalVal.Name = "lblTotalVal";
		    this.lblTotalVal.Size = new System.Drawing.Size(281, 20);
		    this.lblTotalVal.TabIndex = 7;
		    this.lblTotalVal.Text = "lblTotalVal";
		    this.lblTotalVal.TextAlign = System.Drawing.ContentAlignment.TopRight;
		    // 
		    // ProgressDlg
		    // 
		    this.AutoScaleDimensions = new System.Drawing.SizeF(120F, 120F);
		    this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
		    this.AutoSize = true;
		    this.ClientSize = new System.Drawing.Size(531, 152);
		    this.ControlBox = false;
		    this.Controls.Add(this.ProgressBar1);
		    this.Controls.Add(this.lblTitle);
		    this.Controls.Add(this.lblTimePassed);
		    this.Controls.Add(this.lblTimeRemain);
		    this.Controls.Add(this.lblPassedVal);
		    this.Controls.Add(this.lblRemainVal);
		    this.Controls.Add(this.lblTimeTotal);
		    this.Controls.Add(this.lblTotalVal);
		    this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
		    this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedSingle;
		    this.Margin = new System.Windows.Forms.Padding(4);
		    this.MaximizeBox = false;
		    this.MinimizeBox = false;
		    this.Name = "ProgressDlg";
		    this.Padding = new System.Windows.Forms.Padding(8);
		    this.ShowInTaskbar = false;
		    this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
		    this.Text = "ProgressDlg";
		    this.TopMost = true;
		    this.ResumeLayout(false);
		    this.PerformLayout();

		}
	}
}