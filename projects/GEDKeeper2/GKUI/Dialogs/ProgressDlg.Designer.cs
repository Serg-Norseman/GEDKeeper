namespace GKUI.Dialogs
{
	partial class ProgressDlg
	{
		private System.Windows.Forms.Label Label2;
		private System.Windows.Forms.Label Label3;
		private System.Windows.Forms.Label Label7;
		private System.Windows.Forms.Label Label8;
		private System.Windows.Forms.Label Label4;
		private System.Windows.Forms.Label Label9;
		private System.Windows.Forms.ProgressBar ProgressBar1;
		private System.Windows.Forms.Label Label1;

		private void InitializeComponent()
		{
		    this.ProgressBar1 = new System.Windows.Forms.ProgressBar();
		    this.Label1 = new System.Windows.Forms.Label();
		    this.Label2 = new System.Windows.Forms.Label();
		    this.Label3 = new System.Windows.Forms.Label();
		    this.Label7 = new System.Windows.Forms.Label();
		    this.Label8 = new System.Windows.Forms.Label();
		    this.Label4 = new System.Windows.Forms.Label();
		    this.Label9 = new System.Windows.Forms.Label();
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
		    // Label1
		    // 
		    this.Label1.AutoSize = true;
		    this.Label1.Location = new System.Drawing.Point(16, 16);
		    this.Label1.Margin = new System.Windows.Forms.Padding(8, 8, 8, 0);
		    this.Label1.Name = "Label1";
		    this.Label1.Size = new System.Drawing.Size(47, 17);
		    this.Label1.TabIndex = 1;
		    this.Label1.Text = "Label1";
		    // 
		    // Label2
		    // 
		    this.Label2.AutoSize = true;
		    this.Label2.Location = new System.Drawing.Point(16, 69);
		    this.Label2.Margin = new System.Windows.Forms.Padding(4);
		    this.Label2.Name = "Label2";
		    this.Label2.Size = new System.Drawing.Size(117, 17);
		    this.Label2.TabIndex = 2;
		    this.Label2.Text = "Времени прошло";
		    // 
		    // Label3
		    // 
		    this.Label3.AutoSize = true;
		    this.Label3.Location = new System.Drawing.Point(16, 94);
		    this.Label3.Margin = new System.Windows.Forms.Padding(4);
		    this.Label3.Name = "Label3";
		    this.Label3.Size = new System.Drawing.Size(127, 17);
		    this.Label3.TabIndex = 3;
		    this.Label3.Text = "Времени осталось";
		    // 
		    // Label7
		    // 
		    this.Label7.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
		    this.Label7.Location = new System.Drawing.Point(232, 69);
		    this.Label7.Margin = new System.Windows.Forms.Padding(4);
		    this.Label7.Name = "Label7";
		    this.Label7.Size = new System.Drawing.Size(281, 20);
		    this.Label7.TabIndex = 4;
		    this.Label7.Text = "?";
		    this.Label7.TextAlign = System.Drawing.ContentAlignment.TopRight;
		    // 
		    // Label8
		    // 
		    this.Label8.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
		    this.Label8.Location = new System.Drawing.Point(232, 94);
		    this.Label8.Margin = new System.Windows.Forms.Padding(4);
		    this.Label8.Name = "Label8";
		    this.Label8.Size = new System.Drawing.Size(281, 20);
		    this.Label8.TabIndex = 5;
		    this.Label8.Text = "?";
		    this.Label8.TextAlign = System.Drawing.ContentAlignment.TopRight;
		    // 
		    // Label4
		    // 
		    this.Label4.AutoSize = true;
		    this.Label4.Location = new System.Drawing.Point(16, 119);
		    this.Label4.Margin = new System.Windows.Forms.Padding(4);
		    this.Label4.Name = "Label4";
		    this.Label4.Size = new System.Drawing.Size(102, 17);
		    this.Label4.TabIndex = 6;
		    this.Label4.Text = "Времени всего";
		    // 
		    // Label9
		    // 
		    this.Label9.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
		    this.Label9.Location = new System.Drawing.Point(232, 119);
		    this.Label9.Margin = new System.Windows.Forms.Padding(4);
		    this.Label9.Name = "Label9";
		    this.Label9.Size = new System.Drawing.Size(281, 20);
		    this.Label9.TabIndex = 7;
		    this.Label9.Text = "?";
		    this.Label9.TextAlign = System.Drawing.ContentAlignment.TopRight;
		    // 
		    // TfmProgress
		    // 
		    this.AutoScaleDimensions = new System.Drawing.SizeF(120F, 120F);
		    this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
		    this.AutoSize = true;
		    this.ClientSize = new System.Drawing.Size(531, 152);
		    this.ControlBox = false;
		    this.Controls.Add(this.ProgressBar1);
		    this.Controls.Add(this.Label1);
		    this.Controls.Add(this.Label2);
		    this.Controls.Add(this.Label3);
		    this.Controls.Add(this.Label7);
		    this.Controls.Add(this.Label8);
		    this.Controls.Add(this.Label4);
		    this.Controls.Add(this.Label9);
		    this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
		    this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedSingle;
		    this.Margin = new System.Windows.Forms.Padding(4);
		    this.MaximizeBox = false;
		    this.MinimizeBox = false;
		    this.Name = "TfmProgress";
		    this.Padding = new System.Windows.Forms.Padding(8);
		    this.ShowInTaskbar = false;
		    this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
		    this.Text = "Прогресс";
		    this.TopMost = true;
		    this.ResumeLayout(false);
		    this.PerformLayout();

		}
	}
}