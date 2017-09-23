namespace GKUI.Dialogs
{
	partial class DayTipsDlg
	{
		private System.Windows.Forms.Panel Shape1;
		private System.Windows.Forms.CheckBox chkShow;
		private System.Windows.Forms.Button btnNextTip;
		private System.Windows.Forms.Button btnClose;
		private System.Windows.Forms.Panel Shape2;
		private System.Windows.Forms.Panel Shape3;
		private System.Windows.Forms.Label lblTitle;
		private System.Windows.Forms.PictureBox Image1;
		private System.Windows.Forms.TextBox txtTip;

		private void InitializeComponent()
		{
		    this.Shape1 = new System.Windows.Forms.Panel();
		    this.chkShow = new System.Windows.Forms.CheckBox();
		    this.btnNextTip = new System.Windows.Forms.Button();
		    this.btnClose = new System.Windows.Forms.Button();
		    this.Shape2 = new System.Windows.Forms.Panel();
		    this.Shape3 = new System.Windows.Forms.Panel();
		    this.lblTitle = new System.Windows.Forms.Label();
		    this.Image1 = new System.Windows.Forms.PictureBox();
		    this.txtTip = new System.Windows.Forms.TextBox();
		    ((System.ComponentModel.ISupportInitialize)(this.Image1)).BeginInit();
		    this.SuspendLayout();
		    // 
		    // Shape1
		    // 
		    this.Shape1.BackColor = System.Drawing.Color.White;
		    this.Shape1.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
		    this.Shape1.ForeColor = System.Drawing.Color.Black;
		    this.Shape1.Location = new System.Drawing.Point(123, 10);
		    this.Shape1.Name = "Shape1";
		    this.Shape1.Size = new System.Drawing.Size(405, 48);
		    this.Shape1.TabIndex = 0;
		    // 
		    // chkShow
		    // 
		    this.chkShow.Checked = true;
		    this.chkShow.CheckState = System.Windows.Forms.CheckState.Checked;
		    this.chkShow.Location = new System.Drawing.Point(25, 267);
		    this.chkShow.Name = "chkShow";
		    this.chkShow.Size = new System.Drawing.Size(234, 21);
		    this.chkShow.TabIndex = 0;
		    this.chkShow.Text = "chkShow";
		    // 
		    // btnNextTip
		    // 
		    this.btnNextTip.Location = new System.Drawing.Point(302, 262);
		    this.btnNextTip.Name = "btnNextTip";
		    this.btnNextTip.Size = new System.Drawing.Size(105, 31);
		    this.btnNextTip.TabIndex = 1;
		    this.btnNextTip.Text = "btnNextTip";
		    this.btnNextTip.Click += new System.EventHandler(this.btnNextTip_Click);
		    // 
		    // btnClose
		    // 
		    this.btnClose.DialogResult = System.Windows.Forms.DialogResult.Cancel;
		    this.btnClose.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
		    this.btnClose.Location = new System.Drawing.Point(414, 262);
		    this.btnClose.Name = "btnClose";
		    this.btnClose.Size = new System.Drawing.Size(105, 31);
		    this.btnClose.TabIndex = 2;
		    this.btnClose.Text = "btnClose";
		    this.btnClose.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
		    // 
		    // Shape2
		    // 
		    this.Shape2.BackColor = System.Drawing.Color.Gray;
		    this.Shape2.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
		    this.Shape2.Location = new System.Drawing.Point(22, 10);
		    this.Shape2.Name = "Shape2";
		    this.Shape2.Size = new System.Drawing.Size(103, 224);
		    this.Shape2.TabIndex = 1;
		    // 
		    // Shape3
		    // 
		    this.Shape3.BackColor = System.Drawing.Color.White;
		    this.Shape3.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
		    this.Shape3.ForeColor = System.Drawing.Color.Black;
		    this.Shape3.Location = new System.Drawing.Point(123, 57);
		    this.Shape3.Name = "Shape3";
		    this.Shape3.Size = new System.Drawing.Size(405, 177);
		    this.Shape3.TabIndex = 2;
		    // 
		    // lblTitle
		    // 
		    this.lblTitle.BackColor = System.Drawing.Color.White;
		    this.lblTitle.Font = new System.Drawing.Font("Arial", 16F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
		    this.lblTitle.Location = new System.Drawing.Point(134, 19);
		    this.lblTitle.Name = "lblTitle";
		    this.lblTitle.Size = new System.Drawing.Size(378, 36);
		    this.lblTitle.TabIndex = 3;
		    this.lblTitle.Text = "lblTitle";
		    // 
		    // Image1
		    // 
		    this.Image1.ErrorImage = null;
		    this.Image1.Location = new System.Drawing.Point(45, 29);
		    this.Image1.Name = "Image1";
		    this.Image1.Size = new System.Drawing.Size(57, 52);
		    this.Image1.SizeMode = System.Windows.Forms.PictureBoxSizeMode.StretchImage;
		    this.Image1.TabIndex = 4;
		    this.Image1.TabStop = false;
		    // 
		    // txtTip
		    // 
		    this.txtTip.BorderStyle = System.Windows.Forms.BorderStyle.None;
		    this.txtTip.Location = new System.Drawing.Point(137, 69);
		    this.txtTip.Multiline = true;
		    this.txtTip.Name = "txtTip";
		    this.txtTip.Size = new System.Drawing.Size(371, 154);
		    this.txtTip.TabIndex = 3;
		    // 
		    // DayTipsDlg
		    // 
		    this.AutoScaleDimensions = new System.Drawing.SizeF(120F, 120F);
		    this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
		    this.CancelButton = this.btnClose;
		    this.ClientSize = new System.Drawing.Size(547, 313);
		    this.Controls.Add(this.lblTitle);
		    this.Controls.Add(this.Image1);
		    this.Controls.Add(this.chkShow);
		    this.Controls.Add(this.txtTip);
		    this.Controls.Add(this.btnNextTip);
		    this.Controls.Add(this.btnClose);
		    this.Controls.Add(this.Shape1);
		    this.Controls.Add(this.Shape3);
		    this.Controls.Add(this.Shape2);
		    this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
		    this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedToolWindow;
		    this.MaximizeBox = false;
		    this.MinimizeBox = false;
		    this.Name = "DayTipsDlg";
		    this.ShowInTaskbar = false;
		    this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
		    this.Text = " ";
		    this.TopMost = true;
		    ((System.ComponentModel.ISupportInitialize)(this.Image1)).EndInit();
		    this.ResumeLayout(false);
		    this.PerformLayout();
		}
	}
}