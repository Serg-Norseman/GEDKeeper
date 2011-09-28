using System;

namespace GKUI
{
	partial class TfmTipsDialog
	{
		private System.Windows.Forms.Panel Shape1;
		private System.Windows.Forms.CheckBox ShowCheck;
		private System.Windows.Forms.Button NextTipBtn;
		private System.Windows.Forms.Button btnClose;
		private System.Windows.Forms.Panel Shape2;
		private System.Windows.Forms.Panel Shape3;
		private System.Windows.Forms.Label TitleLabel;
		private System.Windows.Forms.PictureBox Image1;
		private System.Windows.Forms.TextBox TipWindow;

		private void InitializeComponent()
		{
			this.Shape1 = new System.Windows.Forms.Panel();
			this.ShowCheck = new System.Windows.Forms.CheckBox();
			this.NextTipBtn = new System.Windows.Forms.Button();
			this.btnClose = new System.Windows.Forms.Button();
			this.Shape2 = new System.Windows.Forms.Panel();
			this.Shape3 = new System.Windows.Forms.Panel();
			this.TitleLabel = new System.Windows.Forms.Label();
			this.Image1 = new System.Windows.Forms.PictureBox();
			this.TipWindow = new System.Windows.Forms.TextBox();
			((System.ComponentModel.ISupportInitialize)(this.Image1)).BeginInit();
			this.SuspendLayout();
			// 
			// Shape1
			// 
			this.Shape1.BackColor = System.Drawing.Color.White;
			this.Shape1.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
			this.Shape1.ForeColor = System.Drawing.Color.Black;
			this.Shape1.Location = new System.Drawing.Point(88, 8);
			this.Shape1.Name = "Shape1";
			this.Shape1.Size = new System.Drawing.Size(289, 40);
			this.Shape1.TabIndex = 0;
			// 
			// ShowCheck
			// 
			this.ShowCheck.Checked = true;
			this.ShowCheck.CheckState = System.Windows.Forms.CheckState.Checked;
			this.ShowCheck.Location = new System.Drawing.Point(18, 220);
			this.ShowCheck.Name = "ShowCheck";
			this.ShowCheck.Size = new System.Drawing.Size(167, 17);
			this.ShowCheck.TabIndex = 0;
			this.ShowCheck.Text = "Показывать при старте";
			// 
			// NextTipBtn
			// 
			this.NextTipBtn.Location = new System.Drawing.Point(216, 216);
			this.NextTipBtn.Name = "NextTipBtn";
			this.NextTipBtn.Size = new System.Drawing.Size(75, 25);
			this.NextTipBtn.TabIndex = 1;
			this.NextTipBtn.Text = "Далее";
			this.NextTipBtn.Click += new System.EventHandler(this.NextTipBtn_Click);
			// 
			// btnClose
			// 
			this.btnClose.DialogResult = System.Windows.Forms.DialogResult.Cancel;
			this.btnClose.Location = new System.Drawing.Point(296, 216);
			this.btnClose.Name = "btnClose";
			this.btnClose.Size = new System.Drawing.Size(75, 25);
			this.btnClose.TabIndex = 2;
			this.btnClose.Text = "Закрыть";
			// 
			// Shape2
			// 
			this.Shape2.BackColor = System.Drawing.Color.Gray;
			this.Shape2.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
			this.Shape2.Location = new System.Drawing.Point(16, 8);
			this.Shape2.Name = "Shape2";
			this.Shape2.Size = new System.Drawing.Size(73, 185);
			this.Shape2.TabIndex = 1;
			// 
			// Shape3
			// 
			this.Shape3.BackColor = System.Drawing.Color.White;
			this.Shape3.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
			this.Shape3.ForeColor = System.Drawing.Color.Black;
			this.Shape3.Location = new System.Drawing.Point(88, 47);
			this.Shape3.Name = "Shape3";
			this.Shape3.Size = new System.Drawing.Size(289, 146);
			this.Shape3.TabIndex = 2;
			// 
			// TitleLabel
			// 
			this.TitleLabel.BackColor = System.Drawing.Color.White;
			this.TitleLabel.Font = new System.Drawing.Font("Arial", 16F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
			this.TitleLabel.Location = new System.Drawing.Point(96, 16);
			this.TitleLabel.Name = "TitleLabel";
			this.TitleLabel.Size = new System.Drawing.Size(270, 22);
			this.TitleLabel.TabIndex = 3;
			this.TitleLabel.Text = "Вы знаете что...";
			// 
			// Image1
			// 
			this.Image1.Image = global::GKResources.iTipsLight;
			this.Image1.Location = new System.Drawing.Point(32, 24);
			this.Image1.Name = "Image1";
			this.Image1.Size = new System.Drawing.Size(41, 43);
			this.Image1.TabIndex = 4;
			this.Image1.TabStop = false;
			// 
			// TipWindow
			// 
			this.TipWindow.BorderStyle = System.Windows.Forms.BorderStyle.None;
			this.TipWindow.Location = new System.Drawing.Point(98, 57);
			this.TipWindow.Multiline = true;
			this.TipWindow.Name = "TipWindow";
			this.TipWindow.Size = new System.Drawing.Size(265, 127);
			this.TipWindow.TabIndex = 3;
			// 
			// TfmTipsDialog
			// 
			this.AutoScaleBaseSize = new System.Drawing.Size(5, 14);
			this.CancelButton = this.btnClose;
			this.ClientSize = new System.Drawing.Size(389, 252);
			this.Controls.Add(this.TitleLabel);
			this.Controls.Add(this.Image1);
			this.Controls.Add(this.ShowCheck);
			this.Controls.Add(this.TipWindow);
			this.Controls.Add(this.NextTipBtn);
			this.Controls.Add(this.btnClose);
			this.Controls.Add(this.Shape1);
			this.Controls.Add(this.Shape2);
			this.Controls.Add(this.Shape3);
			this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
			this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedToolWindow;
			this.MaximizeBox = false;
			this.MinimizeBox = false;
			this.Name = "TfmTipsDialog";
			this.ShowInTaskbar = false;
			this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
			this.Text = " ";
			((System.ComponentModel.ISupportInitialize)(this.Image1)).EndInit();
			this.ResumeLayout(false);
			this.PerformLayout();
		}
	}
}