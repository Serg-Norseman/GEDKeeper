namespace GKUI.Charts
{
	partial class ACOptionsControl
	{
		private System.Windows.Forms.ColorDialog ColorDialog1;
		private System.Windows.Forms.FontDialog FontDialog1;
		private System.Windows.Forms.Label acb0;
		private System.Windows.Forms.Label acb1;
		private System.Windows.Forms.Label acb2;
		private System.Windows.Forms.Label acb3;
		private System.Windows.Forms.Label acb4;
		private System.Windows.Forms.Label acb5;
		private System.Windows.Forms.Label acb6;
		private System.Windows.Forms.Label acb7;
		private System.Windows.Forms.Label acbText;
		private System.Windows.Forms.Label acbBack;
		private System.Windows.Forms.Label acbLine;
		private System.Windows.Forms.CheckBox chkHideEmptySegments;

		private void InitializeComponent()
		{
		    this.acbLine = new System.Windows.Forms.Label();
		    this.acbBack = new System.Windows.Forms.Label();
		    this.acbText = new System.Windows.Forms.Label();
		    this.acb7 = new System.Windows.Forms.Label();
		    this.acb6 = new System.Windows.Forms.Label();
		    this.acb5 = new System.Windows.Forms.Label();
		    this.acb4 = new System.Windows.Forms.Label();
		    this.acb3 = new System.Windows.Forms.Label();
		    this.acb2 = new System.Windows.Forms.Label();
		    this.acb1 = new System.Windows.Forms.Label();
		    this.acb0 = new System.Windows.Forms.Label();
		    this.ColorDialog1 = new System.Windows.Forms.ColorDialog();
		    this.FontDialog1 = new System.Windows.Forms.FontDialog();
		    this.chkHideEmptySegments = new System.Windows.Forms.CheckBox();
		    this.SuspendLayout();
		    // 
		    // acbLine
		    // 
		    this.acbLine.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
		    this.acbLine.Cursor = System.Windows.Forms.Cursors.Hand;
		    this.acbLine.ForeColor = System.Drawing.Color.White;
		    this.acbLine.Location = new System.Drawing.Point(344, 97);
		    this.acbLine.Name = "acbLine";
		    this.acbLine.Size = new System.Drawing.Size(160, 28);
		    this.acbLine.TabIndex = 42;
		    this.acbLine.Text = "Line color";
		    this.acbLine.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
		    this.acbLine.MouseClick += new System.Windows.Forms.MouseEventHandler(this.lblColorClick);
		    // 
		    // acbBack
		    // 
		    this.acbBack.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
		    this.acbBack.Cursor = System.Windows.Forms.Cursors.Hand;
		    this.acbBack.Location = new System.Drawing.Point(176, 97);
		    this.acbBack.Name = "acbBack";
		    this.acbBack.Size = new System.Drawing.Size(160, 28);
		    this.acbBack.TabIndex = 40;
		    this.acbBack.Text = "Background color";
		    this.acbBack.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
		    this.acbBack.MouseClick += new System.Windows.Forms.MouseEventHandler(this.lblColorClick);
		    // 
		    // acbText
		    // 
		    this.acbText.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
		    this.acbText.Cursor = System.Windows.Forms.Cursors.Hand;
		    this.acbText.ForeColor = System.Drawing.Color.White;
		    this.acbText.Location = new System.Drawing.Point(8, 97);
		    this.acbText.Name = "acbText";
		    this.acbText.Size = new System.Drawing.Size(160, 28);
		    this.acbText.TabIndex = 39;
		    this.acbText.Text = "Text color";
		    this.acbText.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
		    this.acbText.MouseClick += new System.Windows.Forms.MouseEventHandler(this.lblColorClick);
		    // 
		    // acb7
		    // 
		    this.acb7.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
		    this.acb7.Cursor = System.Windows.Forms.Cursors.Hand;
		    this.acb7.Location = new System.Drawing.Point(512, 56);
		    this.acb7.Name = "acb7";
		    this.acb7.Size = new System.Drawing.Size(160, 28);
		    this.acb7.TabIndex = 38;
		    this.acb7.Text = "Circle 7";
		    this.acb7.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
		    this.acb7.MouseClick += new System.Windows.Forms.MouseEventHandler(this.lblColorClick);
		    // 
		    // acb6
		    // 
		    this.acb6.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
		    this.acb6.Cursor = System.Windows.Forms.Cursors.Hand;
		    this.acb6.Location = new System.Drawing.Point(344, 56);
		    this.acb6.Name = "acb6";
		    this.acb6.Size = new System.Drawing.Size(160, 28);
		    this.acb6.TabIndex = 37;
		    this.acb6.Text = "Circle 6";
		    this.acb6.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
		    this.acb6.MouseClick += new System.Windows.Forms.MouseEventHandler(this.lblColorClick);
		    // 
		    // acb5
		    // 
		    this.acb5.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
		    this.acb5.Cursor = System.Windows.Forms.Cursors.Hand;
		    this.acb5.Location = new System.Drawing.Point(176, 56);
		    this.acb5.Name = "acb5";
		    this.acb5.Size = new System.Drawing.Size(160, 28);
		    this.acb5.TabIndex = 36;
		    this.acb5.Text = "Circle 5";
		    this.acb5.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
		    this.acb5.MouseClick += new System.Windows.Forms.MouseEventHandler(this.lblColorClick);
		    // 
		    // acb4
		    // 
		    this.acb4.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
		    this.acb4.Cursor = System.Windows.Forms.Cursors.Hand;
		    this.acb4.Location = new System.Drawing.Point(8, 56);
		    this.acb4.Name = "acb4";
		    this.acb4.Size = new System.Drawing.Size(160, 28);
		    this.acb4.TabIndex = 35;
		    this.acb4.Text = "Circle 4";
		    this.acb4.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
		    this.acb4.MouseClick += new System.Windows.Forms.MouseEventHandler(this.lblColorClick);
		    // 
		    // acb3
		    // 
		    this.acb3.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
		    this.acb3.Cursor = System.Windows.Forms.Cursors.Hand;
		    this.acb3.Location = new System.Drawing.Point(512, 15);
		    this.acb3.Name = "acb3";
		    this.acb3.Size = new System.Drawing.Size(160, 28);
		    this.acb3.TabIndex = 34;
		    this.acb3.Text = "Circle 3";
		    this.acb3.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
		    this.acb3.MouseClick += new System.Windows.Forms.MouseEventHandler(this.lblColorClick);
		    // 
		    // acb2
		    // 
		    this.acb2.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
		    this.acb2.Cursor = System.Windows.Forms.Cursors.Hand;
		    this.acb2.Location = new System.Drawing.Point(344, 15);
		    this.acb2.Name = "acb2";
		    this.acb2.Size = new System.Drawing.Size(160, 28);
		    this.acb2.TabIndex = 33;
		    this.acb2.Text = "Circle 2";
		    this.acb2.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
		    this.acb2.MouseClick += new System.Windows.Forms.MouseEventHandler(this.lblColorClick);
		    // 
		    // acb1
		    // 
		    this.acb1.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
		    this.acb1.Cursor = System.Windows.Forms.Cursors.Hand;
		    this.acb1.Location = new System.Drawing.Point(176, 15);
		    this.acb1.Name = "acb1";
		    this.acb1.Size = new System.Drawing.Size(160, 28);
		    this.acb1.TabIndex = 32;
		    this.acb1.Text = "Circle 1";
		    this.acb1.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
		    this.acb1.MouseClick += new System.Windows.Forms.MouseEventHandler(this.lblColorClick);
		    // 
		    // acb0
		    // 
		    this.acb0.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
		    this.acb0.Cursor = System.Windows.Forms.Cursors.Hand;
		    this.acb0.Location = new System.Drawing.Point(8, 15);
		    this.acb0.Name = "acb0";
		    this.acb0.Size = new System.Drawing.Size(160, 28);
		    this.acb0.TabIndex = 31;
		    this.acb0.Text = "Circle 0";
		    this.acb0.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
		    this.acb0.MouseClick += new System.Windows.Forms.MouseEventHandler(this.lblColorClick);
		    // 
		    // chkHideEmptySegments
		    // 
		    this.chkHideEmptySegments.Location = new System.Drawing.Point(8, 142);
		    this.chkHideEmptySegments.Name = "chkHideEmptySegments";
		    this.chkHideEmptySegments.Size = new System.Drawing.Size(328, 24);
		    this.chkHideEmptySegments.TabIndex = 43;
		    this.chkHideEmptySegments.Text = "chkHideEmptySegments";
		    this.chkHideEmptySegments.UseVisualStyleBackColor = true;
		    // 
		    // ACOptionsControl
		    // 
		    this.AutoScaleDimensions = new System.Drawing.SizeF(120F, 120F);
		    this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
		    this.Controls.Add(this.chkHideEmptySegments);
		    this.Controls.Add(this.acb0);
		    this.Controls.Add(this.acb1);
		    this.Controls.Add(this.acb2);
		    this.Controls.Add(this.acb3);
		    this.Controls.Add(this.acb4);
		    this.Controls.Add(this.acb5);
		    this.Controls.Add(this.acb6);
		    this.Controls.Add(this.acb7);
		    this.Controls.Add(this.acbText);
		    this.Controls.Add(this.acbBack);
		    this.Controls.Add(this.acbLine);
		    this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
		    this.Name = "ACOptionsControl";
		    this.Size = new System.Drawing.Size(690, 183);
		    this.ResumeLayout(false);
		}
	}
}