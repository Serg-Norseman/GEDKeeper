using System;

namespace GKUI
{
	partial class ACOptionsDialog
	{
		private System.Windows.Forms.TabControl PageControl1;
		private System.Windows.Forms.Button btnAccept;
		private System.Windows.Forms.Button btnCancel;
		private System.Windows.Forms.ColorDialog ColorDialog1;
		private System.Windows.Forms.FontDialog FontDialog1;

		private void InitializeComponent()
		{
			this.PageControl1 = new System.Windows.Forms.TabControl();
			this.SheetCharts = new System.Windows.Forms.TabPage();
			this.tabControl1 = new System.Windows.Forms.TabControl();
			this.SheetAncCircle = new System.Windows.Forms.TabPage();
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
			this.btnAccept = new System.Windows.Forms.Button();
			this.btnCancel = new System.Windows.Forms.Button();
			this.ColorDialog1 = new System.Windows.Forms.ColorDialog();
			this.FontDialog1 = new System.Windows.Forms.FontDialog();
			this.PageControl1.SuspendLayout();
			this.SheetCharts.SuspendLayout();
			this.tabControl1.SuspendLayout();
			this.SheetAncCircle.SuspendLayout();
			this.SuspendLayout();
			// 
			// PageControl1
			// 
			this.PageControl1.Controls.Add(this.SheetCharts);
			this.PageControl1.Dock = System.Windows.Forms.DockStyle.Top;
			this.PageControl1.Location = new System.Drawing.Point(0, 0);
			this.PageControl1.Name = "PageControl1";
			this.PageControl1.SelectedIndex = 0;
			this.PageControl1.Size = new System.Drawing.Size(512, 377);
			this.PageControl1.TabIndex = 0;
			// 
			// SheetCharts
			// 
			this.SheetCharts.Controls.Add(this.tabControl1);
			this.SheetCharts.Location = new System.Drawing.Point(4, 22);
			this.SheetCharts.Name = "SheetCharts";
			this.SheetCharts.Padding = new System.Windows.Forms.Padding(3);
			this.SheetCharts.Size = new System.Drawing.Size(504, 351);
			this.SheetCharts.TabIndex = 4;
			this.SheetCharts.Text = "Диаграммы";
			this.SheetCharts.UseVisualStyleBackColor = true;
			// 
			// tabControl1
			// 
			this.tabControl1.Controls.Add(this.SheetAncCircle);
			this.tabControl1.Dock = System.Windows.Forms.DockStyle.Fill;
			this.tabControl1.Location = new System.Drawing.Point(3, 3);
			this.tabControl1.Name = "tabControl1";
			this.tabControl1.SelectedIndex = 0;
			this.tabControl1.Size = new System.Drawing.Size(498, 345);
			this.tabControl1.TabIndex = 0;
			// 
			// SheetAncCircle
			// 
			this.SheetAncCircle.Controls.Add(this.acbLine);
			this.SheetAncCircle.Controls.Add(this.acbBack);
			this.SheetAncCircle.Controls.Add(this.acbText);
			this.SheetAncCircle.Controls.Add(this.acb7);
			this.SheetAncCircle.Controls.Add(this.acb6);
			this.SheetAncCircle.Controls.Add(this.acb5);
			this.SheetAncCircle.Controls.Add(this.acb4);
			this.SheetAncCircle.Controls.Add(this.acb3);
			this.SheetAncCircle.Controls.Add(this.acb2);
			this.SheetAncCircle.Controls.Add(this.acb1);
			this.SheetAncCircle.Controls.Add(this.acb0);
			this.SheetAncCircle.Location = new System.Drawing.Point(4, 22);
			this.SheetAncCircle.Name = "SheetAncCircle";
			this.SheetAncCircle.Padding = new System.Windows.Forms.Padding(3);
			this.SheetAncCircle.Size = new System.Drawing.Size(490, 319);
			this.SheetAncCircle.TabIndex = 4;
			this.SheetAncCircle.Text = "Круг предков";
			this.SheetAncCircle.UseVisualStyleBackColor = true;
			// 
			// acbLine
			// 
			this.acbLine.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
			this.acbLine.Cursor = System.Windows.Forms.Cursors.Hand;
			this.acbLine.ForeColor = System.Drawing.Color.White;
			this.acbLine.Location = new System.Drawing.Point(246, 80);
			this.acbLine.Name = "acbLine";
			this.acbLine.Size = new System.Drawing.Size(114, 23);
			this.acbLine.TabIndex = 42;
			this.acbLine.Text = "Line color";
			this.acbLine.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
			this.acbLine.MouseClick += new System.Windows.Forms.MouseEventHandler(this.acbMouseClick);
			// 
			// acbBack
			// 
			this.acbBack.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
			this.acbBack.Cursor = System.Windows.Forms.Cursors.Hand;
			this.acbBack.Location = new System.Drawing.Point(126, 80);
			this.acbBack.Name = "acbBack";
			this.acbBack.Size = new System.Drawing.Size(114, 23);
			this.acbBack.TabIndex = 40;
			this.acbBack.Text = "Background color";
			this.acbBack.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
			this.acbBack.MouseClick += new System.Windows.Forms.MouseEventHandler(this.acbMouseClick);
			// 
			// acbText
			// 
			this.acbText.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
			this.acbText.Cursor = System.Windows.Forms.Cursors.Hand;
			this.acbText.ForeColor = System.Drawing.Color.White;
			this.acbText.Location = new System.Drawing.Point(6, 80);
			this.acbText.Name = "acbText";
			this.acbText.Size = new System.Drawing.Size(114, 23);
			this.acbText.TabIndex = 39;
			this.acbText.Text = "Text color";
			this.acbText.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
			this.acbText.MouseClick += new System.Windows.Forms.MouseEventHandler(this.acbMouseClick);
			// 
			// acb7
			// 
			this.acb7.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
			this.acb7.Cursor = System.Windows.Forms.Cursors.Hand;
			this.acb7.Location = new System.Drawing.Point(366, 46);
			this.acb7.Name = "acb7";
			this.acb7.Size = new System.Drawing.Size(114, 23);
			this.acb7.TabIndex = 38;
			this.acb7.Text = "Circle 7";
			this.acb7.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
			this.acb7.MouseClick += new System.Windows.Forms.MouseEventHandler(this.acbMouseClick);
			// 
			// acb6
			// 
			this.acb6.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
			this.acb6.Cursor = System.Windows.Forms.Cursors.Hand;
			this.acb6.Location = new System.Drawing.Point(246, 46);
			this.acb6.Name = "acb6";
			this.acb6.Size = new System.Drawing.Size(114, 23);
			this.acb6.TabIndex = 37;
			this.acb6.Text = "Circle 6";
			this.acb6.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
			this.acb6.MouseClick += new System.Windows.Forms.MouseEventHandler(this.acbMouseClick);
			// 
			// acb5
			// 
			this.acb5.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
			this.acb5.Cursor = System.Windows.Forms.Cursors.Hand;
			this.acb5.Location = new System.Drawing.Point(126, 46);
			this.acb5.Name = "acb5";
			this.acb5.Size = new System.Drawing.Size(114, 23);
			this.acb5.TabIndex = 36;
			this.acb5.Text = "Circle 5";
			this.acb5.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
			this.acb5.MouseClick += new System.Windows.Forms.MouseEventHandler(this.acbMouseClick);
			// 
			// acb4
			// 
			this.acb4.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
			this.acb4.Cursor = System.Windows.Forms.Cursors.Hand;
			this.acb4.Location = new System.Drawing.Point(6, 46);
			this.acb4.Name = "acb4";
			this.acb4.Size = new System.Drawing.Size(114, 23);
			this.acb4.TabIndex = 35;
			this.acb4.Text = "Circle 4";
			this.acb4.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
			this.acb4.MouseClick += new System.Windows.Forms.MouseEventHandler(this.acbMouseClick);
			// 
			// acb3
			// 
			this.acb3.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
			this.acb3.Cursor = System.Windows.Forms.Cursors.Hand;
			this.acb3.Location = new System.Drawing.Point(366, 12);
			this.acb3.Name = "acb3";
			this.acb3.Size = new System.Drawing.Size(114, 23);
			this.acb3.TabIndex = 34;
			this.acb3.Text = "Circle 3";
			this.acb3.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
			this.acb3.MouseClick += new System.Windows.Forms.MouseEventHandler(this.acbMouseClick);
			// 
			// acb2
			// 
			this.acb2.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
			this.acb2.Cursor = System.Windows.Forms.Cursors.Hand;
			this.acb2.Location = new System.Drawing.Point(246, 12);
			this.acb2.Name = "acb2";
			this.acb2.Size = new System.Drawing.Size(114, 23);
			this.acb2.TabIndex = 33;
			this.acb2.Text = "Circle 2";
			this.acb2.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
			this.acb2.MouseClick += new System.Windows.Forms.MouseEventHandler(this.acbMouseClick);
			// 
			// acb1
			// 
			this.acb1.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
			this.acb1.Cursor = System.Windows.Forms.Cursors.Hand;
			this.acb1.Location = new System.Drawing.Point(126, 12);
			this.acb1.Name = "acb1";
			this.acb1.Size = new System.Drawing.Size(114, 23);
			this.acb1.TabIndex = 32;
			this.acb1.Text = "Circle 1";
			this.acb1.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
			this.acb1.MouseClick += new System.Windows.Forms.MouseEventHandler(this.acbMouseClick);
			// 
			// acb0
			// 
			this.acb0.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
			this.acb0.Cursor = System.Windows.Forms.Cursors.Hand;
			this.acb0.Location = new System.Drawing.Point(6, 12);
			this.acb0.Name = "acb0";
			this.acb0.Size = new System.Drawing.Size(114, 23);
			this.acb0.TabIndex = 31;
			this.acb0.Text = "Circle 0";
			this.acb0.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
			this.acb0.MouseClick += new System.Windows.Forms.MouseEventHandler(this.acbMouseClick);
			// 
			// btnAccept
			// 
			this.btnAccept.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
			this.btnAccept.Location = new System.Drawing.Point(336, 392);
			this.btnAccept.Name = "btnAccept";
			this.btnAccept.Size = new System.Drawing.Size(81, 25);
			this.btnAccept.TabIndex = 1;
			this.btnAccept.Text = "Принять";
			this.btnAccept.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
			this.btnAccept.Click += new System.EventHandler(this.btnAccept_Click);
			// 
			// btnCancel
			// 
			this.btnCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
			this.btnCancel.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
			this.btnCancel.Location = new System.Drawing.Point(424, 392);
			this.btnCancel.Name = "btnCancel";
			this.btnCancel.Size = new System.Drawing.Size(81, 25);
			this.btnCancel.TabIndex = 2;
			this.btnCancel.Text = "Отменить";
			this.btnCancel.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
			// 
			// ACOptionsDialog
			// 
			this.AcceptButton = this.btnAccept;
			this.AutoScaleBaseSize = new System.Drawing.Size(5, 14);
			this.CancelButton = this.btnCancel;
			this.ClientSize = new System.Drawing.Size(512, 425);
			this.Controls.Add(this.PageControl1);
			this.Controls.Add(this.btnAccept);
			this.Controls.Add(this.btnCancel);
			this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
			this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
			this.MaximizeBox = false;
			this.MinimizeBox = false;
			this.Name = "ACOptionsDialog";
			this.ShowInTaskbar = false;
			this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
			this.Text = "Настройки";
			this.PageControl1.ResumeLayout(false);
			this.SheetCharts.ResumeLayout(false);
			this.tabControl1.ResumeLayout(false);
			this.SheetAncCircle.ResumeLayout(false);
			this.ResumeLayout(false);
		}
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
		private System.Windows.Forms.TabPage SheetAncCircle;
		private System.Windows.Forms.TabControl tabControl1;
		private System.Windows.Forms.TabPage SheetCharts;
	}
}