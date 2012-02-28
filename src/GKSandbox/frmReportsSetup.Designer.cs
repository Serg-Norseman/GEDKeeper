using System;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;

namespace GKSandbox
{
	partial class frmReportsSetup
	{
		private System.Windows.Forms.GroupBox groupBox1;
		private System.Windows.Forms.Label label1;
		private System.Windows.Forms.Label label4;
		private System.Windows.Forms.Label label3;
		private System.Windows.Forms.Label label2;
		private System.Windows.Forms.Label label6;
		private System.Windows.Forms.Button button1;
		private System.Windows.Forms.ColorDialog colorDialog1;
		private System.Windows.Forms.Button button2;
		private System.Windows.Forms.Label label7;
		private System.Windows.Forms.Button button3;
		private System.Windows.Forms.Label label8;
		private System.Windows.Forms.Button button4;
		private System.Windows.Forms.Label label9;
		private System.Windows.Forms.Label label10;
		private System.Windows.Forms.Label label11;
		private System.Windows.Forms.Button button5;
		private System.Windows.Forms.Button button6;
		private System.Windows.Forms.GroupBox groupBox2;
		private System.Windows.Forms.Label label5;
		private System.Windows.Forms.GroupBox groupBox3;
		private System.Windows.Forms.Label label12;
		private System.Windows.Forms.Label label13;
		private System.Windows.Forms.Label label14;
		private System.Windows.Forms.Button button7;
		private System.Windows.Forms.GroupBox groupBox4;
		private System.Windows.Forms.RadioButton radioButton2;
		private System.Windows.Forms.RadioButton radioButton1;

		protected override void Dispose(bool disposing)
		{
			base.Dispose(disposing);
		}

		private void InitializeComponent()
		{
			this.groupBox1 = new System.Windows.Forms.GroupBox();
			this.label4 = new System.Windows.Forms.Label();
			this.label3 = new System.Windows.Forms.Label();
			this.label2 = new System.Windows.Forms.Label();
			this.label1 = new System.Windows.Forms.Label();
			this.tbMgnBottom = new System.Windows.Forms.TextBox();
			this.tbMgnTop = new System.Windows.Forms.TextBox();
			this.tbMgnRight = new System.Windows.Forms.TextBox();
			this.tbMgnLeft = new System.Windows.Forms.TextBox();
			this.label6 = new System.Windows.Forms.Label();
			this.button1 = new System.Windows.Forms.Button();
			this.colorDialog1 = new System.Windows.Forms.ColorDialog();
			this.button2 = new System.Windows.Forms.Button();
			this.label7 = new System.Windows.Forms.Label();
			this.button3 = new System.Windows.Forms.Button();
			this.label8 = new System.Windows.Forms.Label();
			this.button4 = new System.Windows.Forms.Button();
			this.label9 = new System.Windows.Forms.Label();
			this.label10 = new System.Windows.Forms.Label();
			this.label11 = new System.Windows.Forms.Label();
			this.button5 = new System.Windows.Forms.Button();
			this.button6 = new System.Windows.Forms.Button();
			this.groupBox2 = new System.Windows.Forms.GroupBox();
			this.label12 = new System.Windows.Forms.Label();
			this.label5 = new System.Windows.Forms.Label();
			this.groupBox3 = new System.Windows.Forms.GroupBox();
			this.label13 = new System.Windows.Forms.Label();
			this.label14 = new System.Windows.Forms.Label();
			this.button7 = new System.Windows.Forms.Button();
			this.groupBox4 = new System.Windows.Forms.GroupBox();
			this.radioButton2 = new System.Windows.Forms.RadioButton();
			this.radioButton1 = new System.Windows.Forms.RadioButton();
			this.groupBox1.SuspendLayout();
			this.groupBox2.SuspendLayout();
			this.groupBox3.SuspendLayout();
			this.groupBox4.SuspendLayout();
			this.SuspendLayout();
			// 
			// groupBox1
			// 
			this.groupBox1.Controls.Add(this.label4);
			this.groupBox1.Controls.Add(this.label3);
			this.groupBox1.Controls.Add(this.label2);
			this.groupBox1.Controls.Add(this.label1);
			this.groupBox1.Controls.Add(this.tbMgnBottom);
			this.groupBox1.Controls.Add(this.tbMgnTop);
			this.groupBox1.Controls.Add(this.tbMgnRight);
			this.groupBox1.Controls.Add(this.tbMgnLeft);
			this.groupBox1.Location = new System.Drawing.Point(12, 12);
			this.groupBox1.Name = "groupBox1";
			this.groupBox1.Size = new System.Drawing.Size(332, 66);
			this.groupBox1.TabIndex = 0;
			this.groupBox1.TabStop = false;
			this.groupBox1.Text = "Margins";
			// 
			// label4
			// 
			this.label4.AutoSize = true;
			this.label4.Location = new System.Drawing.Point(229, 15);
			this.label4.Name = "label4";
			this.label4.Size = new System.Drawing.Size(40, 13);
			this.label4.TabIndex = 7;
			this.label4.Text = "Bottom";
			// 
			// label3
			// 
			this.label3.AutoSize = true;
			this.label3.Location = new System.Drawing.Point(166, 15);
			this.label3.Name = "label3";
			this.label3.Size = new System.Drawing.Size(32, 13);
			this.label3.TabIndex = 6;
			this.label3.Text = "Right";
			// 
			// label2
			// 
			this.label2.AutoSize = true;
			this.label2.Location = new System.Drawing.Point(97, 15);
			this.label2.Name = "label2";
			this.label2.Size = new System.Drawing.Size(26, 13);
			this.label2.TabIndex = 5;
			this.label2.Text = "Top";
			// 
			// label1
			// 
			this.label1.AutoSize = true;
			this.label1.Location = new System.Drawing.Point(31, 15);
			this.label1.Name = "label1";
			this.label1.Size = new System.Drawing.Size(25, 13);
			this.label1.TabIndex = 4;
			this.label1.Text = "Left";
			// 
			// tbMgnBottom
			// 
			this.tbMgnBottom.Location = new System.Drawing.Point(232, 31);
			this.tbMgnBottom.Name = "tbMgnBottom";
			this.tbMgnBottom.Size = new System.Drawing.Size(50, 20);
			this.tbMgnBottom.TabIndex = 3;
			// 
			// tbMgnTop
			// 
			this.tbMgnTop.Location = new System.Drawing.Point(100, 31);
			this.tbMgnTop.Name = "tbMgnTop";
			this.tbMgnTop.Size = new System.Drawing.Size(50, 20);
			this.tbMgnTop.TabIndex = 1;
			// 
			// tbMgnRight
			// 
			this.tbMgnRight.Location = new System.Drawing.Point(166, 31);
			this.tbMgnRight.Name = "tbMgnRight";
			this.tbMgnRight.Size = new System.Drawing.Size(50, 20);
			this.tbMgnRight.TabIndex = 2;
			// 
			// tbMgnLeft
			// 
			this.tbMgnLeft.Location = new System.Drawing.Point(34, 31);
			this.tbMgnLeft.Name = "tbMgnLeft";
			this.tbMgnLeft.Size = new System.Drawing.Size(50, 20);
			this.tbMgnLeft.TabIndex = 0;
			// 
			// label6
			// 
			this.label6.AutoSize = true;
			this.label6.Location = new System.Drawing.Point(361, 27);
			this.label6.Name = "label6";
			this.label6.Size = new System.Drawing.Size(58, 13);
			this.label6.TabIndex = 4;
			this.label6.Text = "Page color";
			// 
			// button1
			// 
			this.button1.Location = new System.Drawing.Point(426, 41);
			this.button1.Name = "button1";
			this.button1.Size = new System.Drawing.Size(39, 23);
			this.button1.TabIndex = 5;
			this.button1.Text = "...";
			this.button1.UseVisualStyleBackColor = true;
			this.button1.Click += new System.EventHandler(this.button1_Click);
			// 
			// button2
			// 
			this.button2.Location = new System.Drawing.Point(414, 43);
			this.button2.Name = "button2";
			this.button2.Size = new System.Drawing.Size(39, 23);
			this.button2.TabIndex = 6;
			this.button2.Text = "...";
			this.button2.UseVisualStyleBackColor = true;
			this.button2.Click += new System.EventHandler(this.button2_Click);
			// 
			// label7
			// 
			this.label7.AutoSize = true;
			this.label7.Location = new System.Drawing.Point(348, 30);
			this.label7.Name = "label7";
			this.label7.Size = new System.Drawing.Size(58, 13);
			this.label7.TabIndex = 7;
			this.label7.Text = "Back color";
			// 
			// button3
			// 
			this.button3.Location = new System.Drawing.Point(544, 43);
			this.button3.Name = "button3";
			this.button3.Size = new System.Drawing.Size(39, 23);
			this.button3.TabIndex = 1;
			this.button3.Text = "...";
			this.button3.UseVisualStyleBackColor = true;
			this.button3.Click += new System.EventHandler(this.button3_Click);
			// 
			// label8
			// 
			this.label8.AutoSize = true;
			this.label8.Location = new System.Drawing.Point(480, 29);
			this.label8.Name = "label8";
			this.label8.Size = new System.Drawing.Size(54, 13);
			this.label8.TabIndex = 3;
			this.label8.Text = "Fore color";
			// 
			// button4
			// 
			this.button4.Location = new System.Drawing.Point(414, 34);
			this.button4.Name = "button4";
			this.button4.Size = new System.Drawing.Size(39, 23);
			this.button4.TabIndex = 1;
			this.button4.Text = "...";
			this.button4.UseVisualStyleBackColor = true;
			this.button4.Click += new System.EventHandler(this.button4_Click);
			// 
			// label9
			// 
			this.label9.AutoSize = true;
			this.label9.Location = new System.Drawing.Point(349, 21);
			this.label9.Name = "label9";
			this.label9.Size = new System.Drawing.Size(54, 13);
			this.label9.TabIndex = 3;
			this.label9.Text = "Text color";
			// 
			// label10
			// 
			this.label10.BackColor = System.Drawing.SystemColors.Window;
			this.label10.Location = new System.Drawing.Point(33, 16);
			this.label10.Name = "label10";
			this.label10.Size = new System.Drawing.Size(249, 59);
			this.label10.TabIndex = 2;
			this.label10.Text = "Text font";
			this.label10.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
			// 
			// label11
			// 
			this.label11.BackColor = System.Drawing.SystemColors.Window;
			this.label11.Location = new System.Drawing.Point(33, 25);
			this.label11.Name = "label11";
			this.label11.Size = new System.Drawing.Size(249, 59);
			this.label11.TabIndex = 4;
			this.label11.Text = "Header font";
			this.label11.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
			// 
			// button5
			// 
			this.button5.Location = new System.Drawing.Point(676, 41);
			this.button5.Name = "button5";
			this.button5.Size = new System.Drawing.Size(75, 23);
			this.button5.TabIndex = 6;
			this.button5.Text = "&OK";
			this.button5.UseVisualStyleBackColor = true;
			this.button5.Click += new System.EventHandler(this.button5_Click);
			// 
			// button6
			// 
			this.button6.Location = new System.Drawing.Point(676, 70);
			this.button6.Name = "button6";
			this.button6.Size = new System.Drawing.Size(75, 23);
			this.button6.TabIndex = 7;
			this.button6.Text = "&Cancel";
			this.button6.UseVisualStyleBackColor = true;
			this.button6.Click += new System.EventHandler(this.button6_Click);
			// 
			// groupBox2
			// 
			this.groupBox2.Controls.Add(this.label12);
			this.groupBox2.Controls.Add(this.label5);
			this.groupBox2.Controls.Add(this.label11);
			this.groupBox2.Controls.Add(this.label7);
			this.groupBox2.Controls.Add(this.button2);
			this.groupBox2.Controls.Add(this.label8);
			this.groupBox2.Controls.Add(this.button3);
			this.groupBox2.Location = new System.Drawing.Point(12, 84);
			this.groupBox2.Name = "groupBox2";
			this.groupBox2.Size = new System.Drawing.Size(614, 100);
			this.groupBox2.TabIndex = 0;
			this.groupBox2.TabStop = false;
			this.groupBox2.Text = "Heading";
			// 
			// label12
			// 
			this.label12.BackColor = System.Drawing.SystemColors.ActiveCaptionText;
			this.label12.Location = new System.Drawing.Point(480, 42);
			this.label12.Name = "label12";
			this.label12.Size = new System.Drawing.Size(58, 23);
			this.label12.TabIndex = 2;
			// 
			// label5
			// 
			this.label5.BackColor = System.Drawing.SystemColors.ActiveCaption;
			this.label5.Location = new System.Drawing.Point(350, 43);
			this.label5.Name = "label5";
			this.label5.Size = new System.Drawing.Size(58, 23);
			this.label5.TabIndex = 5;
			// 
			// groupBox3
			// 
			this.groupBox3.Controls.Add(this.label13);
			this.groupBox3.Controls.Add(this.label10);
			this.groupBox3.Controls.Add(this.label9);
			this.groupBox3.Controls.Add(this.button4);
			this.groupBox3.Location = new System.Drawing.Point(12, 202);
			this.groupBox3.Name = "groupBox3";
			this.groupBox3.Size = new System.Drawing.Size(614, 100);
			this.groupBox3.TabIndex = 0;
			this.groupBox3.TabStop = false;
			this.groupBox3.Text = "Text";
			// 
			// label13
			// 
			this.label13.BackColor = System.Drawing.SystemColors.ActiveCaptionText;
			this.label13.Location = new System.Drawing.Point(349, 34);
			this.label13.Name = "label13";
			this.label13.Size = new System.Drawing.Size(58, 23);
			this.label13.TabIndex = 0;
			// 
			// label14
			// 
			this.label14.BackColor = System.Drawing.SystemColors.ControlLightLight;
			this.label14.Location = new System.Drawing.Point(361, 43);
			this.label14.Name = "label14";
			this.label14.Size = new System.Drawing.Size(58, 23);
			this.label14.TabIndex = 8;
			// 
			// button7
			// 
			this.button7.Location = new System.Drawing.Point(676, 12);
			this.button7.Name = "button7";
			this.button7.Size = new System.Drawing.Size(75, 23);
			this.button7.TabIndex = 9;
			this.button7.Text = "Table Setup";
			this.button7.UseVisualStyleBackColor = true;
			this.button7.Click += new System.EventHandler(this.button7_Click);
			// 
			// groupBox4
			// 
			this.groupBox4.Controls.Add(this.radioButton2);
			this.groupBox4.Controls.Add(this.radioButton1);
			this.groupBox4.Location = new System.Drawing.Point(499, 12);
			this.groupBox4.Name = "groupBox4";
			this.groupBox4.Size = new System.Drawing.Size(127, 69);
			this.groupBox4.TabIndex = 10;
			this.groupBox4.TabStop = false;
			this.groupBox4.Text = "Page orientation";
			// 
			// radioButton2
			// 
			this.radioButton2.AutoSize = true;
			this.radioButton2.Location = new System.Drawing.Point(20, 42);
			this.radioButton2.Name = "radioButton2";
			this.radioButton2.Size = new System.Drawing.Size(78, 17);
			this.radioButton2.TabIndex = 1;
			this.radioButton2.TabStop = true;
			this.radioButton2.Text = "Landscape";
			this.radioButton2.UseVisualStyleBackColor = true;
			// 
			// radioButton1
			// 
			this.radioButton1.AutoSize = true;
			this.radioButton1.Location = new System.Drawing.Point(20, 19);
			this.radioButton1.Name = "radioButton1";
			this.radioButton1.Size = new System.Drawing.Size(58, 17);
			this.radioButton1.TabIndex = 0;
			this.radioButton1.TabStop = true;
			this.radioButton1.Text = "Portrait";
			this.radioButton1.UseVisualStyleBackColor = true;
			// 
			// frmReportsSetup
			// 
			this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
			this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
			this.ClientSize = new System.Drawing.Size(763, 317);
			this.Controls.Add(this.groupBox4);
			this.Controls.Add(this.button7);
			this.Controls.Add(this.label14);
			this.Controls.Add(this.groupBox3);
			this.Controls.Add(this.groupBox2);
			this.Controls.Add(this.button6);
			this.Controls.Add(this.button5);
			this.Controls.Add(this.button1);
			this.Controls.Add(this.label6);
			this.Controls.Add(this.groupBox1);
			this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
			this.Name = "frmReportsSetup";
			this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
			this.Text = "Report Setup";
			this.Shown += new System.EventHandler(this.OnShown);
			this.groupBox1.ResumeLayout(false);
			this.groupBox1.PerformLayout();
			this.groupBox2.ResumeLayout(false);
			this.groupBox2.PerformLayout();
			this.groupBox3.ResumeLayout(false);
			this.groupBox3.PerformLayout();
			this.groupBox4.ResumeLayout(false);
			this.groupBox4.PerformLayout();
			this.ResumeLayout(false);
			this.PerformLayout();
		}
		private System.Windows.Forms.TextBox tbMgnBottom;
		private System.Windows.Forms.TextBox tbMgnRight;
		private System.Windows.Forms.TextBox tbMgnTop;
		private System.Windows.Forms.TextBox tbMgnLeft;

	}
}
