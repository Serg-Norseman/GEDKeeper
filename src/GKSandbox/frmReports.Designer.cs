using System;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;

namespace GKSandbox
{
	partial class frmReports
	{
		private System.ComponentModel.IContainer components;
		private System.Windows.Forms.StatusStrip statusStrip1;
		private System.Windows.Forms.ToolStripStatusLabel toolStripStatusLabel1;
		private System.Windows.Forms.RadioButton radioButton1;
		private System.Windows.Forms.GroupBox groupBox1;
		private System.Windows.Forms.RadioButton radioButton3;
		private System.Windows.Forms.TextBox textBox3;
		private System.Windows.Forms.Label label3;
		private System.Windows.Forms.Timer timer1;
		private System.Windows.Forms.SaveFileDialog saveFileDialog1;

		protected override void Dispose(bool disposing)
		{
			if (disposing && this.components != null)
			{
				this.components.Dispose();
			}
			base.Dispose(disposing);
		}

		private void InitializeComponent()
		{
			this.components = new System.ComponentModel.Container();
			this.statusStrip1 = new System.Windows.Forms.StatusStrip();
			this.toolStripStatusLabel1 = new System.Windows.Forms.ToolStripStatusLabel();
			this.radioButton1 = new System.Windows.Forms.RadioButton();
			this.groupBox1 = new System.Windows.Forms.GroupBox();
			this.radioButton2 = new System.Windows.Forms.RadioButton();
			this.radioButton3 = new System.Windows.Forms.RadioButton();
			this.textBox3 = new System.Windows.Forms.TextBox();
			this.label3 = new System.Windows.Forms.Label();
			this.timer1 = new System.Windows.Forms.Timer(this.components);
			this.btnSettings = new System.Windows.Forms.Button();
			this.btnCreateReport = new System.Windows.Forms.Button();
			this.btnShowReport = new System.Windows.Forms.Button();
			this.btnTableSettings = new System.Windows.Forms.Button();
			this.saveFileDialog1 = new System.Windows.Forms.SaveFileDialog();
			this.statusStrip1.SuspendLayout();
			this.groupBox1.SuspendLayout();
			this.SuspendLayout();
			// 
			// statusStrip1
			// 
			this.statusStrip1.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
									this.toolStripStatusLabel1});
			this.statusStrip1.Location = new System.Drawing.Point(0, 148);
			this.statusStrip1.Name = "statusStrip1";
			this.statusStrip1.Size = new System.Drawing.Size(418, 22);
			this.statusStrip1.TabIndex = 1;
			this.statusStrip1.Text = "statusStrip1";
			// 
			// toolStripStatusLabel1
			// 
			this.toolStripStatusLabel1.Name = "toolStripStatusLabel1";
			this.toolStripStatusLabel1.Size = new System.Drawing.Size(403, 17);
			this.toolStripStatusLabel1.Spring = true;
			this.toolStripStatusLabel1.Text = "toolStripStatusLabel1";
			// 
			// radioButton1
			// 
			this.radioButton1.AutoSize = true;
			this.radioButton1.Checked = true;
			this.radioButton1.Location = new System.Drawing.Point(6, 19);
			this.radioButton1.Name = "radioButton1";
			this.radioButton1.Size = new System.Drawing.Size(130, 17);
			this.radioButton1.TabIndex = 6;
			this.radioButton1.TabStop = true;
			this.radioButton1.Text = "Все записи, таблица";
			this.radioButton1.UseVisualStyleBackColor = true;
			// 
			// groupBox1
			// 
			this.groupBox1.Controls.Add(this.radioButton2);
			this.groupBox1.Controls.Add(this.radioButton3);
			this.groupBox1.Controls.Add(this.radioButton1);
			this.groupBox1.Location = new System.Drawing.Point(9, 41);
			this.groupBox1.Name = "groupBox1";
			this.groupBox1.Size = new System.Drawing.Size(177, 96);
			this.groupBox1.TabIndex = 7;
			this.groupBox1.TabStop = false;
			this.groupBox1.Text = "Тип отчета";
			// 
			// radioButton2
			// 
			this.radioButton2.AutoSize = true;
			this.radioButton2.Location = new System.Drawing.Point(6, 42);
			this.radioButton2.Name = "radioButton2";
			this.radioButton2.Size = new System.Drawing.Size(131, 17);
			this.radioButton2.TabIndex = 1;
			this.radioButton2.Text = "Все записи, линейно";
			this.radioButton2.UseVisualStyleBackColor = true;
			// 
			// radioButton3
			// 
			this.radioButton3.AutoSize = true;
			this.radioButton3.Location = new System.Drawing.Point(6, 65);
			this.radioButton3.Name = "radioButton3";
			this.radioButton3.Size = new System.Drawing.Size(43, 17);
			this.radioButton3.TabIndex = 8;
			this.radioButton3.Text = "ФК";
			this.radioButton3.UseVisualStyleBackColor = true;
			// 
			// textBox3
			// 
			this.textBox3.Location = new System.Drawing.Point(65, 12);
			this.textBox3.Name = "textBox3";
			this.textBox3.Size = new System.Drawing.Size(342, 20);
			this.textBox3.TabIndex = 15;
			// 
			// label3
			// 
			this.label3.AutoSize = true;
			this.label3.Location = new System.Drawing.Point(9, 19);
			this.label3.Name = "label3";
			this.label3.Size = new System.Drawing.Size(50, 13);
			this.label3.TabIndex = 16;
			this.label3.Text = "Heading:";
			// 
			// timer1
			// 
			this.timer1.Interval = 200;
			this.timer1.Tick += new System.EventHandler(this.OnTick);
			// 
			// btnSettings
			// 
			this.btnSettings.Location = new System.Drawing.Point(217, 112);
			this.btnSettings.Name = "btnSettings";
			this.btnSettings.Size = new System.Drawing.Size(92, 23);
			this.btnSettings.TabIndex = 30;
			this.btnSettings.Text = "Settings";
			this.btnSettings.UseVisualStyleBackColor = true;
			this.btnSettings.Click += new System.EventHandler(this.btnSettings_Click);
			// 
			// btnCreateReport
			// 
			this.btnCreateReport.Location = new System.Drawing.Point(217, 83);
			this.btnCreateReport.Name = "btnCreateReport";
			this.btnCreateReport.Size = new System.Drawing.Size(92, 23);
			this.btnCreateReport.TabIndex = 31;
			this.btnCreateReport.Text = "Create report";
			this.btnCreateReport.UseVisualStyleBackColor = true;
			this.btnCreateReport.Click += new System.EventHandler(this.btnCreateReport_Click);
			// 
			// btnShowReport
			// 
			this.btnShowReport.Enabled = false;
			this.btnShowReport.Location = new System.Drawing.Point(315, 83);
			this.btnShowReport.Name = "btnShowReport";
			this.btnShowReport.Size = new System.Drawing.Size(92, 23);
			this.btnShowReport.TabIndex = 32;
			this.btnShowReport.Text = "Show report";
			this.btnShowReport.UseVisualStyleBackColor = true;
			this.btnShowReport.Click += new System.EventHandler(this.btnShowReport_Click);
			// 
			// btnTableSettings
			// 
			this.btnTableSettings.Location = new System.Drawing.Point(315, 112);
			this.btnTableSettings.Name = "btnTableSettings";
			this.btnTableSettings.Size = new System.Drawing.Size(92, 23);
			this.btnTableSettings.TabIndex = 33;
			this.btnTableSettings.Text = "Table Settings";
			this.btnTableSettings.UseVisualStyleBackColor = true;
			this.btnTableSettings.Click += new System.EventHandler(this.btnTableSettings_Click);
			// 
			// frmReports
			// 
			this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
			this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
			this.ClientSize = new System.Drawing.Size(418, 170);
			this.Controls.Add(this.btnTableSettings);
			this.Controls.Add(this.btnShowReport);
			this.Controls.Add(this.btnCreateReport);
			this.Controls.Add(this.btnSettings);
			this.Controls.Add(this.label3);
			this.Controls.Add(this.textBox3);
			this.Controls.Add(this.statusStrip1);
			this.Controls.Add(this.groupBox1);
			this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
			this.MaximizeBox = false;
			this.MinimizeBox = false;
			this.Name = "frmReports";
			this.ShowInTaskbar = false;
			this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
			this.Text = "Person report";
			this.Shown += new System.EventHandler(this.OnShown);
			this.statusStrip1.ResumeLayout(false);
			this.statusStrip1.PerformLayout();
			this.groupBox1.ResumeLayout(false);
			this.groupBox1.PerformLayout();
			this.ResumeLayout(false);
			this.PerformLayout();
		}
		private System.Windows.Forms.Button btnTableSettings;
		private System.Windows.Forms.Button btnSettings;
		private System.Windows.Forms.RadioButton radioButton2;
		private System.Windows.Forms.Button btnCreateReport;
		private System.Windows.Forms.Button btnShowReport;
	}
}
