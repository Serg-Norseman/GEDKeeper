using System;

namespace GKCalculatorPlugin
{
	partial class CalcWidget
	{
		private System.Windows.Forms.ListBox lbOutput;
		private System.Windows.Forms.TextBox edExpression;
		private System.Windows.Forms.CheckBox chkPutToClipboard;
		private System.Windows.Forms.TextBox edCalcResult;

		private void InitializeComponent()
		{
		    this.lbOutput = new System.Windows.Forms.ListBox();
		    this.edExpression = new System.Windows.Forms.TextBox();
		    this.chkPutToClipboard = new System.Windows.Forms.CheckBox();
		    this.edCalcResult = new System.Windows.Forms.TextBox();
		    this.SuspendLayout();
		    // 
		    // lbOutput
		    // 
		    this.lbOutput.ItemHeight = 17;
		    this.lbOutput.Location = new System.Drawing.Point(8, 8);
		    this.lbOutput.Name = "lbOutput";
		    this.lbOutput.Size = new System.Drawing.Size(257, 106);
		    this.lbOutput.TabIndex = 0;
		    this.lbOutput.DoubleClick += new System.EventHandler(this.lbOutput_DoubleClick);
		    // 
		    // edExpression
		    // 
		    this.edExpression.Location = new System.Drawing.Point(8, 126);
		    this.edExpression.Name = "edExpression";
		    this.edExpression.Size = new System.Drawing.Size(257, 24);
		    this.edExpression.TabIndex = 1;
		    this.edExpression.KeyDown += new System.Windows.Forms.KeyEventHandler(this.edExpression_KeyDown);
		    // 
		    // chkPutToClipboard
		    // 
		    this.chkPutToClipboard.Location = new System.Drawing.Point(8, 188);
		    this.chkPutToClipboard.Name = "chkPutToClipboard";
		    this.chkPutToClipboard.Size = new System.Drawing.Size(257, 30);
		    this.chkPutToClipboard.TabIndex = 3;
		    this.chkPutToClipboard.Text = "PutToClipboard";
		    // 
		    // edCalcResult
		    // 
		    this.edCalcResult.Location = new System.Drawing.Point(8, 156);
		    this.edCalcResult.Name = "edCalcResult";
		    this.edCalcResult.ReadOnly = true;
		    this.edCalcResult.Size = new System.Drawing.Size(257, 24);
		    this.edCalcResult.TabIndex = 2;
		    this.edCalcResult.TabStop = false;
		    this.edCalcResult.Text = "";
		    this.edCalcResult.DragOver += new System.Windows.Forms.DragEventHandler(this.edCalcResult_DragOver);
		    this.edCalcResult.MouseMove += new System.Windows.Forms.MouseEventHandler(this.edCalcResult_MouseMove);
		    // 
		    // CalcWidget
		    // 
		    this.AutoScaleDimensions = new System.Drawing.SizeF(8F, 17F);
		    this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
		    this.ClientSize = new System.Drawing.Size(274, 223);
		    this.Controls.Add(this.lbOutput);
		    this.Controls.Add(this.edExpression);
		    this.Controls.Add(this.chkPutToClipboard);
		    this.Controls.Add(this.edCalcResult);
		    this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
		    this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedToolWindow;
		    this.Name = "CalcWidget";
		    this.ShowInTaskbar = false;
		    this.StartPosition = System.Windows.Forms.FormStartPosition.Manual;
		    this.Text = "ExpCalc";
		    this.TopMost = true;
		    this.Closed += new System.EventHandler(this.CalcWidget_Closed);
		    this.Load += new System.EventHandler(this.CalcWidget_Load);
		    this.ResumeLayout(false);
		    this.PerformLayout();
		}
	}
}