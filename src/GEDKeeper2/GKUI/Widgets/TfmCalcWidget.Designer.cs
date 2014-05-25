using System;

namespace GKUI.Widgets
{
	partial class TfmCalcWidget
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
			this.lbOutput.Location = new System.Drawing.Point(8, 8);
			this.lbOutput.Name = "lbOutput";
			this.lbOutput.Size = new System.Drawing.Size(257, 95);
			this.lbOutput.TabIndex = 0;
			this.edExpression.Location = new System.Drawing.Point(8, 120);
			this.edExpression.Name = "edExpression";
			this.edExpression.Size = new System.Drawing.Size(257, 21);
			this.edExpression.TabIndex = 1;
			this.edExpression.Text = "";
			this.edExpression.KeyDown += new System.Windows.Forms.KeyEventHandler(this.edExpression_KeyDown);
			this.chkPutToClipboard.Location = new System.Drawing.Point(8, 176);
			this.chkPutToClipboard.Name = "chkPutToClipboard";
			this.chkPutToClipboard.Size = new System.Drawing.Size(257, 17);
			this.chkPutToClipboard.TabIndex = 3;
			this.chkPutToClipboard.Text = "PutToClipboard";
			this.edCalcResult.Location = new System.Drawing.Point(8, 144);
			this.edCalcResult.Name = "edCalcResult";
			this.edCalcResult.ReadOnly = true;
			this.edCalcResult.Size = new System.Drawing.Size(257, 21);
			this.edCalcResult.TabIndex = 2;
			this.edCalcResult.TabStop = false;
			this.edCalcResult.Text = "1979";
			this.edCalcResult.DragOver += new System.Windows.Forms.DragEventHandler(this.edCalcResult_DragOver);
			this.edCalcResult.MouseMove += new System.Windows.Forms.MouseEventHandler(this.edCalcResult_MouseMove);
			this.AutoScaleBaseSize = new System.Drawing.Size(5, 14);
			this.ClientSize = new System.Drawing.Size(274, 201);
			this.Controls.Add(this.lbOutput);
			this.Controls.Add(this.edExpression);
			this.Controls.Add(this.chkPutToClipboard);
			this.Controls.Add(this.edCalcResult);
			this.Font = new System.Drawing.Font("Tahoma", 8.25f, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, 204);
			this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedToolWindow;
			this.Load += new System.EventHandler(this.TfmCalcWidget_Load);
			this.Name = "TfmCalcWidget";
			this.ShowInTaskbar = false;
			this.StartPosition = System.Windows.Forms.FormStartPosition.Manual;
			this.Text = "ExpCalc";
			this.TopMost = true;
			this.Closed += new System.EventHandler(this.TfmCalcWidget_Closed);
			this.ResumeLayout(false);
		}
	}
}