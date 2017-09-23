namespace GKUI
{
	partial class ScriptEditWin
	{
		private System.Windows.Forms.ToolStrip ToolBar1;
		private System.Windows.Forms.ToolStripButton tbLoadScript;
		private System.Windows.Forms.ToolStripSeparator ToolButton2;
		private System.Windows.Forms.ToolStripButton tbRun;
		private GKUI.Components.TextBoxEx txtDebugOutput;
		private System.Windows.Forms.TextBox txtScriptText;
		private System.Windows.Forms.ToolStripButton tbSaveScript;
		private System.Windows.Forms.ToolStripButton tbNewScript;
		private System.Windows.Forms.SplitContainer splitContainer1;

		private void InitializeComponent()
		{
		    this.ToolBar1 = new System.Windows.Forms.ToolStrip();
		    this.tbNewScript = new System.Windows.Forms.ToolStripButton();
		    this.tbLoadScript = new System.Windows.Forms.ToolStripButton();
		    this.tbSaveScript = new System.Windows.Forms.ToolStripButton();
		    this.ToolButton2 = new System.Windows.Forms.ToolStripSeparator();
		    this.tbRun = new System.Windows.Forms.ToolStripButton();
		    this.splitContainer1 = new System.Windows.Forms.SplitContainer();
		    this.txtScriptText = new System.Windows.Forms.TextBox();
		    this.txtDebugOutput = new GKUI.Components.TextBoxEx();
		    this.ToolBar1.SuspendLayout();
		    this.splitContainer1.Panel1.SuspendLayout();
		    this.splitContainer1.Panel2.SuspendLayout();
		    this.splitContainer1.SuspendLayout();
		    this.SuspendLayout();
		    // 
		    // ToolBar1
		    // 
		    this.ToolBar1.GripStyle = System.Windows.Forms.ToolStripGripStyle.Hidden;
		    this.ToolBar1.ImageScalingSize = new System.Drawing.Size(20, 20);
		    this.ToolBar1.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
		    		    		    this.tbNewScript,
		    		    		    this.tbLoadScript,
		    		    		    this.tbSaveScript,
		    		    		    this.ToolButton2,
		    		    		    this.tbRun});
		    this.ToolBar1.Location = new System.Drawing.Point(0, 0);
		    this.ToolBar1.Name = "ToolBar1";
		    this.ToolBar1.Size = new System.Drawing.Size(712, 25);
		    this.ToolBar1.TabIndex = 0;
		    // 
		    // tbNewScript
		    // 
		    this.tbNewScript.Name = "tbNewScript";
		    this.tbNewScript.Size = new System.Drawing.Size(23, 22);
		    this.tbNewScript.Click += new System.EventHandler(this.tbNewScript_Click);
		    // 
		    // tbLoadScript
		    // 
		    this.tbLoadScript.Name = "tbLoadScript";
		    this.tbLoadScript.Size = new System.Drawing.Size(23, 22);
		    this.tbLoadScript.Click += new System.EventHandler(this.tbLoadScript_Click);
		    // 
		    // tbSaveScript
		    // 
		    this.tbSaveScript.Name = "tbSaveScript";
		    this.tbSaveScript.Size = new System.Drawing.Size(23, 22);
		    this.tbSaveScript.Click += new System.EventHandler(this.tbSaveScript_Click);
		    // 
		    // ToolButton2
		    // 
		    this.ToolButton2.Name = "ToolButton2";
		    this.ToolButton2.Size = new System.Drawing.Size(6, 25);
		    // 
		    // tbRun
		    // 
		    this.tbRun.Name = "tbRun";
		    this.tbRun.Size = new System.Drawing.Size(23, 22);
		    this.tbRun.Click += new System.EventHandler(this.tbRun_Click);
		    // 
		    // splitContainer1
		    // 
		    this.splitContainer1.Dock = System.Windows.Forms.DockStyle.Fill;
		    this.splitContainer1.Location = new System.Drawing.Point(0, 25);
		    this.splitContainer1.Name = "splitContainer1";
		    this.splitContainer1.Orientation = System.Windows.Forms.Orientation.Horizontal;
		    // 
		    // splitContainer1.Panel1
		    // 
		    this.splitContainer1.Panel1.Controls.Add(this.txtScriptText);
		    // 
		    // splitContainer1.Panel2
		    // 
		    this.splitContainer1.Panel2.Controls.Add(this.txtDebugOutput);
		    this.splitContainer1.Size = new System.Drawing.Size(712, 409);
		    this.splitContainer1.SplitterDistance = 240;
		    this.splitContainer1.TabIndex = 3;
		    // 
		    // txtScriptText
		    // 
		    this.txtScriptText.Dock = System.Windows.Forms.DockStyle.Fill;
		    this.txtScriptText.Location = new System.Drawing.Point(0, 0);
		    this.txtScriptText.Multiline = true;
		    this.txtScriptText.Name = "txtScriptText";
		    this.txtScriptText.ScrollBars = System.Windows.Forms.ScrollBars.Both;
		    this.txtScriptText.Size = new System.Drawing.Size(712, 240);
		    this.txtScriptText.TabIndex = 2;
		    // 
		    // txtDebugOutput
		    // 
		    this.txtDebugOutput.Dock = System.Windows.Forms.DockStyle.Fill;
		    this.txtDebugOutput.Location = new System.Drawing.Point(0, 0);
		    this.txtDebugOutput.Multiline = true;
		    this.txtDebugOutput.Name = "txtDebugOutput";
		    this.txtDebugOutput.ReadOnly = true;
		    this.txtDebugOutput.ScrollBars = System.Windows.Forms.ScrollBars.Both;
		    this.txtDebugOutput.Size = new System.Drawing.Size(712, 165);
		    this.txtDebugOutput.TabIndex = 3;
		    // 
		    // ScriptEditWin
		    // 
		    this.AutoScaleDimensions = new System.Drawing.SizeF(120F, 120F);
		    this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
		    this.ClientSize = new System.Drawing.Size(712, 434);
		    this.Controls.Add(this.splitContainer1);
		    this.Controls.Add(this.ToolBar1);
		    this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
		    this.KeyPreview = true;
		    this.Name = "ScriptEditWin";
		    this.ShowInTaskbar = false;
		    this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
		    this.Text = "ScriptEditWin";
		    this.Closing += new System.ComponentModel.CancelEventHandler(this.ScriptEditWin_Closing);
		    this.KeyDown += new System.Windows.Forms.KeyEventHandler(this.ScriptEditWin_KeyDown);
		    this.ToolBar1.ResumeLayout(false);
		    this.ToolBar1.PerformLayout();
		    this.splitContainer1.Panel1.ResumeLayout(false);
		    this.splitContainer1.Panel1.PerformLayout();
		    this.splitContainer1.Panel2.ResumeLayout(false);
		    this.splitContainer1.Panel2.PerformLayout();
		    this.splitContainer1.ResumeLayout(false);
		    this.ResumeLayout(false);
		    this.PerformLayout();
		}
	}
}