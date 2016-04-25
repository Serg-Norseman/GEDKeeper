using System;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;

namespace GKUI
{
	partial class ScriptEditWin
	{
		private System.Windows.Forms.ToolStrip ToolBar1;
		private System.Windows.Forms.ToolStripButton btnLoadScript;
		private System.Windows.Forms.ToolStripSeparator ToolButton2;
		private System.Windows.Forms.ToolStripButton btnRun;
		private System.Windows.Forms.TextBox mmDebugOutput;
		private System.Windows.Forms.TextBox mmScriptText;
		private System.Windows.Forms.OpenFileDialog OpenDialog1;
		private System.Windows.Forms.ToolStripButton btnSaveScript;
		private System.Windows.Forms.SaveFileDialog SaveDialog1;
		private System.Windows.Forms.ToolStripButton btnNewScript;
		private System.Windows.Forms.SplitContainer splitContainer1;

		private void InitializeComponent()
		{
		    System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(ScriptEditWin));
		    this.ToolBar1 = new System.Windows.Forms.ToolStrip();
		    this.btnNewScript = new System.Windows.Forms.ToolStripButton();
		    this.btnLoadScript = new System.Windows.Forms.ToolStripButton();
		    this.btnSaveScript = new System.Windows.Forms.ToolStripButton();
		    this.ToolButton2 = new System.Windows.Forms.ToolStripSeparator();
		    this.btnRun = new System.Windows.Forms.ToolStripButton();
		    this.OpenDialog1 = new System.Windows.Forms.OpenFileDialog();
		    this.SaveDialog1 = new System.Windows.Forms.SaveFileDialog();
		    this.splitContainer1 = new System.Windows.Forms.SplitContainer();
		    this.mmScriptText = new System.Windows.Forms.TextBox();
		    this.mmDebugOutput = new System.Windows.Forms.TextBox();
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
		    		    		    this.btnNewScript,
		    		    		    this.btnLoadScript,
		    		    		    this.btnSaveScript,
		    		    		    this.ToolButton2,
		    		    		    this.btnRun});
		    this.ToolBar1.Location = new System.Drawing.Point(0, 0);
		    this.ToolBar1.Name = "ToolBar1";
		    this.ToolBar1.Size = new System.Drawing.Size(712, 27);
		    this.ToolBar1.TabIndex = 0;
		    // 
		    // btnNewScript
		    // 
		    this.btnNewScript.Image = global::GKResources.iCreateNew;
		    this.btnNewScript.Name = "btnNewScript";
		    this.btnNewScript.Size = new System.Drawing.Size(24, 24);
		    this.btnNewScript.ToolTipText = "Новый скрипт";
		    this.btnNewScript.Click += new System.EventHandler(this.ToolBar1_ButtonClick);
		    // 
		    // btnLoadScript
		    // 
		    this.btnLoadScript.Image = global::GKResources.iLoad;
		    this.btnLoadScript.Name = "btnLoadScript";
		    this.btnLoadScript.Size = new System.Drawing.Size(24, 24);
		    this.btnLoadScript.ToolTipText = "Загрузить скрипт";
		    this.btnLoadScript.Click += new System.EventHandler(this.ToolBar1_ButtonClick);
		    // 
		    // btnSaveScript
		    // 
		    this.btnSaveScript.Image = global::GKResources.iSave;
		    this.btnSaveScript.Name = "btnSaveScript";
		    this.btnSaveScript.Size = new System.Drawing.Size(24, 24);
		    this.btnSaveScript.ToolTipText = "Сохранить скрипт";
		    this.btnSaveScript.Click += new System.EventHandler(this.ToolBar1_ButtonClick);
		    // 
		    // ToolButton2
		    // 
		    this.ToolButton2.Name = "ToolButton2";
		    this.ToolButton2.Size = new System.Drawing.Size(6, 27);
		    // 
		    // btnRun
		    // 
		    this.btnRun.Image = ((System.Drawing.Image)(resources.GetObject("btnRun.Image")));
		    this.btnRun.Name = "btnRun";
		    this.btnRun.Size = new System.Drawing.Size(24, 24);
		    this.btnRun.ToolTipText = "Выполнить";
		    this.btnRun.Click += new System.EventHandler(this.ToolBar1_ButtonClick);
		    // 
		    // OpenDialog1
		    // 
		    this.OpenDialog1.Filter = "Скрипты|*.lua";
		    // 
		    // SaveDialog1
		    // 
		    this.SaveDialog1.DefaultExt = "lua";
		    this.SaveDialog1.Filter = "Скрипты|*.lua";
		    // 
		    // splitContainer1
		    // 
		    this.splitContainer1.Dock = System.Windows.Forms.DockStyle.Fill;
		    this.splitContainer1.Location = new System.Drawing.Point(0, 27);
		    this.splitContainer1.Name = "splitContainer1";
		    this.splitContainer1.Orientation = System.Windows.Forms.Orientation.Horizontal;
		    // 
		    // splitContainer1.Panel1
		    // 
		    this.splitContainer1.Panel1.Controls.Add(this.mmScriptText);
		    // 
		    // splitContainer1.Panel2
		    // 
		    this.splitContainer1.Panel2.Controls.Add(this.mmDebugOutput);
		    this.splitContainer1.Size = new System.Drawing.Size(712, 407);
		    this.splitContainer1.SplitterDistance = 239;
		    this.splitContainer1.TabIndex = 3;
		    // 
		    // mmScriptText
		    // 
		    this.mmScriptText.Dock = System.Windows.Forms.DockStyle.Fill;
		    this.mmScriptText.Location = new System.Drawing.Point(0, 0);
		    this.mmScriptText.Multiline = true;
		    this.mmScriptText.Name = "mmScriptText";
		    this.mmScriptText.ScrollBars = System.Windows.Forms.ScrollBars.Both;
		    this.mmScriptText.Size = new System.Drawing.Size(712, 239);
		    this.mmScriptText.TabIndex = 2;
		    // 
		    // mmDebugOutput
		    // 
		    this.mmDebugOutput.Dock = System.Windows.Forms.DockStyle.Fill;
		    this.mmDebugOutput.Location = new System.Drawing.Point(0, 0);
		    this.mmDebugOutput.Multiline = true;
		    this.mmDebugOutput.Name = "mmDebugOutput";
		    this.mmDebugOutput.ReadOnly = true;
		    this.mmDebugOutput.ScrollBars = System.Windows.Forms.ScrollBars.Both;
		    this.mmDebugOutput.Size = new System.Drawing.Size(712, 164);
		    this.mmDebugOutput.TabIndex = 3;
		    // 
		    // ScriptEditWin
		    // 
		    this.AutoScaleDimensions = new System.Drawing.SizeF(120F, 120F);
		    this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
		    this.ClientSize = new System.Drawing.Size(712, 434);
		    this.Controls.Add(this.splitContainer1);
		    this.Controls.Add(this.ToolBar1);
		    this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
		    this.Name = "ScriptEditWin";
		    this.ShowInTaskbar = false;
		    this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
		    this.Text = "ScriptDaemon";
		    this.Closing += new System.ComponentModel.CancelEventHandler(this.TfmScriptDaemon_Closing);
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