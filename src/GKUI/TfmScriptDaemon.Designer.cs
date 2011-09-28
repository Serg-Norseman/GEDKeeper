using System;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;

namespace GKUI
{
	partial class TfmScriptDaemon
	{
		private System.Windows.Forms.ToolBar ToolBar1;
		private System.Windows.Forms.ToolBarButton btnLoadScript;
		private System.Windows.Forms.ToolBarButton ToolButton2;
		private System.Windows.Forms.ToolBarButton btnRun;
		private System.Windows.Forms.TextBox mmDebugOutput;
		private System.Windows.Forms.TextBox mmScriptText;
		private System.Windows.Forms.OpenFileDialog OpenDialog1;
		private System.Windows.Forms.ToolBarButton btnSaveScript;
		private System.Windows.Forms.SaveFileDialog SaveDialog1;
		private System.Windows.Forms.ToolBarButton btnNewScript;

		private void InitializeComponent()
		{
			this.ToolBar1 = new System.Windows.Forms.ToolBar();
			this.btnNewScript = new System.Windows.Forms.ToolBarButton();
			this.btnLoadScript = new System.Windows.Forms.ToolBarButton();
			this.btnSaveScript = new System.Windows.Forms.ToolBarButton();
			this.ToolButton2 = new System.Windows.Forms.ToolBarButton();
			this.btnRun = new System.Windows.Forms.ToolBarButton();
			this.mmDebugOutput = new System.Windows.Forms.TextBox();
			this.OpenDialog1 = new System.Windows.Forms.OpenFileDialog();
			this.SaveDialog1 = new System.Windows.Forms.SaveFileDialog();
			this.mmScriptText = new System.Windows.Forms.TextBox();
			this.SuspendLayout();
			// 
			// ToolBar1
			// 
			this.ToolBar1.Appearance = System.Windows.Forms.ToolBarAppearance.Flat;
			this.ToolBar1.Buttons.AddRange(new System.Windows.Forms.ToolBarButton[] {
									this.btnNewScript,
									this.btnLoadScript,
									this.btnSaveScript,
									this.ToolButton2,
									this.btnRun});
			this.ToolBar1.DropDownArrows = true;
			this.ToolBar1.Location = new System.Drawing.Point(0, 0);
			this.ToolBar1.Name = "ToolBar1";
			this.ToolBar1.ShowToolTips = true;
			this.ToolBar1.Size = new System.Drawing.Size(712, 28);
			this.ToolBar1.TabIndex = 0;
			this.ToolBar1.ButtonClick += new System.Windows.Forms.ToolBarButtonClickEventHandler(this.ToolBar1_ButtonClick);
			// 
			// btnNewScript
			// 
			this.btnNewScript.ImageIndex = 0;
			this.btnNewScript.Name = "btnNewScript";
			this.btnNewScript.ToolTipText = "Новый скрипт";
			// 
			// btnLoadScript
			// 
			this.btnLoadScript.ImageIndex = 1;
			this.btnLoadScript.Name = "btnLoadScript";
			this.btnLoadScript.ToolTipText = "Загрузить скрипт";
			// 
			// btnSaveScript
			// 
			this.btnSaveScript.ImageIndex = 2;
			this.btnSaveScript.Name = "btnSaveScript";
			this.btnSaveScript.ToolTipText = "Сохранить скрипт";
			// 
			// ToolButton2
			// 
			this.ToolButton2.Name = "ToolButton2";
			this.ToolButton2.Style = System.Windows.Forms.ToolBarButtonStyle.Separator;
			// 
			// btnRun
			// 
			this.btnRun.ImageIndex = 33;
			this.btnRun.Name = "btnRun";
			this.btnRun.ToolTipText = "Выполнить";
			// 
			// mmDebugOutput
			// 
			this.mmDebugOutput.Dock = System.Windows.Forms.DockStyle.Bottom;
			this.mmDebugOutput.Location = new System.Drawing.Point(0, 319);
			this.mmDebugOutput.Multiline = true;
			this.mmDebugOutput.Name = "mmDebugOutput";
			this.mmDebugOutput.ReadOnly = true;
			this.mmDebugOutput.ScrollBars = System.Windows.Forms.ScrollBars.Both;
			this.mmDebugOutput.Size = new System.Drawing.Size(712, 115);
			this.mmDebugOutput.TabIndex = 2;
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
			// mmScriptText
			// 
			this.mmScriptText.Dock = System.Windows.Forms.DockStyle.Fill;
			this.mmScriptText.Location = new System.Drawing.Point(0, 28);
			this.mmScriptText.Multiline = true;
			this.mmScriptText.Name = "mmScriptText";
			this.mmScriptText.ScrollBars = System.Windows.Forms.ScrollBars.Both;
			this.mmScriptText.Size = new System.Drawing.Size(712, 291);
			this.mmScriptText.TabIndex = 1;
			// 
			// TfmScriptDaemon
			// 
			this.AutoScaleBaseSize = new System.Drawing.Size(5, 14);
			this.ClientSize = new System.Drawing.Size(712, 434);
			this.Controls.Add(this.mmScriptText);
			this.Controls.Add(this.ToolBar1);
			this.Controls.Add(this.mmDebugOutput);
			this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
			this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
			this.Name = "TfmScriptDaemon";
			this.ShowInTaskbar = false;
			this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
			this.Text = "ScriptDaemon";
			this.Closing += new System.ComponentModel.CancelEventHandler(this.TfmScriptDaemon_Closing);
			this.ResumeLayout(false);
			this.PerformLayout();
		}
	}
}