using System;
using Eto.Drawing;
using Eto.Forms;
using GKUI.Components;

namespace GKUI
{
    partial class ScriptEditWin
    {
        private ToolBar ToolBar1;
        private ButtonToolItem tbLoadScript;
        private SeparatorToolItem ToolButton2;
        private ButtonToolItem tbRun;
        private GKUI.Components.TextBoxEx txtDebugOutput;
        private TextBoxEx txtScriptText;
        private ButtonToolItem tbSaveScript;
        private ButtonToolItem tbNewScript;
        private Splitter splitContainer1;

        private void InitializeComponent()
        {
            ToolBar1 = new ToolBar();

            SuspendLayout();

            tbNewScript = new ButtonToolItem();
            tbNewScript.Click += tbNewScript_Click;
            tbLoadScript = new ButtonToolItem();
            tbLoadScript.Click += tbLoadScript_Click;
            tbSaveScript = new ButtonToolItem();
            tbSaveScript.Click += tbSaveScript_Click;
            tbRun = new ButtonToolItem();
            tbRun.Click += tbRun_Click;

            ToolBar1.Items.AddRange(new ToolItem[] {
                                        tbNewScript,
                                        tbLoadScript,
                                        tbSaveScript,
                                        new SeparatorToolItem(),
                                        tbRun});

            txtScriptText = new TextBoxEx();
            txtScriptText.Size = new Size(712, 240);

            txtDebugOutput = new GKUI.Components.TextBoxEx();
            txtDebugOutput.ReadOnly = true;
            txtDebugOutput.Size = new Size(712, 165);

            splitContainer1 = new Splitter();
            splitContainer1.Orientation = Orientation.Horizontal;
            splitContainer1.Panel1 = txtScriptText;
            splitContainer1.Panel2 = txtDebugOutput;

            Content = splitContainer1;

            ClientSize = new Size(712, 434);
            ShowInTaskbar = true;
            Title = "ScriptEditWin";
            Closing += ScriptEditWin_Closing;
            KeyDown += ScriptEditWin_KeyDown;

            UIHelper.SetControlFont(this, "Tahoma", 8.25f);
            ResumeLayout();
        }
    }
}
