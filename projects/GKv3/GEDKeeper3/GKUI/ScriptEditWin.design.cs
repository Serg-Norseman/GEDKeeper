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
        private ButtonToolItem tbRun;
        private GKUI.Components.TextBoxEx txtDebugOutput;
        private TextBoxEx txtScriptText;
        private ButtonToolItem tbSaveScript;
        private ButtonToolItem tbNewScript;
        private Splitter splitContainer1;

        private void InitializeComponent()
        {
            SuspendLayout();

            tbNewScript = new ButtonToolItem();
            tbNewScript.Click += tbNewScript_Click;

            tbLoadScript = new ButtonToolItem();
            tbLoadScript.Click += tbLoadScript_Click;

            tbSaveScript = new ButtonToolItem();
            tbSaveScript.Click += tbSaveScript_Click;

            tbRun = new ButtonToolItem();
            tbRun.Click += tbRun_Click;

            ToolBar1 = new ToolBar();
            ToolBar1.Items.AddRange(new ToolItem[] {
                                        tbNewScript,
                                        tbLoadScript,
                                        tbSaveScript,
                                        new SeparatorToolItem(),
                                        tbRun});

            txtScriptText = new TextBoxEx();

            txtDebugOutput = new GKUI.Components.TextBoxEx();
            txtDebugOutput.ReadOnly = true;

            splitContainer1 = new Splitter();
            splitContainer1.Orientation = Orientation.Vertical;
            splitContainer1.Panel1 = txtScriptText;
            splitContainer1.Panel2 = txtDebugOutput;
            splitContainer1.FixedPanel = SplitterFixedPanel.Panel2;
            splitContainer1.Position = 300;

            Content = splitContainer1;
            ToolBar = ToolBar1;

            ClientSize = new Size(710, 430);
            Resizable = true;
            ShowInTaskbar = true;
            Title = "ScriptEditWin";
            Closing += ScriptEditWin_Closing;
            KeyDown += ScriptEditWin_KeyDown;

            UIHelper.SetControlFont(this, "Tahoma", 8.25f);
            ResumeLayout();
        }
    }
}
