using Eto.Drawing;
using Eto.Forms;
using GKUI.Components;

namespace GKUI.Forms
{
    partial class ScriptEditWin
    {
        private ToolBar ToolBar1;
        private ButtonToolItem tbLoadScript;
        private ButtonToolItem tbRun;
        private TextArea txtDebugOutput;
        private TextArea txtScriptText;
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
            ToolBar1.TextAlign = ToolBarTextAlign.Right;
            ToolBar1.Items.AddRange(new ToolItem[] {
                                        tbNewScript,
                                        tbLoadScript,
                                        tbSaveScript,
                                        new SeparatorToolItem(),
                                        tbRun});

            txtScriptText = new TextArea();
            txtScriptText.TextChanged += mmScriptText_TextChanged;

            txtDebugOutput = new TextArea();
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

            UIHelper.SetPredefProperties(this, 710, 430);
            ResumeLayout();
        }
    }
}
