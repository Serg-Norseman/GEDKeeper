using System;
using Eto.Drawing;
using Eto.Forms;

namespace GKUI
{
    partial class ScriptEditWin
    {
        private ToolBar ToolBar1;
        private ButtonToolItem tbLoadScript;
        private SeparatorToolItem ToolButton2;
        private ButtonToolItem tbRun;
        private GKUI.Components.TextBoxEx txtDebugOutput;
        private TextArea txtScriptText;
        private ButtonToolItem tbSaveScript;
        private ButtonToolItem tbNewScript;
        private Splitter splitContainer1;

        private void InitializeComponent()
        {
            ToolBar1 = new ToolBar();
            tbNewScript = new ButtonToolItem();
            tbLoadScript = new ButtonToolItem();
            tbSaveScript = new ButtonToolItem();
            ToolButton2 = new SeparatorToolItem();
            tbRun = new ButtonToolItem();
            splitContainer1 = new Splitter();
            txtScriptText = new TextArea();
            txtDebugOutput = new GKUI.Components.TextBoxEx();
            splitContainer1.Panel1.SuspendLayout();
            splitContainer1.Panel2.SuspendLayout();
            splitContainer1.SuspendLayout();
            SuspendLayout();

            ToolBar1.Items.AddRange(new ToolItem[] {
                                        tbNewScript,
                                        tbLoadScript,
                                        tbSaveScript,
                                        ToolButton2,
                                        tbRun});
            ToolBar1.Location = new Point(0, 0);
            ToolBar1.Size = new Size(712, 25);

            tbNewScript.Click += tbNewScript_Click;

            tbLoadScript.Click += tbLoadScript_Click;

            tbSaveScript.Click += tbSaveScript_Click;

            tbRun.Click += tbRun_Click;

            splitContainer1.Dock = DockStyle.Fill;
            splitContainer1.Location = new Point(0, 25);
            splitContainer1.Orientation = Orientation.Horizontal;

            splitContainer1.Panel1.Controls.Add(txtScriptText);

            splitContainer1.Panel2.Controls.Add(txtDebugOutput);
            splitContainer1.Size = new Size(712, 409);
            splitContainer1.SplitterDistance = 240;

            txtScriptText.Dock = DockStyle.Fill;
            txtScriptText.Location = new Point(0, 0);
            txtScriptText.Size = new Size(712, 240);

            txtDebugOutput.Dock = DockStyle.Fill;
            txtDebugOutput.Location = new Point(0, 0);
            txtDebugOutput.ReadOnly = true;
            txtDebugOutput.Size = new Size(712, 165);

            ClientSize = new Size(712, 434);
            Controls.Add(splitContainer1);
            Controls.Add(ToolBar1);
            //Font = new Font("Tahoma", 8.25F, FontStyle.None);
            ShowInTaskbar = false;
            Title = "ScriptEditWin";
            Closing += ScriptEditWin_Closing;
            KeyDown += ScriptEditWin_KeyDown;
            splitContainer1.Panel1.ResumeLayout();
            splitContainer1.Panel2.ResumeLayout();
            splitContainer1.ResumeLayout();
            ResumeLayout();
        }
    }
}
