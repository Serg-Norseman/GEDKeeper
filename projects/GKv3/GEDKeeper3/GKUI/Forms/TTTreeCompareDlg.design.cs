using Eto.Drawing;
using Eto.Forms;
using GKUI.Components;

namespace GKUI.Forms
{
    partial class TTTreeCompareDlg
    {
        private TabControl tabsTools;
        private TabPage pageTreeCompare;
        private GKUI.Components.TextBoxEx ListCompare;
        private Button btnClose;
        private Label lblFile;
        private TextBox txtCompareFile;
        private Button btnFileChoose;
        private RadioButton radAnalysis;
        private Button btnMatch;
        private RadioButton radMathExternal;
        private RadioButton radMatchInternal;
        private GroupBox grpMatchType;

        private void InitializeComponent()
        {
            SuspendLayout();

            radMatchInternal = new RadioButton();
            radMatchInternal.Checked = true;
            radMatchInternal.Text = "radMatchInternal";
            radMatchInternal.CheckedChanged += rbtnMatch_CheckedChanged;

            radMathExternal = new RadioButton(radMatchInternal);
            radMathExternal.Text = "radMathExternal";
            radMathExternal.CheckedChanged += rbtnMatch_CheckedChanged;

            radAnalysis = new RadioButton(radMatchInternal);
            radAnalysis.Text = "radAnalysis";
            radAnalysis.CheckedChanged += rbtnMatch_CheckedChanged;

            lblFile = new Label();
            lblFile.Enabled = false;
            lblFile.Text = "lblFile";

            txtCompareFile = new TextBox();
            txtCompareFile.Enabled = false;
            txtCompareFile.ReadOnly = true;
            txtCompareFile.Width = 500;

            btnFileChoose = new Button();
            btnFileChoose.Enabled = false;
            btnFileChoose.Size = new Size(130, 26);
            btnFileChoose.Text = "btnFileChoose";
            btnFileChoose.Click += btnFileChoose_Click;

            btnMatch = new Button();
            btnMatch.Size = new Size(130, 26);
            btnMatch.Text = "btnMatch";
            btnMatch.Click += btnMatch_Click;

            grpMatchType = new GroupBox();
            grpMatchType.Text = "grpMatchType";
            grpMatchType.Content = new DefTableLayout {
                Rows = {
                    new TableRow {
                        Cells = { radMatchInternal }
                    },
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { TableLayout.Horizontal(10, radMathExternal, txtCompareFile, null, btnFileChoose) }
                    },
                    new TableRow {
                        Cells = { TableLayout.Horizontal(10, radAnalysis, null, btnMatch) }
                    }
                }
            };

            ListCompare = new GKUI.Components.TextBoxEx();
            ListCompare.ReadOnly = true;
            ListCompare.Height = 160;

            pageTreeCompare = new TabPage();
            pageTreeCompare.Text = "pageTreeCompare";
            pageTreeCompare.Content = new DefTableLayout {
                Rows = {
                    new TableRow {
                        Cells = { grpMatchType }
                    },
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { ListCompare }
                    }
                }
            };

            //

            tabsTools = new TabControl();
            tabsTools.Pages.Add(pageTreeCompare);
            tabsTools.SelectedIndex = 0;

            btnClose = new Button();
            btnClose.ImagePosition = ButtonImagePosition.Left;
            btnClose.Size = new Size(130, 26);
            btnClose.Text = "btnClose";
            btnClose.Click += (sender, e) => { Close(); };

            Content = new DefTableLayout {
                Rows = {
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { tabsTools }
                    },
                    UIHelper.MakeDialogFooter(null, btnClose)
                }
            };

            AbortButton = btnClose;
            Maximizable = false;
            Minimizable = false;
            ShowInTaskbar = false;
            Title = "TreeToolsWin";

            UIHelper.SetPredefProperties(this, 1030, 620);
            ResumeLayout();
        }
    }
}
