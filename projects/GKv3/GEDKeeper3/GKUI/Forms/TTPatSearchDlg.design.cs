using Eto.Drawing;
using Eto.Forms;
using GKUI.Components;

namespace GKUI.Forms
{
    partial class TTPatSearchDlg
    {
        private TabControl tabsTools;
        private Button btnClose;
        private TabPage pagePatSearch;
        private Button btnPatSearch;
        private Panel panPatriarchsContainer;
        private Label lblMinGenerations;
        private NumericUpDown edMinGens;
        private Button btnSetPatriarch;
        private Button btnPatriarchsDiagram;
        private CheckBox chkWithoutDates;

        private void InitializeComponent()
        {
            SuspendLayout();

            btnPatriarchsDiagram = new Button();
            btnPatriarchsDiagram.Size = new Size(130, 26);
            btnPatriarchsDiagram.Text = "btnPatriarchsDiagram";
            btnPatriarchsDiagram.Click += btnPatriarchsDiagram_Click;

            chkWithoutDates = new CheckBox();
            chkWithoutDates.Text = "chkWithoutDates";

            lblMinGenerations = new Label();
            lblMinGenerations.Text = "lblMinGenerations";

            btnPatSearch = new Button();
            btnPatSearch.Size = new Size(130, 26);
            btnPatSearch.Text = "btnPatSearch";
            btnPatSearch.Click += btnPatSearch_Click;

            panPatriarchsContainer = new Panel();
            panPatriarchsContainer.Size = new Size(880, 400);

            edMinGens = new NumericUpDown();
            edMinGens.Value = 2;

            btnSetPatriarch = new Button();
            btnSetPatriarch.Size = new Size(130, 26);
            btnSetPatriarch.Text = "btnSetPatriarch";
            btnSetPatriarch.Click += btnSetPatriarch_Click;

            pagePatSearch = new TabPage();
            pagePatSearch.Text = "pagePatSearch";
            pagePatSearch.Content = new DefTableLayout {
                Rows = {
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { panPatriarchsContainer }
                    },
                    UIHelper.MakeDialogFooter(lblMinGenerations, edMinGens, chkWithoutDates, null, btnSetPatriarch, btnPatSearch, btnPatriarchsDiagram)
                }
            };

            //

            tabsTools = new TabControl();
            tabsTools.Pages.Add(pagePatSearch);
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
