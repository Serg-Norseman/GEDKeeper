using Eto.Drawing;
using Eto.Forms;
using GKUI.Components;

namespace GKUI.Forms
{
    partial class TTTreeSplitDlg
    {
        private TabControl tabsTools;
        private Button btnClose;
        private TabPage pageTreeSplit;
        private Button btnSelectAll;
        private GKListView ListSelected;
        private GKListView ListSkipped;
        private Button btnSelectFamily;
        private Button btnSelectAncestors;
        private Button btnSelectDescendants;
        private Button btnDelete;
        private Button btnSave;

        private void InitializeComponent()
        {
            SuspendLayout();

            ListSelected = new GKListView();
            ListSelected.Height = 200;

            ListSkipped = new GKListView();
            ListSkipped.Height = 200;

            btnSelectAll = new Button();
            btnSelectAll.Size = new Size(130, 26);
            btnSelectAll.Text = "btnSelectAll";
            btnSelectAll.Click += btnSelectAll_Click;

            btnSelectFamily = new Button();
            btnSelectFamily.Size = new Size(130, 26);
            btnSelectFamily.Text = "btnSelectFamily";
            btnSelectFamily.Click += btnSelectFamily_Click;

            btnSelectAncestors = new Button();
            btnSelectAncestors.Size = new Size(130, 26);
            btnSelectAncestors.Text = "btnSelectAncestors";
            btnSelectAncestors.Click += btnSelectAncestors_Click;

            btnSelectDescendants = new Button();
            btnSelectDescendants.Size = new Size(130, 26);
            btnSelectDescendants.Text = "btnSelectDescendants";
            btnSelectDescendants.Click += btnSelectDescendants_Click;

            btnDelete = new Button();
            btnDelete.Size = new Size(130, 26);
            btnDelete.Text = "btnDelete";
            btnDelete.Click += btnDelete_Click;

            btnSave = new Button();
            btnSave.Size = new Size(130, 26);
            btnSave.Text = "btnSave";
            btnSave.Click += btnSave_Click;

            pageTreeSplit = new TabPage();
            pageTreeSplit.Text = "pageTreeSplit";
            pageTreeSplit.Content = new DefTableLayout {
                Rows = {
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { TableLayout.HorizontalScaled(10, ListSelected, ListSkipped) }
                    },
                    UIHelper.MakeDialogFooter(btnSelectAll, btnSelectFamily, btnSelectAncestors, btnSelectDescendants, null, btnSave, btnDelete)
                }
            };

            //

            tabsTools = new TabControl();
            tabsTools.Pages.Add(pageTreeSplit);
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
