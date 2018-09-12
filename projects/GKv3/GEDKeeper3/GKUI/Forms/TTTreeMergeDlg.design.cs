using Eto.Drawing;
using Eto.Forms;
using GKUI.Components;

namespace GKUI.Forms
{
    partial class TTTreeMergeDlg
    {
        private TabControl tabsTools;
        private Button btnClose;
        private TabPage pageTreeMerge;
        private Label lblMasterBase;
        private TextBox edMasterBase;
        private Label lblOtherBase;
        private TextBox edUpdateBase;
        private Button btnTreeMerge;
        private TextArea mSyncRes;

        private void InitializeComponent()
        {
            SuspendLayout();

            lblMasterBase = new Label();
            //lblMasterBase.Size = new Size(88, 17);
            lblMasterBase.Text = "lblMasterBase";

            lblOtherBase = new Label();
            //lblOtherBase.Size = new Size(122, 17);
            lblOtherBase.Text = "lblOtherBase";

            edMasterBase = new TextBox();
            edMasterBase.BackgroundColor = SystemColors.Control;
            edMasterBase.ReadOnly = true;
            //edMasterBase.Size = new Size(853, 24);
            edMasterBase.Text = "edMasterBase";

            edUpdateBase = new TextBox();
            edUpdateBase.ReadOnly = true;
            edUpdateBase.Width = 600;

            btnTreeMerge = new Button();
            btnTreeMerge.Size = new Size(130, 26);
            btnTreeMerge.Text = "btnTreeMerge";
            btnTreeMerge.Click += btnTreeMerge_Click;

            mSyncRes = new TextArea();
            mSyncRes.ReadOnly = true;
            mSyncRes.Height = 160;

            pageTreeMerge = new TabPage();
            pageTreeMerge.Text = "pageTreeMerge";
            pageTreeMerge.Content = new DefTableLayout {
                Rows = {
                    new DefTableLayout {
                        Rows = {
                            new TableRow {
                                Cells = { lblMasterBase, edMasterBase }
                            },
                            new TableRow {
                                Cells = { lblOtherBase, TableLayout.Horizontal(10, edUpdateBase, btnTreeMerge) }
                            }
                        }
                    },
                    mSyncRes
                }
            };

            tabsTools = new TabControl();
            tabsTools.Pages.Add(pageTreeMerge);
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
