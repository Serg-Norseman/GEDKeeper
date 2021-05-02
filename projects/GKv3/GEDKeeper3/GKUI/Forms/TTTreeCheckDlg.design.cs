using Eto.Drawing;
using Eto.Forms;
using GKUI.Components;

namespace GKUI.Forms
{
    partial class TTTreeCheckDlg
    {
        private TabControl tabsTools;
        private Button btnClose;
        private TabPage pageTreeCheck;
        private Button btnAnalyseBase;
        private Button btnBaseRepair;
        private Panel panProblemsContainer;
        private ContextMenu contextMenu;
        private ButtonMenuItem miDetails;
        private ButtonMenuItem miGoToRecord;
        private ButtonMenuItem miCopyXRef;

        private void InitializeComponent()
        {
            SuspendLayout();

            btnAnalyseBase = new Button();
            btnAnalyseBase.Size = new Size(130, 26);
            btnAnalyseBase.Text = "btnAnalyseBase";
            btnAnalyseBase.Click += btnAnalyseBase_Click;

            btnBaseRepair = new Button();
            btnBaseRepair.Size = new Size(130, 26);
            btnBaseRepair.Text = "btnBaseRepair";
            btnBaseRepair.Click += btnBaseRepair_Click;

            panProblemsContainer = new Panel();
            panProblemsContainer.Size = new Size(880, 400);

            pageTreeCheck = new TabPage();
            pageTreeCheck.Text = "pageTreeCheck";
            pageTreeCheck.Content = new DefTableLayout {
                Rows = {
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { panProblemsContainer }
                    },
                    UIHelper.MakeDialogFooter(btnAnalyseBase, null, btnBaseRepair)
                }
            };

            miDetails = new ButtonMenuItem();
            miDetails.Text = "miDetails";
            miDetails.Click += miDetails_Click;

            miGoToRecord = new ButtonMenuItem();
            miGoToRecord.Text = "miGoToRecord";
            miGoToRecord.Click += miGoToRecord_Click;

            miCopyXRef = new ButtonMenuItem();
            miCopyXRef.Text = "miCopyXRef";
            miCopyXRef.Click += miCopyXRef_Click;

            contextMenu = new ContextMenu();
            contextMenu.Items.AddRange(new MenuItem[] {
                miDetails,
                miGoToRecord,
                miCopyXRef
            });
            contextMenu.Opening += contextMenu_Opening;

            //

            tabsTools = new TabControl();
            tabsTools.Pages.Add(pageTreeCheck);
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
