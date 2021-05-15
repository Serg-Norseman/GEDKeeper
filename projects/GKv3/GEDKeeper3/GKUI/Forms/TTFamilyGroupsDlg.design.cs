using Eto.Drawing;
using Eto.Forms;
using GKUI.Components;

namespace GKUI.Forms
{
    partial class TTFamilyGroupsDlg
    {
        private TabControl tabsTools;
        private Button btnClose;
        private TabPage pageFamilyGroups;
        private TreeView tvGroups;
        private GKUI.Components.LogChart gkLogChart1;
        private Button btnAnalyseGroups;
        private ContextMenu contextMenu;
        private ButtonMenuItem miDetails;
        private ButtonMenuItem miGoToRecord;
        private ButtonMenuItem miCopyXRef;

        private void InitializeComponent()
        {
            SuspendLayout();

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

            gkLogChart1 = new GKUI.Components.LogChart();
            gkLogChart1.Height = 34;
            gkLogChart1.OnHintRequest += HintRequestEventHandler;

            tvGroups = new TreeView();
            tvGroups.LabelEdit = false;
            tvGroups.MouseDoubleClick += tvGroups_DoubleClick;
            tvGroups.Size = new Size(780, 350);
            tvGroups.ContextMenu = contextMenu;

            btnAnalyseGroups = new Button();
            btnAnalyseGroups.Size = new Size(130, 26);
            btnAnalyseGroups.Text = "btnSkip";
            btnAnalyseGroups.Click += btnAnalyseGroups_Click;

            pageFamilyGroups = new TabPage();
            pageFamilyGroups.Text = "pageFamilyGroups";
            pageFamilyGroups.Content = new DefTableLayout {
                Rows = {
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { tvGroups }
                    },
                    new TableRow {
                        ScaleHeight = false,
                        Cells = { TableLayout.Horizontal(10, btnAnalyseGroups, new TableCell(gkLogChart1, true)) }
                    }
                }
            };

            //

            tabsTools = new TabControl();
            tabsTools.Pages.Add(pageFamilyGroups);
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
