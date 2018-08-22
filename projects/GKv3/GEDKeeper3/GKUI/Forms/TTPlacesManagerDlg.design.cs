using Eto.Drawing;
using Eto.Forms;
using GKUI.Components;

namespace GKUI.Forms
{
    partial class TTPlacesManagerDlg
    {
        private TabControl tabsTools;
        private Button btnClose;
        private TabPage pagePlaceManage;
        private Panel panPlacesContainer;
        private Button btnAnalysePlaces;
        private Button btnIntoList;

        private void InitializeComponent()
        {
            SuspendLayout();

            panPlacesContainer = new Panel();
            panPlacesContainer.Size = new Size(800, 360);

            btnAnalysePlaces = new Button();
            btnAnalysePlaces.Size = new Size(130, 26);
            btnAnalysePlaces.Text = "btnSkip";
            btnAnalysePlaces.Click += btnAnalysePlaces_Click;

            btnIntoList = new Button();
            btnIntoList.Size = new Size(160, 26);
            btnIntoList.Text = "btnIntoList";
            btnIntoList.Click += btnIntoList_Click;

            pagePlaceManage = new TabPage();
            pagePlaceManage.Text = "pagePlaceManage";
            pagePlaceManage.Content = new DefTableLayout {
                Rows = {
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { panPlacesContainer }
                    },
                    UIHelper.MakeDialogFooter(btnAnalysePlaces, null, btnIntoList)
                }
            };

            //

            tabsTools = new TabControl();
            tabsTools.Pages.Add(pagePlaceManage);
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
