using Eto.Drawing;
using Eto.Forms;
using GKUI.Components;

namespace GKUI.Forms
{
    partial class SourceCitEditDlg
    {
        private Button btnAccept;
        private Button btnCancel;
        private Label lblPage;
        private TextBox txtPage;
        private Label lblSource;
        private Button btnSourceAdd;
        private Label lblCertainty;
        private ComboBox txtCertainty;
        private ComboBox cmbSource;
        private TabControl tabsData;
        private TabPage pageCommon;
        private TabPage pageOther;
        private TextBox txtText;
        private GKDateControl dateCtl;

        private void InitializeComponent()
        {
            SuspendLayout();

            lblPage = new Label();
            lblPage.Text = "lblPage";

            txtPage = new TextBox();

            lblSource = new Label();
            lblSource.Text = "lblSource";

            btnSourceAdd = new Button();
            btnSourceAdd.Size = new Size(26, 26);
            btnSourceAdd.Click += btnSourceAdd_Click;

            lblCertainty = new Label();
            lblCertainty.Text = "lblCertainty";

            txtCertainty = new ComboBox();
            txtCertainty.ReadOnly = true;

            cmbSource = new ComboBox();
            cmbSource.KeyDown += cbSource_KeyDown;
            cmbSource.KeyUp += cbSource_KeyUp;
            cmbSource.Width = 400;

            btnAccept = new Button();
            btnAccept.ImagePosition = ButtonImagePosition.Left;
            btnAccept.Size = new Size(130, 26);
            btnAccept.Text = "btnAccept";
            btnAccept.Click += btnAccept_Click;

            btnCancel = new Button();
            btnCancel.ImagePosition = ButtonImagePosition.Left;
            btnCancel.Size = new Size(130, 26);
            btnCancel.Text = "btnCancel";
            btnCancel.Click += btnCancel_Click;

            pageCommon = new TabPage();
            pageCommon.Text = "pageCommon";
            pageCommon.Content = new TableLayout {
                Spacing = new Size(10, 10),
                Rows = {
                    new TableRow {
                        Cells = { lblSource, TableLayout.Horizontal(10, new TableCell(cmbSource, true), btnSourceAdd) }
                        //Cells = { lblSource, UIHelper.CreateRCS(new StackLayoutItem(cmbSource, true), btnSourceAdd) }
                    },
                    new TableRow {
                        Cells = { lblPage, TableLayout.Horizontal(10, txtPage, lblCertainty, txtCertainty) }
                    },
                    null
                }
            };

            txtText = new TextBox();

            dateCtl = new GKDateControl();

            pageOther = new TabPage();
            pageOther.Text = "pageOther";
            pageOther.Content = new DefTableLayout {
                Rows = {
                    new TableRow {
                        Cells = { dateCtl }
                    },
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { txtText }
                    }
                }
            };

            tabsData = new TabControl();
            tabsData.Pages.Add(pageCommon);
            tabsData.Pages.Add(pageOther);
            tabsData.Size = new Size(440, 200);

            Content = new DefTableLayout {
                Rows = {
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { tabsData }
                    },
                    UIHelper.MakeDialogFooter(null, btnAccept, btnCancel)
                }
            };

            DefaultButton = btnAccept;
            AbortButton = btnCancel;
            Title = "SourceCitEditDlg";

            SetPredefProperties(440, 250);
            ResumeLayout();
        }
    }
}
