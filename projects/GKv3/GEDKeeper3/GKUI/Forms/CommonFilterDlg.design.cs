using Eto.Drawing;
using Eto.Forms;
using GKUI.Components;

namespace GKUI.Forms
{
    partial class CommonFilterDlg
    {
        protected TabControl tabsFilters;

        private Button btnAccept;
        private Button btnCancel;
        private TabPage tsFieldsFilter;
        private Button btnReset;
        private GKListView dataGridView1;

        private void InitializeComponent()
        {
            SuspendLayout();

            btnReset = new Button();
            btnReset.Size = new Size(130, 26);
            btnReset.Text = "btnReset";
            btnReset.Click += btnReset_Click;

            btnAccept = new Button();
            btnAccept.ImagePosition = ButtonImagePosition.Left;
            btnAccept.Size = new Size(130, 26);
            btnAccept.Text = "btnAccept";
            btnAccept.Click += btnAccept_Click;
            btnAccept.Image = Bitmap.FromResource("Resources.btn_accept.gif");

            btnCancel = new Button();
            btnCancel.ImagePosition = ButtonImagePosition.Left;
            btnCancel.Size = new Size(130, 26);
            btnCancel.Text = "btnCancel";
            btnCancel.Click += CancelClickHandler;
            btnCancel.Image = Bitmap.FromResource("Resources.btn_cancel.gif");

            dataGridView1 = new GKListView();
            dataGridView1.Height = 460;

            tsFieldsFilter = new TabPage();
            tsFieldsFilter.Content = dataGridView1;
            tsFieldsFilter.Text = "tsFieldsFilter";

            tabsFilters = new TabControl();
            tabsFilters.Pages.Add(tsFieldsFilter);

            Content = new DefTableLayout {
                Rows = {
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { tabsFilters }
                    },
                    UIHelper.MakeDialogFooter(btnReset, null, btnAccept, btnCancel)
                }
            };

            DefaultButton = btnAccept;
            AbortButton = btnCancel;
            Title = "CommonFilterDlg";

            SetPredefProperties(860, 580);
            ResumeLayout();
        }
    }
}
