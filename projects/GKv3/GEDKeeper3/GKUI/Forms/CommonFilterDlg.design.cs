using Eto.Drawing;
using Eto.Forms;
using GKUI.Components;

namespace GKUI.Forms
{
    partial class CommonFilterDlg
    {
        protected TabControl tabsFilters;

        protected Button btnAccept;
        protected Button btnCancel;
        protected TabPage tsFieldsFilter;
        protected Button btnReset;

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

            btnCancel = new Button();
            btnCancel.ImagePosition = ButtonImagePosition.Left;
            btnCancel.Size = new Size(130, 26);
            btnCancel.Text = "btnCancel";
            btnCancel.Click += CancelClickHandler;

            tsFieldsFilter = new TabPage();
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
