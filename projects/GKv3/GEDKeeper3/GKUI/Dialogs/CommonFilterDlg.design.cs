using System;
using Eto.Drawing;
using Eto.Forms;
using GKUI.Components;

namespace GKUI.Dialogs
{
    partial class CommonFilterDlg
    {
        private Button btnAccept;
        private Button btnCancel;
        protected TabControl tabsFilters;
        private TabPage tsFieldsFilter;
        private Button btnReset;
        private GridView dataGridView1;

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

            dataGridView1 = new GridView();
            //dataGridView1.AllowUserToResizeRows = false;
            //dataGridView1.ColumnHeadersHeightSizeMode = DataGridViewColumnHeadersHeightSizeMode.AutoSize;
            //dataGridView1.MultiSelect = false;
            dataGridView1.Size = new Size(819, 464);

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
            ClientSize = new Size(859, 588);
            Title = "CommonFilterDlg";

            UIHelper.SetControlFont(this, "Tahoma", 8.25f);
            ResumeLayout();
        }
    }
}
