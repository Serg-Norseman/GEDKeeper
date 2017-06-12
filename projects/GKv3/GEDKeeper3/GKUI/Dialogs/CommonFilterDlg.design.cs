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
            btnReset = new Button();
            btnAccept = new Button();
            btnCancel = new Button();
            tabsFilters = new TabControl();
            tsFieldsFilter = new TabPage();
            dataGridView1 = new GridView();

            SuspendLayout();

            btnReset.Size = new Size(114, 30);
            btnReset.Text = "btnReset";
            btnReset.Click += btnReset_Click;

            btnAccept.ImagePosition = ButtonImagePosition.Left;
            btnAccept.Size = new Size(112, 30);
            btnAccept.Text = "btnAccept";
            btnAccept.Click += btnAccept_Click;

            btnCancel.ImagePosition = ButtonImagePosition.Left;
            btnCancel.Size = new Size(114, 30);
            btnCancel.Text = "btnCancel";

            tabsFilters.Pages.Add(tsFieldsFilter);
            tabsFilters.SelectedIndex = 0;
            tabsFilters.Size = new Size(827, 494);

            tsFieldsFilter.Content = dataGridView1;
            tsFieldsFilter.Size = new Size(819, 464);
            tsFieldsFilter.Text = "tsFieldsFilter";

            //dataGridView1.AllowUserToResizeRows = false;
            //dataGridView1.ColumnHeadersHeightSizeMode = DataGridViewColumnHeadersHeightSizeMode.AutoSize;
            //dataGridView1.MultiSelect = false;
            dataGridView1.Size = new Size(819, 464);

            Content = new TableLayout {
                Padding = new Padding(10),
                Spacing = new Size(10, 10),
                Rows = {
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { tabsFilters }
                    },
                    new TableRow {
                        ScaleHeight = false,
                        Cells = { btnReset, null, btnAccept, btnCancel }
                    }
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
