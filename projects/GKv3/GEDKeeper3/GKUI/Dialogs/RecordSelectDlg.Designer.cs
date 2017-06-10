using System;
using Eto.Drawing;
using Eto.Forms;
using GKUI.Components;

namespace GKUI.Dialogs
{
    partial class RecordSelectDlg
    {
        private Button btnSelect;
        private Button btnCreate;
        private Button btnCancel;
        private Panel panList;
        public TextBox txtFastFilter;

        private void InitializeComponent()
        {
            SuspendLayout();

            btnSelect = new Button();
            btnSelect.ImagePosition = ButtonImagePosition.Left;
            btnSelect.Size = new Size(112, 32);
            btnSelect.Text = "btnSelect";
            btnSelect.Click += btnSelect_Click;

            btnCreate = new Button();
            btnCreate.Size = new Size(112, 32);
            btnCreate.Text = "btnCreate";
            btnCreate.Click += btnCreate_Click;

            btnCancel = new Button();
            btnCancel.ImagePosition = ButtonImagePosition.Left;
            btnCancel.Size = new Size(112, 32);
            btnCancel.Text = "btnCancel";

            txtFastFilter = new TextBox();
            txtFastFilter.TextChanged += txtFastFilter_TextChanged;

            panList = new Panel();

            Content = new TableLayout {
                Padding = new Padding(10),
                Spacing = new Size(10, 10),
                Rows = {
                    new TableRow {
                        Cells = { txtFastFilter }
                    },
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { panList }
                    },
                    new TableRow {
                        Cells = { null, btnCreate, btnSelect, btnCancel }
                    }
                }
            };

            AbortButton = btnCancel;
            ClientSize = new Size(540, 511);
            Title = "RecordSelectDlg";

            UIHelper.SetControlFont(this, "Tahoma", 8.25f);
            ResumeLayout();
        }
    }
}
