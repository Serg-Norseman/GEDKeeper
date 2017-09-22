using Eto.Drawing;
using Eto.Forms;
using GKUI.Components;

namespace GKUI.Forms
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
            btnSelect.Size = new Size(130, 26);
            btnSelect.Text = "btnSelect";
            btnSelect.Click += btnSelect_Click;
            btnSelect.Image = Bitmap.FromResource("Resources.btn_accept.gif");

            btnCreate = new Button();
            btnCreate.Size = new Size(130, 26);
            btnCreate.Text = "btnCreate";
            btnCreate.Click += btnCreate_Click;

            btnCancel = new Button();
            btnCancel.ImagePosition = ButtonImagePosition.Left;
            btnCancel.Size = new Size(130, 26);
            btnCancel.Text = "btnCancel";
            btnCancel.Click += CancelClickHandler;
            btnCancel.Image = Bitmap.FromResource("Resources.btn_cancel.gif");

            txtFastFilter = new TextBox();
            txtFastFilter.TextChanged += txtFastFilter_TextChanged;

            panList = new Panel();
            panList.Size = new Size(540, 400);

            Content = new DefTableLayout {
                Rows = {
                    new TableRow {
                        Cells = { txtFastFilter }
                    },
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { panList }
                    },
                    UIHelper.MakeDialogFooter(null, btnCreate, btnSelect, btnCancel)
                }
            };

            AbortButton = btnCancel;
            Title = "RecordSelectDlg";

            SetPredefProperties(560, 510);
            ResumeLayout();
        }
    }
}
