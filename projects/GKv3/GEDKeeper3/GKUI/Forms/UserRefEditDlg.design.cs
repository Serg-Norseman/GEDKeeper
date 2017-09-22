using Eto.Drawing;
using Eto.Forms;
using GKUI.Components;

namespace GKUI.Forms
{
    partial class UserRefEditDlg
    {
        private Button btnAccept;
        private Button btnCancel;
        private Label lblReference;
        private ComboBox cmbRef;
        private Label lblRefType;
        private ComboBox cmbRefType;

        private void InitializeComponent()
        {
            SuspendLayout();

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

            lblReference = new Label();
            lblReference.Text = "lblReference";

            cmbRef = new ComboBox();
            cmbRef.Width = 200;

            lblRefType = new Label();
            lblRefType.Text = "lblRefType";

            cmbRefType = new ComboBox();

            var panelData = new TableLayout {
                Spacing = new Size(10, 10),
                Rows = {
                    new TableRow {
                        Cells = { lblReference, cmbRef }
                    },
                    new TableRow {
                        Cells = { lblRefType, cmbRefType }
                    },
                    null
                }
            };

            Content = new DefTableLayout {
                Rows = {
                    new TableRow {
                        Cells = { panelData }
                    },
                    UIHelper.MakeDialogFooter(null, btnAccept, btnCancel)
                }
            };

            DefaultButton = btnAccept;
            AbortButton = btnCancel;
            Title = "UserRefEditDlg";

            SetPredefProperties(500, 180);
            ResumeLayout();
        }
    }
}
