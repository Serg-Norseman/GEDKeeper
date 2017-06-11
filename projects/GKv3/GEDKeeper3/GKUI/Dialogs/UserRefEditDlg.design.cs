using System;
using Eto.Drawing;
using Eto.Forms;
using GKUI.Components;

namespace GKUI.Dialogs
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
            btnAccept.Text = "btnAccept";
            btnAccept.Click += btnAccept_Click;

            btnCancel = new Button();
            btnCancel.ImagePosition = ButtonImagePosition.Left;
            btnCancel.Text = "btnCancel";

            lblReference = new Label();
            lblReference.Text = "lblReference";

            cmbRef = new ComboBox();

            lblRefType = new Label();
            lblRefType.Text = "lblRefType";

            cmbRefType = new ComboBox();

            Content = new TableLayout {
                Padding = new Padding(10),
                Spacing = new Size(10, 10),
                Rows = {
                    new TableRow {
                        Cells = { lblReference }
                    },
                    new TableRow {
                        Cells = { cmbRef }
                    },
                    new TableRow {
                        Cells = { lblRefType }
                    },
                    new TableRow {
                        Cells = { cmbRefType }
                    },
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { null }
                    },
                    new TableRow {
                        Cells = { null, btnAccept, btnCancel }
                    }
                }
            };

            DefaultButton = btnAccept;
            AbortButton = btnCancel;
            ClientSize = new Size(495, 179);
            Title = "UserRefEditDlg";

            UIHelper.SetControlFont(this, "Tahoma", 8.25f);
            ResumeLayout();
        }
    }
}
