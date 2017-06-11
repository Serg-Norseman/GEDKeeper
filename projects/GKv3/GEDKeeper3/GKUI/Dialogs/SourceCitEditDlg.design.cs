using System;
using Eto.Drawing;
using Eto.Forms;
using GKUI.Components;

namespace GKUI.Dialogs
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

        private void InitializeComponent()
        {
            SuspendLayout();

            btnAccept = new Button();
            btnAccept.ImagePosition = ButtonImagePosition.Left;
            btnAccept.Size = new Size(80, 26);
            btnAccept.Text = "btnAccept";
            btnAccept.Click += btnAccept_Click;

            btnCancel = new Button();
            btnCancel.ImagePosition = ButtonImagePosition.Left;
            btnCancel.Size = new Size(80, 26);
            btnCancel.Text = "btnCancel";

            lblPage = new Label();
            lblPage.Text = "lblPage";

            txtPage = new TextBox();

            lblSource = new Label();
            lblSource.Text = "lblSource";

            btnSourceAdd = new Button();
            btnSourceAdd.Size = new Size(35, 35);
            btnSourceAdd.Click += btnSourceAdd_Click;

            lblCertainty = new Label();
            lblCertainty.Text = "lblCertainty";

            txtCertainty = new ComboBox();
            txtCertainty.ReadOnly = true;

            cmbSource = new ComboBox();
            //cmbSource.Sorted = true;
            cmbSource.KeyDown += cbSource_KeyDown;
            cmbSource.KeyUp += cbSource_KeyUp;

            DefaultButton = btnAccept;
            AbortButton = btnCancel;
            ClientSize = new Size(441, 250);
            Title = "SourceCitEditDlg";

            Content = new TableLayout {
                Padding = new Padding(10),
                Spacing = new Size(10, 10),
                Rows = {
                    new TableRow {
                        Cells = { lblSource }
                    },
                    new TableRow {
                        Cells = { cmbSource, btnSourceAdd }
                    },
                    new TableRow {
                        Cells = { lblPage }
                    },
                    new TableRow {
                        Cells = { txtPage }
                    },
                    new TableRow {
                        Cells = { lblCertainty }
                    },
                    new TableRow {
                        Cells = { txtCertainty }
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

            UIHelper.SetControlFont(this, "Tahoma", 8.25f);
            ResumeLayout();
        }
    }
}
