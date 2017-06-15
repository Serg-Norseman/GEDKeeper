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

            lblPage = new Label();
            lblPage.Text = "lblPage";

            txtPage = new TextBox();

            lblSource = new Label();
            lblSource.Text = "lblSource";

            btnSourceAdd = new Button();
            btnSourceAdd.Size = new Size(26, 26);
            btnSourceAdd.Click += btnSourceAdd_Click;
            btnSourceAdd.Image = Bitmap.FromResource("Resources.btn_rec_new.gif");

            lblCertainty = new Label();
            lblCertainty.Text = "lblCertainty";

            txtCertainty = new ComboBox();
            txtCertainty.ReadOnly = true;

            cmbSource = new ComboBox();
            //cmbSource.Sorted = true;
            cmbSource.KeyDown += cbSource_KeyDown;
            cmbSource.KeyUp += cbSource_KeyUp;

            Content = new DefTableLayout {
                Rows = {
                    new TableRow {
                        Cells = { new DefTableLayout {
                                Rows = {
                                    new TableRow {
                                        Cells = { lblSource, TableLayout.Horizontal(10, new TableCell(cmbSource, true), btnSourceAdd) }
                                    },
                                    new TableRow {
                                        Cells = { lblPage, txtPage }
                                    },
                                    new TableRow {
                                        Cells = { lblCertainty, txtCertainty }
                                    }
                                } } }
                    },
                    null,
                    UIHelper.MakeDialogFooter(null, btnAccept, btnCancel)
                }
            };

            DefaultButton = btnAccept;
            AbortButton = btnCancel;
            ClientSize = new Size(441, 250);
            Title = "SourceCitEditDlg";

            UIHelper.SetControlFont(this, "Tahoma", 8.25f);
            ResumeLayout();
        }
    }
}
