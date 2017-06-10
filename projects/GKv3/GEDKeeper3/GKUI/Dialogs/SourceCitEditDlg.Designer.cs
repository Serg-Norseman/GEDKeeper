using System;
using Eto.Drawing;
using Eto.Forms;

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
            btnAccept = new Button();
            btnCancel = new Button();
            lblPage = new Label();
            txtPage = new TextBox();
            lblSource = new Label();
            btnSourceAdd = new Button();
            lblCertainty = new Label();
            txtCertainty = new ComboBox();
            cmbSource = new ComboBox();
            SuspendLayout();

            btnAccept.ImagePosition = ButtonImagePosition.Left;
            btnAccept.Location = new Point(216, 204);
            btnAccept.Size = new Size(101, 31);
            btnAccept.Text = "btnAccept";
            btnAccept.Click += btnAccept_Click;

            btnCancel.ImagePosition = ButtonImagePosition.Left;
            btnCancel.Location = new Point(325, 204);
            btnCancel.Size = new Size(101, 31);
            btnCancel.Text = "btnCancel";

            lblPage.Location = new Point(15, 70);
            lblPage.Size = new Size(50, 17);
            lblPage.Text = "lblPage";

            txtPage.Location = new Point(15, 90);
            txtPage.Size = new Size(410, 24);

            lblSource.Location = new Point(15, 10);
            lblSource.Size = new Size(63, 17);
            lblSource.Text = "lblSource";

            btnSourceAdd.Location = new Point(391, 24);
            btnSourceAdd.Size = new Size(35, 35);
            btnSourceAdd.Click += btnSourceAdd_Click;

            lblCertainty.Location = new Point(15, 130);
            lblCertainty.Size = new Size(76, 17);
            lblCertainty.Text = "lblCertainty";

            txtCertainty.ReadOnly = true;
            txtCertainty.Location = new Point(15, 150);
            txtCertainty.Size = new Size(410, 25);

            cmbSource.Location = new Point(15, 30);
            cmbSource.Size = new Size(368, 25);
            cmbSource.Sorted = true;
            cmbSource.KeyDown += cbSource_KeyDown;
            cmbSource.KeyUp += cbSource_KeyUp;

            DefaultButton = btnAccept;
            AbortButton = btnCancel;
            ClientSize = new Size(441, 250);
            Controls.Add(btnAccept);
            Controls.Add(btnCancel);
            Controls.Add(lblPage);
            Controls.Add(txtPage);
            Controls.Add(lblSource);
            Controls.Add(btnSourceAdd);
            Controls.Add(lblCertainty);
            Controls.Add(txtCertainty);
            Controls.Add(cmbSource);
            //Font = new Font("Tahoma", 8.25F, FontStyle.None);
            Maximizable = false;
            Minimizable = false;
            ShowInTaskbar = false;
            Title = "SourceCitEditDlg";
            ResumeLayout();
        }
    }
}
