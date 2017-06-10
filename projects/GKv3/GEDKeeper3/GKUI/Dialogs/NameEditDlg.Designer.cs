using System;
using Eto.Drawing;
using Eto.Forms;

namespace GKUI.Dialogs
{
    partial class NameEditDlg
    {
        private Label lblName;
        private TextBox txtName;
        private Label lblSex;
        private ComboBox cmbSex;
        private Button btnAccept;
        private Button btnCancel;
        private GroupBox grpPatronymics;
        private Label lblFemale;
        private TextBox txtFPatr;
        private Label lblMale;
        private TextBox txtMPatr;

        private void InitializeComponent()
        {
            lblName = new Label();
            txtName = new TextBox();
            lblSex = new Label();
            cmbSex = new ComboBox();
            btnAccept = new Button();
            btnCancel = new Button();
            grpPatronymics = new GroupBox();
            lblFemale = new Label();
            lblMale = new Label();
            txtFPatr = new TextBox();
            txtMPatr = new TextBox();
            grpPatronymics.SuspendLayout();
            SuspendLayout();

            lblName.Location = new Point(12, 13);
            lblName.Size = new Size(33, 17);
            lblName.Text = "lblName";

            txtName.Location = new Point(101, 10);
            txtName.Size = new Size(270, 24);
            txtName.KeyPress += edName_KeyPress;

            lblSex.Location = new Point(12, 52);
            lblSex.Size = new Size(33, 17);
            lblSex.Text = "lblSex";

            cmbSex.ReadOnly = true;
            cmbSex.Location = new Point(101, 49);
            cmbSex.Size = new Size(169, 25);

            btnAccept.ImagePosition = ButtonImagePosition.Left;
            btnAccept.Location = new Point(134, 204);
            btnAccept.Size = new Size(114, 30);
            btnAccept.Text = "btnAccept";
            btnAccept.Click += btnAccept_Click;

            btnCancel.ImagePosition = ButtonImagePosition.Left;
            btnCancel.Location = new Point(258, 204);
            btnCancel.Size = new Size(113, 30);
            btnCancel.Text = "btnCancel";

            grpPatronymics.Controls.Add(lblFemale);
            grpPatronymics.Controls.Add(lblMale);
            grpPatronymics.Controls.Add(txtFPatr);
            grpPatronymics.Controls.Add(txtMPatr);
            grpPatronymics.Location = new Point(11, 87);
            grpPatronymics.Size = new Size(360, 95);
            grpPatronymics.Text = "grpPatronymics";

            lblFemale.Location = new Point(11, 22);
            lblFemale.Size = new Size(66, 17);
            lblFemale.Text = "lblFemale";

            lblMale.Location = new Point(11, 61);
            lblMale.Size = new Size(65, 17);
            lblMale.Text = "lblMale";

            txtFPatr.Location = new Point(90, 19);
            txtFPatr.Size = new Size(259, 24);
            txtFPatr.KeyPress += edName_KeyPress;

            txtMPatr.Location = new Point(90, 58);
            txtMPatr.Size = new Size(259, 24);
            txtMPatr.KeyPress += edName_KeyPress;

            DefaultButton = btnAccept;
            AbortButton = btnCancel;
            ClientSize = new Size(385, 250);
            Controls.Add(lblName);
            Controls.Add(txtName);
            Controls.Add(lblSex);
            Controls.Add(cmbSex);
            Controls.Add(btnAccept);
            Controls.Add(btnCancel);
            Controls.Add(grpPatronymics);
            //Font = new Font("Tahoma", 8.25F, FontStyle.None);
            Maximizable = false;
            Minimizable = false;
            ShowInTaskbar = false;
            Title = "NameEditDlg";
            grpPatronymics.ResumeLayout();
            ResumeLayout();
        }
    }
}
