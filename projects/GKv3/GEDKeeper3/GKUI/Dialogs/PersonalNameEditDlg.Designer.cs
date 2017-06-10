using System;
using Eto.Drawing;
using Eto.Forms;

namespace GKUI.Dialogs
{
    partial class PersonalNameEditDlg
    {
        private TextBox txtMarriedSurname;
        private Label lblMarriedSurname;
        private ComboBox cmbNameType;
        private Label lblType;
        private TextBox txtNickname;
        private TextBox txtNameSuffix;
        private TextBox txtNamePrefix;
        private TextBox txtSurnamePrefix;
        private TextBox txtPatronymic;
        private TextBox txtName;
        private TextBox txtSurname;
        private Label lblNickname;
        private Label lblNameSuffix;
        private Label lblNamePrefix;
        private Label lblSurnamePrefix;
        private Button btnCancel;
        private Label lblPatronymic;
        private Label lblName;
        private Button btnAccept;
        private Label lblSurname;

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            lblSurname = new Label();
            btnAccept = new Button();
            lblName = new Label();
            lblPatronymic = new Label();
            btnCancel = new Button();
            lblSurnamePrefix = new Label();
            lblNamePrefix = new Label();
            lblNameSuffix = new Label();
            lblNickname = new Label();
            txtSurname = new TextBox();
            txtName = new TextBox();
            txtPatronymic = new TextBox();
            txtSurnamePrefix = new TextBox();
            txtNamePrefix = new TextBox();
            txtNameSuffix = new TextBox();
            txtNickname = new TextBox();
            lblType = new Label();
            cmbNameType = new ComboBox();
            lblMarriedSurname = new Label();
            txtMarriedSurname = new TextBox();
            SuspendLayout();

            lblSurname.Location = new Point(12, 9);
            lblSurname.Size = new Size(75, 17);
            lblSurname.Text = "lblSurname";

            btnAccept.ImagePosition = ButtonImagePosition.Left;
            btnAccept.Location = new Point(195, 282);
            btnAccept.Size = new Size(114, 30);
            btnAccept.Text = "btnAccept";
            btnAccept.Click += btnAccept_Click;

            lblName.Location = new Point(12, 108);
            lblName.Size = new Size(55, 17);
            lblName.Text = "lblName";

            lblPatronymic.Location = new Point(12, 155);
            lblPatronymic.Size = new Size(90, 17);
            lblPatronymic.Text = "lblPatronymic";

            btnCancel.ImagePosition = ButtonImagePosition.Left;
            btnCancel.Location = new Point(314, 282);
            btnCancel.Size = new Size(114, 30);
            btnCancel.Text = "btnCancel";

            lblSurnamePrefix.Location = new Point(259, 9);
            lblSurnamePrefix.Size = new Size(109, 17);
            lblSurnamePrefix.Text = "lblSurnamePrefix";

            lblNamePrefix.Location = new Point(259, 58);
            lblNamePrefix.Size = new Size(89, 17);
            lblNamePrefix.Text = "lblNamePrefix";

            lblNameSuffix.Location = new Point(259, 108);
            lblNameSuffix.Size = new Size(89, 17);
            lblNameSuffix.Text = "lblNameSuffix";

            lblNickname.Location = new Point(259, 155);
            lblNickname.Size = new Size(79, 17);
            lblNickname.Text = "lblNickname";

            txtSurname.Location = new Point(12, 29);
            txtSurname.Size = new Size(226, 24);

            txtName.Location = new Point(12, 126);
            txtName.Size = new Size(226, 24);

            txtPatronymic.Location = new Point(12, 175);
            txtPatronymic.Size = new Size(226, 24);

            txtSurnamePrefix.Location = new Point(259, 29);
            txtSurnamePrefix.Size = new Size(169, 24);

            txtNamePrefix.Location = new Point(259, 78);
            txtNamePrefix.Size = new Size(169, 24);

            txtNameSuffix.Location = new Point(259, 126);
            txtNameSuffix.Size = new Size(169, 24);

            txtNickname.Location = new Point(259, 175);
            txtNickname.Size = new Size(169, 24);

            lblType.Location = new Point(12, 210);
            lblType.Size = new Size(51, 17);
            lblType.Text = "lblType";

            cmbNameType.ReadOnly = true;
            cmbNameType.Location = new Point(12, 229);
            cmbNameType.Size = new Size(225, 25);

            lblMarriedSurname.Location = new Point(12, 58);
            lblMarriedSurname.Size = new Size(119, 17);
            lblMarriedSurname.Text = "lblMarriedSurname";

            txtMarriedSurname.Location = new Point(12, 78);
            txtMarriedSurname.Size = new Size(226, 24);

            DefaultButton = btnAccept;
            AbortButton = btnCancel;
            ClientSize = new Size(444, 323);
            Controls.Add(lblMarriedSurname);
            Controls.Add(txtMarriedSurname);
            Controls.Add(lblType);
            Controls.Add(cmbNameType);
            Controls.Add(lblSurname);
            Controls.Add(btnAccept);
            Controls.Add(lblName);
            Controls.Add(lblPatronymic);
            Controls.Add(btnCancel);
            Controls.Add(lblSurnamePrefix);
            Controls.Add(lblNamePrefix);
            Controls.Add(lblNameSuffix);
            Controls.Add(lblNickname);
            Controls.Add(txtSurname);
            Controls.Add(txtName);
            Controls.Add(txtPatronymic);
            Controls.Add(txtSurnamePrefix);
            Controls.Add(txtNamePrefix);
            Controls.Add(txtNameSuffix);
            Controls.Add(txtNickname);
            //Font = new Font("Tahoma", 8.25F, FontStyle.None);
            Maximizable = false;
            Minimizable = false;
            Title = "PersonalNameEditDlg";
            ResumeLayout();
        }
    }
}
