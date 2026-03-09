#pragma warning disable IDE1006 // Naming Styles

using Terminal.Gui;

namespace GKUI.Forms
{
    partial class AgeEditDlg
    {
        private Button btnAccept;
        private Button btnCancel;
        private FrameView grpBox;
        private ComboBox cmbRel1;
        private ComboBox cmbRel2;
        private Label lblAge1;
        private TextValidateField txtVal1;
        private Label lblAge2;
        private TextValidateField txtVal2;

        private void InitializeComponent()
        {
            btnAccept = new Button();
            btnCancel = new Button();
            grpBox = new FrameView();
            lblAge1 = new Label();
            cmbRel1 = new ComboBox();
            txtVal1 = new TextValidateField();
            lblAge2 = new Label();
            cmbRel2 = new ComboBox();
            txtVal2 = new TextValidateField();

            grpBox.Add(lblAge1);
            grpBox.Add(lblAge2);
            grpBox.Add(cmbRel1);
            grpBox.Add(cmbRel2);
            grpBox.Add(txtVal1);
            grpBox.Add(txtVal2);

            grpBox.Location = new Point(0, 0);
            grpBox.Size = new Size(58, 7);
            grpBox.TabIndex = 4;
            grpBox.TabStop = false;

            lblAge1.Location = new Point(1, 1);
            lblAge1.TabIndex = 0;

            lblAge2.Location = new Point(1, 3);
            lblAge2.TabIndex = 2;

            cmbRel1.Location = new Point(18, 1);
            cmbRel1.Size = new Size(6, 1);
            cmbRel1.TabIndex = 3;

            cmbRel2.Location = new Point(18, 3);
            cmbRel2.Size = new Size(6, 1);
            cmbRel2.TabIndex = 3;

            txtVal1.Location = new Point(26, 1);
            txtVal1.Size = new Size(30, 1);
            txtVal1.TabIndex = 1;

            txtVal2.Location = new Point(26, 3);
            txtVal2.Size = new Size(30, 1);
            txtVal2.TabIndex = 3;

            btnAccept.Size = new Size(16, 1);
            btnAccept.TabIndex = 5;
            btnAccept.Clicked += AcceptClickHandler;

            btnCancel.Size = new Size(16, 1);
            btnCancel.TabIndex = 6;
            btnCancel.Clicked += CancelClickHandler;

            Size = new Size(60, 10);
            Add(grpBox);
            AddButton(btnAccept);
            AddButton(btnCancel);
        }
    }
}
