#pragma warning disable IDE1006 // Naming Styles

using Terminal.Gui;

namespace GKUI.Forms
{
    partial class SexCheckDlg
    {
        private TextField txtName;
        private FrameView grpSex;
        private RadioButton rbNone;
        private RadioButton rbMale;
        private RadioButton rbFemale;
        private Button btnAccept;
        private Button btnCancel;

        private void InitializeComponent()
        {
            txtName = new TextField();
            grpSex = new FrameView();
            rbNone = new RadioButton();
            rbMale = new RadioButton();
            rbFemale = new RadioButton();
            btnAccept = new Button();
            btnCancel = new Button();

            txtName.Location = new Point(1, 1);
            txtName.ReadOnly = true;
            txtName.Size = new Size(46, 1);
            txtName.TabIndex = 0;

            grpSex.Add(rbNone);
            grpSex.Add(rbMale);
            grpSex.Add(rbFemale);
            grpSex.Location = new Point(1, 3);
            grpSex.Size = new Size(46, 7);
            grpSex.TabIndex = 1;
            grpSex.TabStop = false;

            rbNone.Location = new Point(1, 1);
            rbNone.TabIndex = 0;
            rbNone.Group = "gs";

            rbMale.Location = new Point(1, 2);
            rbMale.TabIndex = 1;
            rbMale.Group = "gs";

            rbFemale.Location = new Point(1, 3);
            rbFemale.TabIndex = 2;
            rbFemale.Group = "gs";

            btnAccept.Size = new Size(16, 1);
            btnAccept.TabIndex = 2;
            btnAccept.Clicked += AcceptClickHandler;

            btnCancel.Size = new Size(16, 1);
            btnCancel.TabIndex = 3;
            btnCancel.Clicked += CancelClickHandler;

            Size = new Size(50, 15);
            Add(txtName);
            Add(grpSex);

            AddButton(btnAccept);
            AddButton(btnCancel);
        }
    }
}
