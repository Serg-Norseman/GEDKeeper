using Eto.Drawing;
using Eto.Forms;
using GKUI.Components;

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
            SuspendLayout();

            lblName = new Label();
            lblName.Text = "lblName";

            txtName = new TextBox();
            //txtName.KeyPress += edName_KeyPress;

            lblSex = new Label();
            lblSex.Text = "lblSex";

            cmbSex = new ComboBox();
            cmbSex.ReadOnly = true;

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

            lblFemale = new Label();
            lblFemale.Text = "lblFemale";

            lblMale = new Label();
            lblMale.Text = "lblMale";

            txtFPatr = new TextBox();
            txtFPatr.KeyDown += edName_KeyDown;

            txtMPatr = new TextBox();
            txtMPatr.KeyDown += edName_KeyDown;

            grpPatronymics = new GroupBox();
            grpPatronymics.Text = "grpPatronymics";
            grpPatronymics.Content = new DefTableLayout {
                Rows = {
                    new TableRow {
                        Cells = { lblFemale, txtFPatr }
                    },
                    new TableRow {
                        Cells = { lblMale, txtMPatr }
                    }
                }
            };

            Content = new DefTableLayout {
                Rows = {
                    new TableRow {
                        Cells = { lblName, txtName }
                    },
                    new TableRow {
                        Cells = { lblSex, cmbSex }
                    },
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { grpPatronymics }
                    },
                    UIHelper.MakeDialogFooter(null, btnAccept, btnCancel)
                }
            };

            DefaultButton = btnAccept;
            AbortButton = btnCancel;
            Title = "NameEditDlg";

            SetPredefProperties(380, 250);
            ResumeLayout();
        }
    }
}
