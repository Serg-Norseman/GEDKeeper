using System;
using Eto.Drawing;
using Eto.Forms;
using GKUI.Components;

namespace GKUI.Dialogs
{
    partial class SexCheckDlg
    {
        private TextBox txtName;
        private GroupBox grpSex;
        private RadioButton rbNone;
        private RadioButton rbMale;
        private RadioButton rbFemale;
        private Button btnAccept;
        private Button btnCancel;

        private void InitializeComponent()
        {
            SuspendLayout();

            txtName = new TextBox();
            txtName.ReadOnly = true;

            rbNone = new RadioButton();
            rbNone.Text = "?";

            rbMale = new RadioButton();
            rbMale.Text = "rbMale";

            rbFemale = new RadioButton();
            rbFemale.Text = "rbFemale";

            grpSex = new GroupBox();
            grpSex.Text = "grpSex";
            grpSex.Content = new StackLayout {
                Orientation = Orientation.Horizontal,
                Items = { rbNone, rbMale, rbFemale }
            };

            btnAccept = new Button();
            btnAccept.ImagePosition = ButtonImagePosition.Left;
            btnAccept.Size = new Size(80, 26);
            btnAccept.Text = "btnAccept";

            btnCancel = new Button();
            btnCancel.ImagePosition = ButtonImagePosition.Left;
            btnCancel.Size = new Size(80, 26);
            btnCancel.Text = "btnCancel";

            Content = new TableLayout {
                Padding = new Padding(10),
                Spacing = new Size(10, 10),
                Rows = {
                    new TableRow {
                        Cells = { txtName }
                    },
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { grpSex }
                    },
                    new TableRow {
                        Cells = { null, btnAccept, btnCancel }
                    }
                }
            };

            DefaultButton = btnAccept;
            AbortButton = btnCancel;
            ClientSize = new Size(504, 158);
            Title = "SexCheckDlg";
            Topmost = true;

            UIHelper.SetControlFont(this, "Tahoma", 8.25f);
            ResumeLayout();
        }
    }
}
