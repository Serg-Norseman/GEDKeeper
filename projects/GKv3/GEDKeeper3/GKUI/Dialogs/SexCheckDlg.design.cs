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

            rbMale = new RadioButton(rbNone);
            rbMale.Text = "rbMale";

            rbFemale = new RadioButton(rbNone);
            rbFemale.Text = "rbFemale";

            grpSex = new GroupBox();
            grpSex.Text = "grpSex";
            grpSex.Content = new HDefStackLayout {
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

            Content = new DefTableLayout {
                Rows = {
                    new TableRow {
                        Cells = { txtName }
                    },
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { grpSex }
                    },
                    UIHelper.MakeDialogFooter(null, btnAccept, btnCancel)
                }
            };

            DefaultButton = btnAccept;
            AbortButton = btnCancel;
            Title = "SexCheckDlg";
            Topmost = true;

            SetPredefProperties(500, 160);
            ResumeLayout();
        }
    }
}
