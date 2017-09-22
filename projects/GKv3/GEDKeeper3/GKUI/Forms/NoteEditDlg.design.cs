using Eto.Drawing;
using Eto.Forms;
using GKUI.Components;

namespace GKUI.Forms
{
    partial class NoteEditDlg
    {
        private Button btnAccept;
        private Button btnCancel;
        private TextArea txtNote;

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

            txtNote = new TextArea();
            txtNote.AcceptsReturn = true;
            txtNote.Height = 190;
            txtNote.Width = 380;

            Content = new DefTableLayout {
                Rows = {
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { txtNote }
                    },
                    UIHelper.MakeDialogFooter(null, btnAccept, btnCancel)
                }
            };

            DefaultButton = btnAccept;
            AbortButton = btnCancel;
            Title = "NoteEditDlg";

            SetPredefProperties(400, 250);
            ResumeLayout();
        }
    }
}
