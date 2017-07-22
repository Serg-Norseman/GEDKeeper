using Eto.Forms;
using GKUI.Components;

namespace GKUI.Dialogs
{
    partial class PortraitSelectDlg
    {
        private Button btnAccept;
        private Button btnCancel;
        private GKUI.Components.ImageView imageView1;

        private void InitializeComponent()
        {
            SuspendLayout();

            btnAccept = new Button();
            btnAccept.ImagePosition = ButtonImagePosition.Left;
            btnAccept.Text = "btnAccept";
            btnAccept.Click += btnAccept_Click;

            btnCancel = new Button();
            btnCancel.ImagePosition = ButtonImagePosition.Left;
            btnCancel.Text = "btnCancel";

            imageView1 = new GKUI.Components.ImageView();
            imageView1.SelectionMode = ImageBoxSelectionMode.Zoom;
            imageView1.ShowToolbar = true;

            Content = new DefTableLayout {
                Rows = {
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { imageView1 }
                    },
                    UIHelper.MakeDialogFooter(null, btnAccept, btnCancel)
                }
            };

            DefaultButton = btnAccept;
            AbortButton = btnCancel;
            Title = "PortraitSelectDlg";

            SetPredefProperties(870, 680);
            ResumeLayout();
        }
    }
}
