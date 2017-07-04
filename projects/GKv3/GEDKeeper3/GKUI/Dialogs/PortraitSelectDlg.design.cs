using System;
using Eto.Drawing;
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
            btnAccept = new Button();
            btnCancel = new Button();
            imageView1 = new GKUI.Components.ImageView();
            SuspendLayout();

            btnAccept.ImagePosition = ButtonImagePosition.Left;
            btnAccept.Text = "btnAccept";
            btnAccept.Click += btnAccept_Click;

            btnCancel.ImagePosition = ButtonImagePosition.Left;
            btnCancel.Text = "btnCancel";

            imageView1.SelectionMode = ImageBoxSelectionMode.Zoom;
            //imageView1.ShowToolbar = true;

            Content = new TableLayout {
                Padding = new Padding(10),
                Spacing = new Size(10, 10),
                Rows = {
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { imageView1 }
                    },
                    new TableRow {
                        Cells = { null, btnAccept, btnCancel }
                    }
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
