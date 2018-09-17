using Eto.Drawing;
using Eto.Forms;
using GKUI.Components;

namespace GKUI.Forms
{
    partial class LanguageEditDlg
    {
        private ComboBox cmbLanguage;
        private Label lblLanguage;
        private Button btnCancel;
        private Button btnAccept;

        private void InitializeComponent()
        {
            SuspendLayout();

            btnAccept = new Button();
            btnAccept.ImagePosition = ButtonImagePosition.Left;
            btnAccept.Size = new Size(130, 26);
            btnAccept.Text = "btnAccept";
            btnAccept.Click += btnAccept_Click;

            btnCancel = new Button();
            btnCancel.ImagePosition = ButtonImagePosition.Left;
            btnCancel.Size = new Size(130, 26);
            btnCancel.Text = "btnCancel";
            btnCancel.Click += CancelClickHandler;

            lblLanguage = new Label();
            lblLanguage.Text = "lblLanguage";

            cmbLanguage = new ComboBox();
            cmbLanguage.ReadOnly = true;

            Content = new DefTableLayout {
                Rows = {
                    new TableRow {
                        Cells = { lblLanguage }
                    },
                    new TableRow {
                        Cells = { cmbLanguage }
                    },
                    null,
                    UIHelper.MakeDialogFooter(null, btnAccept, btnCancel)
                }
            };

            DefaultButton = btnAccept;
            AbortButton = btnCancel;
            Title = "LanguageEditDlg";

            SetPredefProperties(400, 120);
            ResumeLayout();
        }
    }
}
