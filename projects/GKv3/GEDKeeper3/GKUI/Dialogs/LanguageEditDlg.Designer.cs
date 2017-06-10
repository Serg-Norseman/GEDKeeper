using System;
using Eto.Drawing;
using Eto.Forms;
using GKUI.Components;

namespace GKUI.Dialogs
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
            btnAccept.Size = new Size(80, 26);
            btnAccept.Text = "btnAccept";
            btnAccept.Click += btnAccept_Click;

            btnCancel = new Button();
            btnCancel.ImagePosition = ButtonImagePosition.Left;
            btnCancel.Size = new Size(80, 26);
            btnCancel.Text = "btnCancel";

            lblLanguage = new Label();
            lblLanguage.Text = "lblLanguage";

            cmbLanguage = new ComboBox();
            cmbLanguage.ReadOnly = true;
            //cmbLanguage.Sorted = true;

            Content = new TableLayout {
                Padding = new Padding(10),
                Spacing = new Size(10, 10),
                Rows = {
                    new TableRow {
                        Cells = { lblLanguage }
                    },
                    new TableRow {
                        Cells = { cmbLanguage }
                    },
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { null }
                    },
                    new TableRow {
                        Cells = { null, btnAccept, btnCancel }
                    }
                }
            };

            DefaultButton = btnAccept;
            AbortButton = btnCancel;
            ClientSize = new Size(394, 116);
            Title = "LanguageEditDlg";

            UIHelper.SetControlFont(this, "Tahoma", 8.25f);
            ResumeLayout();
        }
    }
}
