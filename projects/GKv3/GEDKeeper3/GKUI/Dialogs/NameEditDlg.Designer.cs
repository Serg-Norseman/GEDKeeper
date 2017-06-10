using System;
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
            btnAccept.Size = new Size(80, 26);
            btnAccept.Text = "btnAccept";
            btnAccept.Click += btnAccept_Click;

            btnCancel = new Button();
            btnCancel.ImagePosition = ButtonImagePosition.Left;
            btnCancel.Size = new Size(80, 26);
            btnCancel.Text = "btnCancel";

            lblFemale = new Label();
            lblFemale.Text = "lblFemale";

            lblMale = new Label();
            lblMale.Text = "lblMale";

            txtFPatr = new TextBox();
            //txtFPatr.KeyPress += edName_KeyPress;

            txtMPatr = new TextBox();
            //txtMPatr.KeyPress += edName_KeyPress;

            grpPatronymics = new GroupBox();
            grpPatronymics.Text = "grpPatronymics";
            grpPatronymics.Content = new TableLayout {
                Padding = new Padding(10),
                Spacing = new Size(10, 10),
                Rows = {
                    new TableRow {
                        Cells = { lblFemale, txtFPatr }
                    },
                    new TableRow {
                        Cells = { lblMale, txtMPatr }
                    }
                }
            };

            Content = new TableLayout {
                Padding = new Padding(10),
                Spacing = new Size(10, 10),
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
                    new TableRow {
                        Cells = { null, btnAccept, btnCancel }
                    }
                }
            };

            DefaultButton = btnAccept;
            AbortButton = btnCancel;
            ClientSize = new Size(385, 250);
            Title = "NameEditDlg";

            UIHelper.SetControlFont(this, "Tahoma", 8.25f);
            ResumeLayout();
        }
    }
}
