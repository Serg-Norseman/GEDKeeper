using System;
using Eto.Drawing;
using Eto.Forms;
using GKUI.Components;

namespace GKUI.Dialogs
{
    partial class RepositoryEditDlg
    {
        private Button btnAccept;
        private Button btnCancel;
        private GroupBox GroupBox1;
        private Label lblName;
        private TextBox txtName;
        private TabControl tabsData;
        private TabPage pageNotes;
        private Button btnAddress;

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
            btnCancel.Click += btnCancel_Click;

            lblName = new Label();
            lblName.Text = "lblName";

            txtName = new TextBox();

            btnAddress = new Button();
            btnAddress.Size = new Size(80, 26);
            btnAddress.Text = "btnAddress";
            btnAddress.Click += btnAddress_Click;

            pageNotes = new TabPage();
            pageNotes.Text = "pageNotes";

            tabsData = new TabControl();
            tabsData.Pages.Add(pageNotes);
            tabsData.SelectedIndex = 0;

            Content = new TableLayout {
                Padding = new Padding(10),
                Spacing = new Size(10, 10),
                Rows = {
                    new TableRow {
                        Cells = { lblName, txtName }
                    },
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { tabsData }
                    },
                    new TableRow {
                        Cells = { btnAddress, null, btnAccept, btnCancel }
                    }
                }
            };

            DefaultButton = btnAccept;
            AbortButton = btnCancel;
            ClientSize = new Size(586, 455);
            Title = "RepositoryEditDlg";

            UIHelper.SetControlFont(this, "Tahoma", 8.25f);
            ResumeLayout();
        }
    }
}
