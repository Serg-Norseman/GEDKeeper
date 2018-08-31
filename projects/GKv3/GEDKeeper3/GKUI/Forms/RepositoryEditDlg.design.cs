using Eto.Drawing;
using Eto.Forms;
using GKUI.Components;

namespace GKUI.Forms
{
    partial class RepositoryEditDlg
    {
        private Button btnAccept;
        private Button btnCancel;
        private Label lblName;
        private TextBox txtName;
        private TabControl tabsData;
        private TabPage pageNotes;
        private Button btnAddress;

        private void InitializeComponent()
        {
            SuspendLayout();

            lblName = new Label();
            lblName.Text = "lblName";

            txtName = new TextBox();

            pageNotes = new TabPage();
            pageNotes.Text = "pageNotes";

            tabsData = new TabControl();
            tabsData.Pages.Add(pageNotes);
            tabsData.Size = new Size(600, 260);

            //

            btnAddress = new Button();
            btnAddress.Size = new Size(130, 26);
            btnAddress.Text = "btnAddress";
            btnAddress.Click += btnAddress_Click;

            btnAccept = new Button();
            btnAccept.ImagePosition = ButtonImagePosition.Left;
            btnAccept.Size = new Size(130, 26);
            btnAccept.Text = "btnAccept";
            btnAccept.Click += btnAccept_Click;

            btnCancel = new Button();
            btnCancel.ImagePosition = ButtonImagePosition.Left;
            btnCancel.Size = new Size(130, 26);
            btnCancel.Text = "btnCancel";
            btnCancel.Click += btnCancel_Click;

            Content = new DefTableLayout {
                Rows = {
                    new TableRow {
                        Cells = { TableLayout.Horizontal(10, lblName, txtName) }
                    },
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { tabsData }
                    },
                    UIHelper.MakeDialogFooter(btnAddress, null, btnAccept, btnCancel)
                }
            };

            DefaultButton = btnAccept;
            AbortButton = btnCancel;
            Title = "RepositoryEditDlg";

            SetPredefProperties(580, 460);
            ResumeLayout();
        }
    }
}
