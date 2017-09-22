using Eto.Drawing;
using Eto.Forms;
using GKUI.Components;

namespace GKUI.Forms
{
    partial class GroupEditDlg
    {
        private TextBox edName;
        private Label lblName;
        private TabControl tabsGroupData;
        private TabPage pageNotes;
        private TabPage pageMultimedia;
        private TabPage pageMembers;
        private Button btnAccept;
        private Button btnCancel;

        private void InitializeComponent()
        {
            SuspendLayout();

            lblName = new Label();
            lblName.Text = "lblName";

            edName = new TextBox();

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
            btnCancel.Click += btnCancel_Click;
            btnCancel.Image = Bitmap.FromResource("Resources.btn_cancel.gif");

            pageMembers = new TabPage();
            pageMembers.Text = "pageMembers";

            pageNotes = new TabPage();
            pageNotes.Text = "pageNotes";

            pageMultimedia = new TabPage();
            pageMultimedia.Text = "pageMultimedia";

            tabsGroupData = new TabControl();
            tabsGroupData.Pages.Add(pageMembers);
            tabsGroupData.Pages.Add(pageNotes);
            tabsGroupData.Pages.Add(pageMultimedia);

            Content = new DefTableLayout {
                Rows = {
                    new TableRow {
                        Cells = { TableLayout.Horizontal(10, lblName, edName) }
                    },
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { tabsGroupData }
                    },
                    UIHelper.MakeDialogFooter(null, btnAccept, btnCancel)
                }
            };

            DefaultButton = btnAccept;
            AbortButton = btnCancel;
            Title = "GroupEditDlg";

            SetPredefProperties(580, 460);
            ResumeLayout();
        }
    }
}
