using System;
using Eto.Drawing;
using Eto.Forms;
using GKUI.Components;

namespace GKUI.Dialogs
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
            btnAccept.Size = new Size(80, 26);
            btnAccept.Text = "btnAccept";
            btnAccept.Click += btnAccept_Click;

            btnCancel = new Button();
            btnCancel.ImagePosition = ButtonImagePosition.Left;
            btnCancel.Size = new Size(80, 26);
            btnCancel.Text = "btnCancel";
            btnCancel.Click += btnCancel_Click;

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
            tabsGroupData.SelectedIndex = 0;

            Content = new TableLayout {
                Padding = new Padding(10),
                Spacing = new Size(10, 10),
                Rows = {
                    new TableRow {
                        Cells = { lblName, edName }
                    },
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { tabsGroupData }
                    },
                    new TableRow {
                        Cells = { null, btnAccept, btnCancel }
                    }
                }
            };

            DefaultButton = btnAccept;
            AbortButton = btnCancel;
            ClientSize = new Size(673, 560);
            Title = "GroupEditDlg";

            UIHelper.SetControlFont(this, "Tahoma", 8.25f);
            ResumeLayout();
        }
    }
}
