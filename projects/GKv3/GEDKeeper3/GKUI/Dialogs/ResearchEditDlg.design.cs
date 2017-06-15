using System;
using Eto.Drawing;
using Eto.Forms;
using GKUI.Components;

namespace GKUI.Dialogs
{
    partial class ResearchEditDlg
    {
        private GroupBox GroupBox1;
        private TextBox txtName;
        private Label lblName;
        private TabControl tabsData;
        private TabPage pageNotes;
        private TabPage pageTasks;
        private Button btnAccept;
        private Button btnCancel;
        private Label lblPriority;
        private ComboBox cmbPriority;
        private TabPage pageCommunications;
        private Label lblStatus;
        private ComboBox cmbStatus;
        private Label lblStartDate;
        private MaskedTextBox txtStartDate;
        private Label lblStopDate;
        private MaskedTextBox txtStopDate;
        private Label lblPercent;
        private NumericUpDown nudPercent;
        private TabPage pageGroups;

        private void InitializeComponent()
        {
            SuspendLayout();

            lblName = new Label();
            //lblName.Size = new Size(67, 17);
            lblName.Text = "lblName";

            lblPriority = new Label();
            //lblPriority.Size = new Size(80, 17);
            lblPriority.Text = "lblPriority";

            lblStatus = new Label();
            //lblStatus.Size = new Size(78, 17);
            lblStatus.Text = "lblStatus";

            lblStartDate = new Label();
            //lblStartDate.Size = new Size(72, 17);
            lblStartDate.Text = "lblStartDate";

            lblStopDate = new Label();
            //lblStopDate.Size = new Size(77, 17);
            lblStopDate.Text = "lblStopDate";

            lblPercent = new Label();
            //lblPercent.Size = new Size(64, 17);
            lblPercent.Text = "lblPercent";

            txtName = new TextBox();
            //txtName.Size = new Size(740, 24);

            cmbPriority = new ComboBox();
            cmbPriority.ReadOnly = true;
            //cmbPriority.Size = new Size(225, 25);

            cmbStatus = new ComboBox();
            cmbStatus.ReadOnly = true;
            //cmbStatus.Size = new Size(225, 25);

            txtStartDate = new MaskedTextBox();
            txtStartDate.Provider = new FixedMaskedTextProvider("00/00/0000");
            //txtStartDate.Size = new Size(225, 24);

            txtStopDate = new MaskedTextBox();
            txtStopDate.Provider = new FixedMaskedTextProvider("00/00/0000");
            //txtStopDate.Size = new Size(225, 24);

            nudPercent = new NumericUpDown();
            nudPercent.Increment = 5;
            //nudPercent.Size = new Size(57, 24);

            GroupBox1 = new GroupBox();
            GroupBox1.Content = new DefTableLayout {
                Rows = {
                    new TableRow {
                        Cells = { lblName, txtName }
                    },
                    new TableRow {
                        Cells = { lblPriority, cmbPriority, lblStatus, cmbStatus, lblPercent, nudPercent }
                    },
                    new TableRow {
                        Cells = { lblStartDate, txtStartDate, lblStopDate, txtStopDate }
                    }
                }
            };

            //

            pageTasks = new TabPage();
            pageTasks.Text = "pageTasks";

            pageCommunications = new TabPage();
            pageCommunications.Text = "pageCommunications";

            pageGroups = new TabPage();
            pageGroups.Text = "pageGroups";

            pageNotes = new TabPage();
            pageNotes.Text = "pageNotes";

            tabsData = new TabControl();
            tabsData.Pages.Add(pageTasks);
            tabsData.Pages.Add(pageCommunications);
            tabsData.Pages.Add(pageGroups);
            tabsData.Pages.Add(pageNotes);

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

            Content = new DefTableLayout {
                Rows = {
                    new TableRow {
                        Cells = { GroupBox1 }
                    },
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { tabsData }
                    },
                    UIHelper.MakeDialogFooter(null, btnAccept, btnCancel)
                }
            };

            DefaultButton = btnAccept;
            AbortButton = btnCancel;
            ClientSize = new Size(852, 557);
            Title = "ResearchEditDlg";

            UIHelper.SetControlFont(this, "Tahoma", 8.25f);
            ResumeLayout();
        }
    }
}
