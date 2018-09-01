using Eto.Drawing;
using Eto.Forms;
using GKUI.Components;

namespace GKUI.Forms
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
            lblName.Text = "lblName";

            lblPriority = new Label();
            lblPriority.Text = "lblPriority";

            lblStatus = new Label();
            lblStatus.Text = "lblStatus";

            lblStartDate = new Label();
            lblStartDate.Text = "lblStartDate";

            lblStopDate = new Label();
            lblStopDate.Text = "lblStopDate";

            lblPercent = new Label();
            lblPercent.Text = "lblPercent";

            txtName = new TextBox();

            cmbPriority = new ComboBox();
            cmbPriority.ReadOnly = true;

            cmbStatus = new ComboBox();
            cmbStatus.ReadOnly = true;

            txtStartDate = new MaskedTextBox();
            txtStartDate.Provider = new FixedMaskedTextProvider("00/00/0000");

            txtStopDate = new MaskedTextBox();
            txtStopDate.Provider = new FixedMaskedTextProvider("00/00/0000");

            nudPercent = new NumericUpDown();
            nudPercent.Increment = 5;

            GroupBox1 = new GroupBox();
            GroupBox1.Content = new DefTableLayout {
                Rows = {
                    new TableRow {
                        Cells = { lblName, txtName }
                    },
                    new TableRow {
                        Cells = {
                            lblPriority,
                            TableLayout.Horizontal(10, cmbPriority, lblStatus, cmbStatus, lblPercent, nudPercent)
                        }
                    },
                    new TableRow {
                        Cells = {
                            lblStartDate,
                            TableLayout.Horizontal(10, txtStartDate, lblStopDate, txtStopDate)
                        }
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
            tabsData.Size = new Size(600, 260);

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
            Title = "ResearchEditDlg";

            SetPredefProperties(850, 560);
            ResumeLayout();
        }
    }
}
