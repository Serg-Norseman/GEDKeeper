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
            GroupBox1 = new GroupBox();
            lblName = new Label();
            lblPriority = new Label();
            lblStatus = new Label();
            lblStartDate = new Label();
            lblStopDate = new Label();
            lblPercent = new Label();
            txtName = new TextBox();
            cmbPriority = new ComboBox();
            cmbStatus = new ComboBox();
            txtStartDate = new MaskedTextBox();
            txtStopDate = new MaskedTextBox();
            nudPercent = new NumericUpDown();
            btnAccept = new Button();
            btnCancel = new Button();
            tabsData = new TabControl();
            pageTasks = new TabPage();
            pageCommunications = new TabPage();
            pageGroups = new TabPage();
            pageNotes = new TabPage();

            SuspendLayout();

            GroupBox1.Size = new Size(852, 118);
            GroupBox1.Content = new TableLayout {
                Padding = new Padding(10),
                Spacing = new Size(10, 10),
                Rows = {
                    new TableRow {
                        Cells = { lblName, txtName }
                    },
                    new TableRow {
                        Cells = { lblPriority, cmbPriority, lblStatus, cmbStatus, lblPercent, nudPercent }
                    },
                    new TableRow {
                        Cells = { lblStartDate, txtStartDate, lblStopDate, txtStopDate }
                    },
                    null
                }
            };

            lblName.Size = new Size(67, 17);
            lblName.Text = "lblName";

            lblPriority.Size = new Size(80, 17);
            lblPriority.Text = "lblPriority";

            lblStatus.Size = new Size(78, 17);
            lblStatus.Text = "lblStatus";

            lblStartDate.Size = new Size(72, 17);
            lblStartDate.Text = "lblStartDate";

            lblStopDate.Size = new Size(77, 17);
            lblStopDate.Text = "lblStopDate";

            lblPercent.Size = new Size(64, 17);
            lblPercent.Text = "lblPercent";

            txtName.Size = new Size(740, 24);

            cmbPriority.ReadOnly = true;
            cmbPriority.Size = new Size(225, 25);

            cmbStatus.ReadOnly = true;
            cmbStatus.Size = new Size(225, 25);

            txtStartDate.Provider = new FixedMaskedTextProvider("00/00/0000");
            //txtStartDate.Mask = "00/00/0000";
            txtStartDate.Size = new Size(225, 24);
            //txtStartDate.TextMaskFormat = MaskFormat.IncludePromptAndLiterals;

            txtStopDate.Provider = new FixedMaskedTextProvider("00/00/0000");
            //txtStopDate.Mask = "00/00/0000";
            txtStopDate.Size = new Size(225, 24);
            //txtStopDate.TextMaskFormat = MaskFormat.IncludePromptAndLiterals;

            nudPercent.Increment = 5;
            nudPercent.Size = new Size(57, 24);

            btnAccept.ImagePosition = ButtonImagePosition.Left;
            btnAccept.Size = new Size(113, 30);
            btnAccept.Text = "btnAccept";
            btnAccept.Click += btnAccept_Click;

            btnCancel.ImagePosition = ButtonImagePosition.Left;
            btnCancel.Size = new Size(113, 30);
            btnCancel.Text = "btnCancel";
            btnCancel.Click += btnCancel_Click;

            tabsData.Pages.Add(pageTasks);
            tabsData.Pages.Add(pageCommunications);
            tabsData.Pages.Add(pageGroups);
            tabsData.Pages.Add(pageNotes);
            tabsData.SelectedIndex = 0;
            tabsData.Size = new Size(852, 379);

            pageTasks.Text = "pageTasks";

            pageCommunications.Text = "pageCommunications";

            pageGroups.Text = "pageGroups";

            pageNotes.Text = "pageNotes";

            Content = new TableLayout {
                Padding = new Padding(10),
                Spacing = new Size(10, 10),
                Rows = {
                    new TableRow {
                        Cells = { GroupBox1 }
                    },
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { tabsData }
                    },
                    new TableRow {
                        ScaleHeight = false,
                        Cells = { null, btnAccept, btnCancel }
                    }
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
