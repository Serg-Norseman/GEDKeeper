using System;
using Eto.Drawing;
using Eto.Forms;
using GKUI.Components;

namespace GKUI.Dialogs
{
    partial class TaskEditDlg
    {
        private GroupBox GroupBox1;
        private TabControl tabsData;
        private TabPage pageNotes;
        private Button btnAccept;
        private Button btnCancel;
        private Label lblPriority;
        private ComboBox txtPriority;
        private Label lblStartDate;
        private MaskedTextBox txtStartDate;
        private MaskedTextBox txtStopDate;
        private Label lblStopDate;
        private Label lblGoal;
        private ComboBox cmbGoalType;
        private TextBox txtGoal;
        private Button btnGoalSelect;

        private void InitializeComponent()
        {
            GroupBox1 = new GroupBox();
            lblPriority = new Label();
            lblStartDate = new Label();
            lblStopDate = new Label();
            lblGoal = new Label();
            btnGoalSelect = new Button();
            txtPriority = new ComboBox();
            txtStartDate = new MaskedTextBox();
            txtStopDate = new MaskedTextBox();
            cmbGoalType = new ComboBox();
            txtGoal = new TextBox();
            btnAccept = new Button();
            btnCancel = new Button();
            tabsData = new TabControl();
            pageNotes = new TabPage();

            SuspendLayout();

            GroupBox1.Size = new Size(674, 118);
            GroupBox1.Content = new TableLayout {
                Padding = new Padding(10),
                Spacing = new Size(10, 10),
                Rows = {
                    new TableRow {
                        Cells = { lblGoal, cmbGoalType, txtGoal, btnGoalSelect }
                    },
                    new TableRow {
                        Cells = { lblPriority, txtPriority }
                    },
                    new TableRow {
                        Cells = { lblStartDate, txtStartDate, lblStopDate, txtStopDate }
                    },
                    null
                }
            };

            lblPriority.Size = new Size(63, 17);
            lblPriority.Text = "lblPriority";

            lblStartDate.Size = new Size(79, 17);
            lblStartDate.Text = "lblStartDate";

            lblStopDate.Size = new Size(78, 17);
            lblStopDate.Text = "lblStopDate";

            lblGoal.Size = new Size(46, 17);
            lblGoal.Text = "lblGoal";

            btnGoalSelect.Size = new Size(39, 34);
            btnGoalSelect.Click += btnGoalSelect_Click;

            txtPriority.ReadOnly = true;
            txtPriority.Size = new Size(225, 25);

            txtStartDate.Provider = new FixedMaskedTextProvider("00/00/0000");
            //txtStartDate.Mask = "00/00/0000";
            txtStartDate.Size = new Size(225, 24);
            //txtStartDate.TextMaskFormat = MaskFormat.IncludePromptAndLiterals;

            txtStopDate.Provider = new FixedMaskedTextProvider("00/00/0000");
            //txtStopDate.Mask = "00/00/0000";
            txtStopDate.Size = new Size(225, 24);
            //txtStopDate.TextMaskFormat = MaskFormat.IncludePromptAndLiterals;

            cmbGoalType.ReadOnly = true;
            cmbGoalType.Size = new Size(158, 25);
            cmbGoalType.SelectedIndexChanged += cmbGoalType_SelectedIndexChanged;

            txtGoal.ReadOnly = true;
            txtGoal.Size = new Size(348, 24);

            btnAccept.ImagePosition = ButtonImagePosition.Left;
            btnAccept.Size = new Size(113, 30);
            btnAccept.Text = "btnAccept";
            btnAccept.Click += btnAccept_Click;

            btnCancel.ImagePosition = ButtonImagePosition.Left;
            btnCancel.Size = new Size(113, 30);
            btnCancel.Text = "btnCancel";
            btnCancel.Click += btnCancel_Click;

            tabsData.Pages.Add(pageNotes);
            tabsData.SelectedIndex = 0;
            tabsData.Size = new Size(674, 311);

            pageNotes.Size = new Size(666, 281);
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
            ClientSize = new Size(674, 494);
            Title = "TaskEditDlg";

            UIHelper.SetControlFont(this, "Tahoma", 8.25f);
            ResumeLayout();
        }
    }
}
