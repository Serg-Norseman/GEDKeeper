﻿using Eto.Drawing;
using Eto.Forms;
using GKUI.Components;

namespace GKUI.Forms
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
        private GKDateBox txtStartDate;
        private GKDateBox txtStopDate;
        private Label lblStopDate;
        private Label lblGoal;
        private ComboBox cmbGoalType;
        private TextBox txtGoal;
        private Button btnGoalSelect;

        private void InitializeComponent()
        {
            SuspendLayout();

            lblPriority = new Label();
            lblPriority.Text = "lblPriority";

            lblStartDate = new Label();
            lblStartDate.Text = "lblStartDate";

            lblStopDate = new Label();
            lblStopDate.Text = "lblStopDate";

            lblGoal = new Label();
            lblGoal.Text = "lblGoal";

            btnGoalSelect = new Button();
            btnGoalSelect.Size = new Size(26, 26);
            btnGoalSelect.Click += btnGoalSelect_Click;

            txtPriority = new ComboBox();
            txtPriority.ReadOnly = true;

            txtStartDate = new GKDateBox();
            txtStartDate.Provider = new FixedMaskedTextProvider("00/00/0000");

            txtStopDate = new GKDateBox();
            txtStopDate.Provider = new FixedMaskedTextProvider("00/00/0000");

            cmbGoalType = new ComboBox();
            cmbGoalType.ReadOnly = true;
            cmbGoalType.SelectedIndexChanged += cmbGoalType_SelectedIndexChanged;

            txtGoal = new TextBox();
            txtGoal.ReadOnly = true;

            GroupBox1 = new GroupBox();
            GroupBox1.Content = new DefTableLayout {
                Rows = {
                    new TableRow {
                        Cells = {
                            lblGoal,
                            cmbGoalType,
                            TableLayout.Horizontal(10, new TableCell(txtGoal, true), btnGoalSelect)
                        }
                    },
                    new TableRow {
                        Cells = {
                            lblPriority,
                            txtPriority,
                            TableLayout.Horizontal(10, lblStartDate, txtStartDate, lblStopDate, txtStopDate)
                        }
                    }
                }
            };

            //

            pageNotes = new TabPage();
            pageNotes.Text = "pageNotes";

            tabsData = new TabControl();
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
            Title = "TaskEditDlg";

            SetPredefProperties(680, 500);
            ResumeLayout();
        }
    }
}
