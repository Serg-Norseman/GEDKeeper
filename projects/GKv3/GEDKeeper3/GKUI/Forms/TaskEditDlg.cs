/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using Eto.Forms;
using Eto.Serialization.Xaml;
using GDModel;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Lists;
using GKUI.Components;

namespace GKUI.Forms
{
    public sealed partial class TaskEditDlg : CommonDialog<ITaskEditDlg, TaskEditDlgController>, ITaskEditDlg
    {
        #region Design components
#pragma warning disable CS0169, CS0649, IDE0044, IDE0051

        private GroupBox GroupBox1;
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
        private GKSheetList fNotesList;

#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
        #endregion

        public GDMTaskRecord TaskRecord
        {
            get { return fController.TaskRecord; }
            set { fController.TaskRecord = value; }
        }

        #region View Interface

        ISheetList ITaskEditDlg.NotesList
        {
            get { return fNotesList; }
        }

        IComboBox ITaskEditDlg.Priority
        {
            get { return GetControlHandler<IComboBox>(txtPriority); }
        }

        IDateBox ITaskEditDlg.StartDate
        {
            get { return GetControlHandler<IDateBox>(txtStartDate); }
        }

        IDateBox ITaskEditDlg.StopDate
        {
            get { return GetControlHandler<IDateBox>(txtStopDate); }
        }

        IComboBox ITaskEditDlg.GoalType
        {
            get { return GetControlHandler<IComboBox>(cmbGoalType); }
        }

        ITextBox ITaskEditDlg.Goal
        {
            get { return GetControlHandler<ITextBox>(txtGoal); }
        }

        IButton ITaskEditDlg.GoalSelect
        {
            get { return GetControlHandler<IButton>(btnGoalSelect); }
        }

        #endregion

        public TaskEditDlg(IBaseWindow baseWin)
        {
            XamlReader.Load(this);

            fController = new TaskEditDlgController(this);
            fController.Init(baseWin);
        }

        private void btnGoalSelect_Click(object sender, EventArgs e)
        {
            fController.SelectGoal();
        }

        private void cmbGoalType_SelectedIndexChanged(object sender, EventArgs e)
        {
            fController.ChangeGoalType();
        }
    }
}
