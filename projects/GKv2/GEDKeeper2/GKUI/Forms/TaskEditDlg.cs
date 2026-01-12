/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
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
        private readonly GKSheetList fNotesList;

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
            InitializeComponent();

            fNotesList = new GKSheetList(pageNotes);

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
