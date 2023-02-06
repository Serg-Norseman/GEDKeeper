/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
 *
 *  This file is part of "GEDKeeper".
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

using System;
using GDModel;
using GKCore.Controllers;
using GKCore.Design.MVP.Controls;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.MVP.Controls;
using GKCore.MVP.Views;
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

            btnGoalSelect.Image = UIHelper.LoadResourceImage("Resources.btn_rec_new.gif");
            btnAccept.Image = UIHelper.LoadResourceImage("Resources.btn_accept.gif");
            btnCancel.Image = UIHelper.LoadResourceImage("Resources.btn_cancel.gif");

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
