/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2018 by Sergey V. Zhdanovskih.
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
using System.Windows.Forms;
using BSLib.Design.MVP.Controls;
using GDModel;
using GKCore;
using GKCore.Controllers;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.MVP.Controls;
using GKCore.MVP.Views;
using GKUI.Components;

namespace GKUI.Forms
{
    public sealed partial class TaskEditDlg : EditorDialog, ITaskEditDlg
    {
        private readonly TaskEditDlgController fController;

        private readonly GKSheetList fNotesList;

        public GDMTaskRecord Task
        {
            get { return fController.Task; }
            set { fController.Task = value; }
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

        ITextBox ITaskEditDlg.StartDate
        {
            get { return GetControlHandler<ITextBox>(txtStartDate); }
        }

        ITextBox ITaskEditDlg.StopDate
        {
            get { return GetControlHandler<ITextBox>(txtStopDate); }
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

        private void btnAccept_Click(object sender, EventArgs e)
        {
            DialogResult = fController.Accept() ? DialogResult.OK : DialogResult.None;
        }

        private void btnCancel_Click(object sender, EventArgs e)
        {
            try {
                fController.Cancel();
            } catch (Exception ex) {
                Logger.LogWrite("TaskEditDlg.btnCancel_Click(): " + ex.Message);
            }
        }

        private void btnGoalSelect_Click(object sender, EventArgs e)
        {
            fController.SelectGoal();
        }

        private void cmbGoalType_SelectedIndexChanged(object sender, EventArgs e)
        {
            fController.ChangeGoalType();
        }

        public TaskEditDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

            btnGoalSelect.Image = UIHelper.LoadResourceImage("Resources.btn_rec_new.gif");
            btnAccept.Image = UIHelper.LoadResourceImage("Resources.btn_accept.gif");
            btnCancel.Image = UIHelper.LoadResourceImage("Resources.btn_cancel.gif");

            fNotesList = new GKSheetList(pageNotes);

            // SetLang()
            Text = LangMan.LS(LSID.LSID_WinTaskEdit);
            btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
            btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
            pageNotes.Text = LangMan.LS(LSID.LSID_RPNotes);
            lblGoal.Text = LangMan.LS(LSID.LSID_Goal);
            lblPriority.Text = LangMan.LS(LSID.LSID_Priority);
            lblStartDate.Text = LangMan.LS(LSID.LSID_StartDate);
            lblStopDate.Text = LangMan.LS(LSID.LSID_StopDate);

            SetToolTip(btnGoalSelect, LangMan.LS(LSID.LSID_GoalSelectTip));

            fController = new TaskEditDlgController(this);
            fController.Init(baseWin);

            fNotesList.ListModel = new NoteLinksListModel(baseWin, fController.LocalUndoman);
        }
    }
}
