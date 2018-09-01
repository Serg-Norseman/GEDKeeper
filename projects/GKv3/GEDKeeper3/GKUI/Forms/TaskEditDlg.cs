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
using Eto.Drawing;
using Eto.Forms;

using GKCommon.GEDCOM;
using GKCore;
using GKCore.Controllers;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.UIContracts;
using GKUI.Components;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    public sealed partial class TaskEditDlg : EditorDialog, ITaskEditDlg
    {
        private readonly TaskEditDlgController fController;

        private readonly GKSheetList fNotesList;

        public GEDCOMTaskRecord Task
        {
            get { return fController.Task; }
            set { fController.Task = value; }
        }

        #region View Interface

        ISheetList ITaskEditDlg.NotesList
        {
            get { return fNotesList; }
        }

        IComboBoxHandler ITaskEditDlg.Priority
        {
            get { return fControlsManager.GetControlHandler<IComboBoxHandler>(txtPriority); }
        }

        ITextBoxHandler ITaskEditDlg.StartDate
        {
            get { return fControlsManager.GetControlHandler<ITextBoxHandler>(txtStartDate); }
        }

        ITextBoxHandler ITaskEditDlg.StopDate
        {
            get { return fControlsManager.GetControlHandler<ITextBoxHandler>(txtStopDate); }
        }

        IComboBoxHandler ITaskEditDlg.GoalType
        {
            get { return fControlsManager.GetControlHandler<IComboBoxHandler>(cmbGoalType); }
        }

        ITextBoxHandler ITaskEditDlg.Goal
        {
            get { return fControlsManager.GetControlHandler<ITextBoxHandler>(txtGoal); }
        }

        IButtonHandler ITaskEditDlg.GoalSelect
        {
            get { return fControlsManager.GetControlHandler<IButtonHandler>(btnGoalSelect); }
        }

        #endregion

        private void btnAccept_Click(object sender, EventArgs e)
        {
            DialogResult = fController.Accept() ? DialogResult.Ok : DialogResult.None;
        }

        private void btnCancel_Click(object sender, EventArgs e)
        {
            try {
                fController.Cancel();
                CancelClickHandler(sender, e);
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

        public TaskEditDlg()
        {
            InitializeComponent();

            btnGoalSelect.Image = UIHelper.LoadResourceImage("Resources.btn_rec_new.gif");
            btnAccept.Image = UIHelper.LoadResourceImage("Resources.btn_accept.gif");
            btnCancel.Image = UIHelper.LoadResourceImage("Resources.btn_cancel.gif");

            fNotesList = new GKSheetList(pageNotes);

            // SetLang()
            Title = LangMan.LS(LSID.LSID_WinTaskEdit);
            btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
            btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
            pageNotes.Text = LangMan.LS(LSID.LSID_RPNotes);
            lblGoal.Text = LangMan.LS(LSID.LSID_Goal);
            lblPriority.Text = LangMan.LS(LSID.LSID_Priority);
            lblStartDate.Text = LangMan.LS(LSID.LSID_StartDate);
            lblStopDate.Text = LangMan.LS(LSID.LSID_StopDate);

            btnGoalSelect.ToolTip = LangMan.LS(LSID.LSID_GoalSelectTip);

            fController = new TaskEditDlgController(this);
        }

        public override void InitDialog(IBaseWindow baseWin)
        {
            base.InitDialog(baseWin);
            fController.Init(baseWin);

            fNotesList.ListModel = new NoteLinksListModel(fBase, fController.LocalUndoman);
        }
    }
}
