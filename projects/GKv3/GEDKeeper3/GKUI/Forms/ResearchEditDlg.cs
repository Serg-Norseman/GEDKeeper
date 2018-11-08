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
using Eto.Forms;

using GKCommon.GEDCOM;
using GKCore;
using GKCore.Controllers;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.MVP.Controls;
using GKCore.MVP.Views;
using GKCore.Types;
using GKUI.Components;

namespace GKUI.Forms
{
    public partial class ResearchEditDlg : EditorDialog, IResearchEditDlg
    {
        private readonly ResearchEditDlgController fController;

        private readonly GKSheetList fTasksList;
        private readonly GKSheetList fCommunicationsList;
        private readonly GKSheetList fGroupsList;
        private readonly GKSheetList fNotesList;

        public GEDCOMResearchRecord Research
        {
            get { return fController.Research; }
            set { fController.Research = value; }
        }

        #region View Interface

        ISheetList IResearchEditDlg.TasksList
        {
            get { return fTasksList; }
        }

        ISheetList IResearchEditDlg.CommunicationsList
        {
            get { return fCommunicationsList; }
        }

        ISheetList IResearchEditDlg.GroupsList
        {
            get { return fGroupsList; }
        }

        ISheetList IResearchEditDlg.NotesList
        {
            get { return fNotesList; }
        }


        ITextBoxHandler IResearchEditDlg.Name
        {
            get { return GetControlHandler<ITextBoxHandler>(txtName); }
        }

        IComboBoxHandler IResearchEditDlg.Priority
        {
            get { return GetControlHandler<IComboBoxHandler>(cmbPriority); }
        }

        IComboBoxHandler IResearchEditDlg.Status
        {
            get { return GetControlHandler<IComboBoxHandler>(cmbStatus); }
        }

        ITextBoxHandler IResearchEditDlg.StartDate
        {
            get { return GetControlHandler<ITextBoxHandler>(txtStartDate); }
        }

        ITextBoxHandler IResearchEditDlg.StopDate
        {
            get { return GetControlHandler<ITextBoxHandler>(txtStopDate); }
        }

        INumericBoxHandler IResearchEditDlg.Percent
        {
            get { return GetControlHandler<INumericBoxHandler>(nudPercent); }
        }

        #endregion

        public ResearchEditDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

            btnAccept.Image = UIHelper.LoadResourceImage("Resources.btn_accept.gif");
            btnCancel.Image = UIHelper.LoadResourceImage("Resources.btn_cancel.gif");

            fTasksList = new GKSheetList(pageTasks);
            fTasksList.OnModify += ListTasksModify;

            fCommunicationsList = new GKSheetList(pageCommunications);
            fCommunicationsList.OnModify += ListCommunicationsModify;

            fGroupsList = new GKSheetList(pageGroups);
            fGroupsList.OnModify += ListGroupsModify;

            fNotesList = new GKSheetList(pageNotes);

            SetLang();

            fController = new ResearchEditDlgController(this);
            fController.Init(baseWin);

            fTasksList.ListModel = new ResTasksSublistModel(baseWin, fController.LocalUndoman);
            fCommunicationsList.ListModel = new ResCommunicationsSublistModel(baseWin, fController.LocalUndoman);
            fGroupsList.ListModel = new ResGroupsSublistModel(baseWin, fController.LocalUndoman);
            fNotesList.ListModel = new NoteLinksListModel(baseWin, fController.LocalUndoman);
        }

        public void SetLang()
        {
            btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
            btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
            Title = LangMan.LS(LSID.LSID_WinResearchEdit);
            pageTasks.Text = LangMan.LS(LSID.LSID_RPTasks);
            pageCommunications.Text = LangMan.LS(LSID.LSID_RPCommunications);
            pageGroups.Text = LangMan.LS(LSID.LSID_RPGroups);
            pageNotes.Text = LangMan.LS(LSID.LSID_RPNotes);
            lblName.Text = LangMan.LS(LSID.LSID_Title);
            lblPriority.Text = LangMan.LS(LSID.LSID_Priority);
            lblStatus.Text = LangMan.LS(LSID.LSID_Status);
            lblPercent.Text = LangMan.LS(LSID.LSID_Percent);
            lblStartDate.Text = LangMan.LS(LSID.LSID_StartDate);
            lblStopDate.Text = LangMan.LS(LSID.LSID_StopDate);
        }

        private void ListTasksModify(object sender, ModifyEventArgs eArgs)
        {
            if (eArgs.Action == RecordAction.raJump) {
                fController.JumpToRecord(eArgs.ItemData as GEDCOMTaskRecord);
            }
        }

        private void ListCommunicationsModify(object sender, ModifyEventArgs eArgs)
        {
            if (eArgs.Action == RecordAction.raJump) {
                fController.JumpToRecord(eArgs.ItemData as GEDCOMCommunicationRecord);
            }
        }

        private void ListGroupsModify(object sender, ModifyEventArgs eArgs)
        {
            if (eArgs.Action == RecordAction.raJump) {
                fController.JumpToRecord(eArgs.ItemData as GEDCOMGroupRecord);
            }
        }

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
                Logger.LogWrite("ResearchEditDlg.btnCancel_Click(): " + ex.Message);
            }
        }
    }
}
