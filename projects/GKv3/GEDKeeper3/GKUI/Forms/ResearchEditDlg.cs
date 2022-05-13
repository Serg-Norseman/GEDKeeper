/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2022 by Sergey V. Zhdanovskih.
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
using System.ComponentModel;
using BSLib.Design.MVP.Controls;
using Eto.Forms;
using Eto.Serialization.Xaml;
using GDModel;
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
        #region Design components

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
        private GKDateBox txtStartDate;
        private Label lblStopDate;
        private GKDateBox txtStopDate;
        private Label lblPercent;
        private NumericUpDown nudPercent;
        private TabPage pageGroups;

        #endregion

        private readonly ResearchEditDlgController fController;

        private readonly GKSheetList fTasksList;
        private readonly GKSheetList fCommunicationsList;
        private readonly GKSheetList fGroupsList;
        private readonly GKSheetList fNotesList;

        public GDMResearchRecord Research
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


        ITextBox IResearchEditDlg.Name
        {
            get { return GetControlHandler<ITextBox>(txtName); }
        }

        IComboBox IResearchEditDlg.Priority
        {
            get { return GetControlHandler<IComboBox>(cmbPriority); }
        }

        IComboBox IResearchEditDlg.Status
        {
            get { return GetControlHandler<IComboBox>(cmbStatus); }
        }

        IDateBox IResearchEditDlg.StartDate
        {
            get { return GetControlHandler<IDateBox>(txtStartDate); }
        }

        IDateBox IResearchEditDlg.StopDate
        {
            get { return GetControlHandler<IDateBox>(txtStopDate); }
        }

        INumericBox IResearchEditDlg.Percent
        {
            get { return GetControlHandler<INumericBox>(nudPercent); }
        }

        #endregion

        public ResearchEditDlg(IBaseWindow baseWin)
        {
            XamlReader.Load(this);

            txtStartDate.Provider = new FixedMaskedTextProvider("00/00/0000");
            txtStopDate.Provider = new FixedMaskedTextProvider("00/00/0000");

            btnAccept.Image = UIHelper.LoadResourceImage("Resources.btn_accept.gif");
            btnCancel.Image = UIHelper.LoadResourceImage("Resources.btn_cancel.gif");

            fTasksList = new GKSheetList(pageTasks);
            fTasksList.OnModify += ListTasksModify;

            fCommunicationsList = new GKSheetList(pageCommunications);
            fCommunicationsList.OnModify += ListCommunicationsModify;

            fGroupsList = new GKSheetList(pageGroups);
            fGroupsList.OnModify += ListGroupsModify;

            fNotesList = new GKSheetList(pageNotes);

            fController = new ResearchEditDlgController(this);
            fController.Init(baseWin);

            fTasksList.ListModel = new ResTasksSublistModel(baseWin, fController.LocalUndoman);
            fCommunicationsList.ListModel = new ResCommunicationsSublistModel(baseWin, fController.LocalUndoman);
            fGroupsList.ListModel = new ResGroupsSublistModel(baseWin, fController.LocalUndoman);
            fNotesList.ListModel = new NoteLinksListModel(baseWin, fController.LocalUndoman);
        }

        private void ListTasksModify(object sender, ModifyEventArgs eArgs)
        {
            if (eArgs.Action == RecordAction.raJump) {
                fController.JumpToRecord(eArgs.ItemData as GDMTaskRecord);
            }
        }

        private void ListCommunicationsModify(object sender, ModifyEventArgs eArgs)
        {
            if (eArgs.Action == RecordAction.raJump) {
                fController.JumpToRecord(eArgs.ItemData as GDMCommunicationRecord);
            }
        }

        private void ListGroupsModify(object sender, ModifyEventArgs eArgs)
        {
            if (eArgs.Action == RecordAction.raJump) {
                fController.JumpToRecord(eArgs.ItemData as GDMGroupRecord);
            }
        }

        private void btnAccept_Click(object sender, EventArgs e)
        {
            DialogResult = fController.Accept() ? DialogResult.Ok : DialogResult.None;
        }

        private void btnCancel_Click(object sender, EventArgs e)
        {
            DialogResult = fController.Cancel() ? DialogResult.Cancel : DialogResult.None;
        }

        protected override void OnClosing(CancelEventArgs e)
        {
            base.OnClosing(e);
            e.Cancel = fController.CheckChangesPersistence();
        }
    }
}
