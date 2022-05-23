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

        private readonly TaskEditDlgController fController;

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

            txtStartDate.Provider = new FixedMaskedTextProvider("00/00/0000");
            txtStopDate.Provider = new FixedMaskedTextProvider("00/00/0000");

            fController = new TaskEditDlgController(this);
            fController.Init(baseWin);

            fNotesList.ListModel = new NoteLinksListModel(baseWin, fController.LocalUndoman);
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
