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
using GKCore.Design.Views;
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

        public TaskEditDlg(IBaseWindow baseWin)
        {
            XamlReader.Load(this);

            fController = new TaskEditDlgController(this);
            fController.InitView(baseWin);
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
