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
using GKCore.Design.Views;
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

        public TaskEditDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

            fNotesList = new GKSheetList(pageNotes);

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
