/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
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

using GDModel;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Lists;
using GKUI.Components;

namespace GKUI.Forms
{
    public partial class ResearchEditDlg : CommonDialog<IResearchEditDlg, ResearchEditDlgController>, IResearchEditDlg
    {
        private readonly GKSheetList fTasksList;
        private readonly GKSheetList fCommunicationsList;
        private readonly GKSheetList fGroupsList;
        private readonly GKSheetList fNotesList;

        public GDMResearchRecord ResearchRecord
        {
            get { return fController.ResearchRecord; }
            set { fController.ResearchRecord = value; }
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
            InitializeComponent();

            fTasksList = new GKSheetList(pageTasks);
            fCommunicationsList = new GKSheetList(pageCommunications);
            fGroupsList = new GKSheetList(pageGroups);
            fNotesList = new GKSheetList(pageNotes);

            fController = new ResearchEditDlgController(this);
            fController.Init(baseWin);
        }
    }
}
