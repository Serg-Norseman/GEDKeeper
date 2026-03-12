/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
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
