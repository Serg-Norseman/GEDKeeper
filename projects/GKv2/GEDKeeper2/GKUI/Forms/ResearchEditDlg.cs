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
using GKCore.Design.Views;
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

        public ResearchEditDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

            fTasksList = new GKSheetList(pageTasks);
            fCommunicationsList = new GKSheetList(pageCommunications);
            fGroupsList = new GKSheetList(pageGroups);
            fNotesList = new GKSheetList(pageNotes);

            fController = new ResearchEditDlgController(this);
            fController.InitView(baseWin);
        }
    }
}
