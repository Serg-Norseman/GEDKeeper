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
    public sealed partial class GroupEditDlg : CommonDialog<IGroupEditDlg, GroupEditDlgController>, IGroupEditDlg
    {
        private readonly GKSheetList fMembersList;
        private readonly GKSheetList fNotesList;
        private readonly GKSheetList fMediaList;

        public GDMGroupRecord GroupRecord
        {
            get { return fController.GroupRecord; }
            set { fController.GroupRecord = value; }
        }

        public GroupEditDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

            fMembersList = new GKSheetList(pageMembers);
            fNotesList = new GKSheetList(pageNotes);
            fMediaList = new GKSheetList(pageMultimedia);

            fController = new GroupEditDlgController(this);
            fController.InitView(baseWin);
        }
    }
}
