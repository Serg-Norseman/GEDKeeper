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
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Lists;
using GKUI.Components;

namespace GKUI.Forms
{
    public sealed partial class RepositoryEditDlg : CommonDialog<IRepositoryEditDlg, RepositoryEditDlgController>, IRepositoryEditDlg
    {
        private readonly GKSheetList fNotesList;
        private readonly GKSheetList fUserRefList;

        public GDMRepositoryRecord RepositoryRecord
        {
            get { return fController.RepositoryRecord; }
            set { fController.RepositoryRecord = value; }
        }

        #region View Interface

        ISheetList IRepositoryEditDlg.NotesList
        {
            get { return fNotesList; }
        }

        ISheetList IRepositoryEditDlg.UserRefList
        {
            get { return fUserRefList; }
        }

        ITextBox IRepositoryEditDlg.Name
        {
            get { return GetControlHandler<ITextBox>(txtName); }
        }

        #endregion

        public RepositoryEditDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

            tabsData.SelectedIndexChanged += tabControl_SelectedIndexChanged;

            fNotesList = new GKSheetList(pageNotes);
            fUserRefList = new GKSheetList(pageUserRefs);

            fController = new RepositoryEditDlgController(this);
            fController.Init(baseWin);
        }

        private void btnAddress_Click(object sender, EventArgs e)
        {
            fController.ModifyAddress();
        }
    }
}
