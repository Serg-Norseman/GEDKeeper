/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2024 by Sergey V. Zhdanovskih.
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
using GDModel;
using GKCore.Controllers;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Interfaces;
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

            btnAccept.Image = UIHelper.LoadResourceImage("Resources.btn_accept.gif");
            btnCancel.Image = UIHelper.LoadResourceImage("Resources.btn_cancel.gif");

            fNotesList = new GKSheetList(pageNotes);

            fUserRefList = new GKSheetList(pageUserRefs);
            fUserRefList.SetControlName("fUserRefList"); // for purpose of tests

            fController = new RepositoryEditDlgController(this);
            fController.Init(baseWin);
        }

        private void btnAddress_Click(object sender, EventArgs e)
        {
            fController.ModifyAddress();
        }
    }
}
