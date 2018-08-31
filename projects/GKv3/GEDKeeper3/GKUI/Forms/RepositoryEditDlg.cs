/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2018 by Sergey V. Zhdanovskih.
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
using Eto.Forms;

using GKCommon.GEDCOM;
using GKCore;
using GKCore.Controllers;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.Types;
using GKCore.UIContracts;
using GKUI.Components;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    public sealed partial class RepositoryEditDlg : EditorDialog, IRepositoryEditDlg
    {
        private readonly RepositoryEditDlgController fController;

        private readonly GKSheetList fNotesList;

        public GEDCOMRepositoryRecord Repository
        {
            get { return fController.Repository; }
            set { fController.Repository = value; }
        }

        #region View Interface

        ISheetList IRepositoryEditDlg.NotesList
        {
            get { return fNotesList; }
        }

        ITextBoxHandler IRepositoryEditDlg.Name
        {
            get { return fControlsManager.GetControlHandler<ITextBoxHandler>(txtName); }
        }

        #endregion

        private void btnAddress_Click(object sender, EventArgs e)
        {
            fController.ModifyAddress();
        }

        private void btnAccept_Click(object sender, EventArgs e)
        {
            bool res = fController.Accept();
            if (res) {
                CommitChanges();
            }
            DialogResult = res ? DialogResult.Ok : DialogResult.None;
        }

        private void btnCancel_Click(object sender, EventArgs e)
        {
            try {
                RollbackChanges();
                CancelClickHandler(sender, e);
            } catch (Exception ex) {
                Logger.LogWrite("RepositoryEditDlg.btnCancel_Click(): " + ex.Message);
            }
        }

        public RepositoryEditDlg()
        {
            InitializeComponent();

            btnAccept.Image = UIHelper.LoadResourceImage("Resources.btn_accept.gif");
            btnCancel.Image = UIHelper.LoadResourceImage("Resources.btn_cancel.gif");

            fNotesList = new GKSheetList(pageNotes);

            // SetLang()
            Title = LangMan.LS(LSID.LSID_Repository);
            btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
            btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
            lblName.Text = LangMan.LS(LSID.LSID_Title);
            pageNotes.Text = LangMan.LS(LSID.LSID_RPNotes);
            btnAddress.Text = LangMan.LS(LSID.LSID_Address) + @"...";

            txtName.Focus();
            fController = new RepositoryEditDlgController(this);
        }

        public override void InitDialog(IBaseWindow baseWin)
        {
            base.InitDialog(baseWin);
            fController.Init(baseWin);

            fNotesList.ListModel = new NoteLinksListModel(fBase, fLocalUndoman);
        }
    }
}
