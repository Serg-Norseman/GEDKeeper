/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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
using System.Windows.Forms;

using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKUI.Contracts;
using GKUI.Sheets;

namespace GKUI.Dialogs
{
    /// <summary>
    /// 
    /// </summary>
    public sealed partial class RepositoryEditDlg : EditorDialog, IRepositoryEditDlg
    {
        private readonly GKSheetList fNotesList;

        private GEDCOMRepositoryRecord fRepository;
        
        public GEDCOMRepositoryRecord Repository
        {
            get { return fRepository; }
            set { SetRepository(value); }
        }

        private void SetRepository(GEDCOMRepositoryRecord value)
        {
            fRepository = value;
            txtName.Text = fRepository.RepositoryName;

            fNotesList.ListModel.DataOwner = fRepository;
        }

        private void btnAddress_Click(object sender, EventArgs e)
        {
            AppHub.BaseController.ModifyAddress(fBase, fRepository.Address);
        }

        private void btnAccept_Click(object sender, EventArgs e)
        {
            try
            {
                fRepository.RepositoryName = txtName.Text;
                CommitChanges();
                fBase.ChangeRecord(fRepository);
                DialogResult = DialogResult.OK;
            }
            catch (Exception ex)
            {
                fBase.Host.LogWrite("RepositoryEditDlg.btnAccept_Click(): " + ex.Message);
                DialogResult = DialogResult.None;
            }
        }

        private void btnCancel_Click(object sender, EventArgs e)
        {
            try
            {
                RollbackChanges();
            }
            catch (Exception ex)
            {
                fBase.Host.LogWrite("RepositoryEditDlg.btnCancel_Click(): " + ex.Message);
            }
        }

        public RepositoryEditDlg()
        {
            InitializeComponent();

            btnAccept.Image = GKResources.iBtnAccept;
            btnCancel.Image = GKResources.iBtnCancel;

            fNotesList = new GKSheetList(pageNotes);

            // SetLang()
            Text = LangMan.LS(LSID.LSID_Repository);
            btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
            btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
            lblName.Text = LangMan.LS(LSID.LSID_Title);
            pageNotes.Text = LangMan.LS(LSID.LSID_RPNotes);
            btnAddress.Text = LangMan.LS(LSID.LSID_Address) + @"...";
        }

        public override void InitDialog(IBaseWindow baseWin)
        {
            base.InitDialog(baseWin);

            fNotesList.ListModel = new GKNotesListModel(fBase, fLocalUndoman);
        }

        public override bool ShowModalX()
        {
            return base.ShowModalX();
        }
    }
}
