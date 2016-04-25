/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
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
using GKUI.Sheets;

namespace GKUI.Dialogs
{
    /// <summary>
    /// 
    /// </summary>
    public partial class RepositoryEditDlg : Form, IBaseEditor
    {
        private readonly IBaseWindow fBase;
        private readonly GKNotesSheet fNotesList;

        private GEDCOMRepositoryRecord fRepository;
        
        public GEDCOMRepositoryRecord Repository
        {
            get { return this.fRepository; }
            set { this.SetRepository(value); }
        }

        public IBaseWindow Base
        {
            get { return this.fBase; }
        }

        private void SetRepository(GEDCOMRepositoryRecord value)
        {
            this.fRepository = value;
            this.edName.Text = this.fRepository.RepositoryName;

            this.fNotesList.DataList = this.fRepository.Notes.GetEnumerator();
        }

        private void btnAddress_Click(object sender, EventArgs e)
        {
            this.fBase.ModifyAddress(this.fRepository.Address);
        }

        private void btnAccept_Click(object sender, EventArgs e)
        {
            try
            {
                this.fRepository.RepositoryName = this.edName.Text;
                this.fBase.ChangeRecord(this.fRepository);
                base.DialogResult = DialogResult.OK;
            }
            catch (Exception ex)
            {
                this.fBase.Host.LogWrite("TfmRepositoryEdit.btnAccept_Click(): " + ex.Message);
                base.DialogResult = DialogResult.None;
            }
        }

        public RepositoryEditDlg(IBaseWindow aBase)
        {
            this.InitializeComponent();
            this.fBase = aBase;

            this.fNotesList = new GKNotesSheet(this, this.SheetNotes);

            // SetLang()
            this.Text = LangMan.LS(LSID.LSID_Repository);
            this.btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
            this.btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
            this.Label1.Text = LangMan.LS(LSID.LSID_Title);
            this.SheetNotes.Text = LangMan.LS(LSID.LSID_RPNotes);
            this.btnAddress.Text = LangMan.LS(LSID.LSID_Address) + @"...";
        }
    }
}
