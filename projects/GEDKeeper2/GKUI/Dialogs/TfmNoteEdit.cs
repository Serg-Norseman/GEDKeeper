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

namespace GKUI.Dialogs
{
    /// <summary>
    /// 
    /// </summary>
    public partial class TfmNoteEdit : Form, IBaseEditor
    {
        private readonly IBaseWindow fBase;
        private GEDCOMNoteRecord fNoteRecord;

        public GEDCOMNoteRecord NoteRecord
        {
            get { return this.fNoteRecord; }
            set { this.SetNoteRecord(value); }
        }

        public IBaseWindow Base
        {
            get { return this.fBase; }
        }

        private void SetNoteRecord(GEDCOMNoteRecord value)
        {
            this.fNoteRecord = value;
            this.mmNote.Text = this.fNoteRecord.Note.Text.Trim();
        }

        private void btnAccept_Click(object sender, EventArgs e)
        {
            try
            {
                this.fNoteRecord.SetNotesArray(this.mmNote.Lines);
                this.fBase.ChangeRecord(this.fNoteRecord);
                base.DialogResult = DialogResult.OK;
            }
            catch (Exception ex)
            {
                this.fBase.Host.LogWrite("TfmNoteEdit.btnAccept_Click(): " + ex.Message);
                base.DialogResult = DialogResult.None;
            }
        }

        public TfmNoteEdit(IBaseWindow aBase)
        {
            this.InitializeComponent();
            this.fBase = aBase;

            // SetLang()
            this.btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
            this.btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
            this.Text = LangMan.LS(LSID.LSID_Note);
        }
    }
}
