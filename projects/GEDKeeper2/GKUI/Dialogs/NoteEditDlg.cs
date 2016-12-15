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
    public partial class NoteEditDlg : Form, IBaseEditor
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
            this.txtNote.Text = this.fNoteRecord.Note.Text.Trim();
        }

        private void btnAccept_Click(object sender, EventArgs e)
        {
            try
            {
                int length = 0;
                for (int it = 0; txtNote.Lines.Length > it; ++it)
                {
                    length += txtNote.Lines[it].Trim().Length;
                }
                if (0 != length)
                {
                    this.fNoteRecord.SetNotesArray(this.txtNote.Lines);
                    this.fBase.ChangeRecord(this.fNoteRecord);
                    base.DialogResult = DialogResult.OK;
                }
                else
                {
                    base.DialogResult = DialogResult.Cancel;
                }
            }
            catch (Exception ex)
            {
                this.fBase.Host.LogWrite("NoteEditDlg.btnAccept_Click(): " + ex.Message);
                base.DialogResult = DialogResult.None;
            }
        }

        public NoteEditDlg(IBaseWindow aBase)
        {
            this.InitializeComponent();

            this.btnAccept.Image = GKResources.iBtnAccept;
            this.btnCancel.Image = GKResources.iBtnCancel;

            this.fBase = aBase;

            // SetLang()
            this.btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
            this.btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
            this.Text = LangMan.LS(LSID.LSID_Note);
        }
    }
}
