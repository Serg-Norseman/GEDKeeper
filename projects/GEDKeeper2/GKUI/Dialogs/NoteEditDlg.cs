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

using GKCommon;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKCore.Types;
using GKCore.UIContracts;

namespace GKUI.Dialogs
{
    /// <summary>
    /// 
    /// </summary>
    public sealed partial class NoteEditDlg : EditorDialog, INoteEditDlg
    {
        private GEDCOMNoteRecord fNoteRecord;

        public GEDCOMNoteRecord NoteRecord
        {
            get { return fNoteRecord; }
            set { SetNoteRecord(value); }
        }

        private void SetNoteRecord(GEDCOMNoteRecord value)
        {
            fNoteRecord = value;
            txtNote.Text = fNoteRecord.Note.Text.Trim();
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
                    fNoteRecord.SetNotesArray(txtNote.Lines);

                    fBase.NotifyRecord(fNoteRecord, RecordAction.raEdit);

                    DialogResult = DialogResult.OK;
                }
                else
                {
                    DialogResult = DialogResult.Cancel;
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("NoteEditDlg.btnAccept_Click(): " + ex.Message);
                DialogResult = DialogResult.None;
            }
        }

        public NoteEditDlg()
        {
            InitializeComponent();

            btnAccept.Image = GKResources.iBtnAccept;
            btnCancel.Image = GKResources.iBtnCancel;

            // SetLang()
            btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
            btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
            Text = LangMan.LS(LSID.LSID_Note);
        }
    }
}
