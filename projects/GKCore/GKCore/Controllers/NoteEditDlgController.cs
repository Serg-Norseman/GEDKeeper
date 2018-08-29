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
using GKCommon.GEDCOM;
using GKCore.Types;
using GKCore.UIContracts;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class NoteEditDlgController : DialogController<INoteEditDlg>
    {
        private GEDCOMNoteRecord fNoteRecord;

        public GEDCOMNoteRecord NoteRecord
        {
            get { return fNoteRecord; }
            set {
                if (fNoteRecord != value) {
                    fNoteRecord = value;
                    UpdateView();
                }
            }
        }


        public NoteEditDlgController(INoteEditDlg view) : base(view)
        {
        }

        public override bool Accept()
        {
            try {
                string noteText = fView.Note.Text.Trim();
                if (!string.IsNullOrEmpty(noteText)) {
                    fNoteRecord.SetNotesArray(fView.Note.Lines);

                    fBase.NotifyRecord(fNoteRecord, RecordAction.raEdit);

                    return true;
                } else {
                    return false;
                }
            } catch (Exception ex) {
                Logger.LogWrite("NoteEditDlgController.Accept(): " + ex.Message);
                return false;
            }
        }

        public override void UpdateView()
        {
            fView.Note.Text = fNoteRecord.Note.Text.Trim();
        }
    }
}
