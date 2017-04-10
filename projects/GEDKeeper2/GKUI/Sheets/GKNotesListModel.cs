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

using GKCommon;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKCore.Operations;
using GKCore.Types;

namespace GKUI.Sheets
{
    public sealed class GKNotesListModel : GKListModel
    {
        public GKNotesListModel(IBaseWindow baseWin, ChangeTracker undoman) : base(baseWin, undoman)
        {
        }

        public override void InitView()
        {
            fSheetList.AddColumn(LangMan.LS(LSID.LSID_Note), 500, false);
        }

        public override void UpdateContent()
        {
            var dataOwner = fDataOwner as IGEDCOMStructWithLists;
            if (fSheetList == null || dataOwner == null) return;

            try
            {
                fSheetList.ClearItems();

                foreach (GEDCOMNotes note in dataOwner.Notes)
                {
                    fSheetList.AddItem(note.Notes.Text.Trim(), note);
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("GKNotesSheet.UpdateSheet(): " + ex.Message);
            }
        }

        public override void Modify(object sender, ModifyEventArgs eArgs)
        {
            var dataOwner = fDataOwner as IGEDCOMStructWithLists;
            if (fBaseWin == null || fSheetList == null || dataOwner == null) return;

            GEDCOMNotes notes = eArgs.ItemData as GEDCOMNotes;

            bool result = false;

            GEDCOMNoteRecord noteRec;
            switch (eArgs.Action)
            {
                case RecordAction.raAdd:
                    noteRec = AppHub.BaseController.SelectRecord(fBaseWin, GEDCOMRecordType.rtNote, null) as GEDCOMNoteRecord;
                    if (noteRec != null) {
                        result = fUndoman.DoOrdinaryOperation(OperationType.otRecordNoteAdd, (GEDCOMObject)dataOwner, noteRec);
                    }
                    break;

                case RecordAction.raEdit:
                    if (notes != null)
                    {
                        noteRec = notes.Value as GEDCOMNoteRecord;
                        result = AppHub.BaseController.ModifyNote(fBaseWin, ref noteRec);
                    }
                    break;

                case RecordAction.raDelete:
                    if (AppHub.StdDialogs.ShowQuestionYN(LangMan.LS(LSID.LSID_DetachNoteQuery)) != false)
                    {
                        result = fUndoman.DoOrdinaryOperation(OperationType.otRecordNoteRemove, (GEDCOMObject)dataOwner, notes);
                    }
                    break;
            }
            
            if (result)
            {
                fBaseWin.Modified = true;
                fSheetList.UpdateSheet();
            }
        }
    }
}
