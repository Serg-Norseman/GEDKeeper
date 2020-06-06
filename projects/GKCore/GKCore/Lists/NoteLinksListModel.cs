/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2020 by Sergey V. Zhdanovskih.
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
using BSLib;
using GDModel;
using GKCore.Controllers;
using GKCore.Interfaces;
using GKCore.Operations;
using GKCore.Types;

namespace GKCore.Lists
{
    public sealed class NoteLinksListModel : ListModel
    {
        public NoteLinksListModel(IBaseWindow baseWin, ChangeTracker undoman) : base(baseWin, undoman)
        {
            AllowedActions = EnumSet<RecordAction>.Create(
                RecordAction.raAdd, RecordAction.raEdit, RecordAction.raDelete,
                RecordAction.raMoveUp, RecordAction.raMoveDown);

            fListColumns.AddColumn(LSID.LSID_Note, 500, false);
            fListColumns.ResetDefaults();
        }

        public override void UpdateContents()
        {
            var dataOwner = fDataOwner as IGEDCOMStructWithLists;
            if (fSheetList == null || dataOwner == null) return;

            try {
                fSheetList.ClearItems();

                foreach (GDMNotes note in dataOwner.Notes) {
                    fSheetList.AddItem(note, new object[] { note.Lines.Text.Trim() });
                }
            } catch (Exception ex) {
                Logger.WriteError("NoteLinksListModel.UpdateContents(): ", ex);
            }
        }

        public override void Modify(object sender, ModifyEventArgs eArgs)
        {
            var dataOwner = fDataOwner as IGEDCOMStructWithLists;
            if (fBaseWin == null || fSheetList == null || dataOwner == null) return;

            GDMNotes notes = eArgs.ItemData as GDMNotes;

            bool result = false;

            GDMNoteRecord noteRec;
            switch (eArgs.Action) {
                case RecordAction.raAdd:
                    noteRec = fBaseWin.Context.SelectRecord(GDMRecordType.rtNote, null) as GDMNoteRecord;
                    if (noteRec != null) {
                        result = fUndoman.DoOrdinaryOperation(OperationType.otRecordNoteAdd, (GDMObject)dataOwner, noteRec);
                    }
                    break;

                case RecordAction.raEdit:
                    if (notes != null) {
                        noteRec = notes.Value as GDMNoteRecord;
                        result = BaseController.ModifyNote(fBaseWin, ref noteRec);
                    }
                    break;

                case RecordAction.raDelete:
                    if (AppHost.StdDialogs.ShowQuestionYN(LangMan.LS(LSID.LSID_DetachNoteQuery))) {
                        result = fUndoman.DoOrdinaryOperation(OperationType.otRecordNoteRemove, (GDMObject)dataOwner, notes);
                    }
                    break;

                case RecordAction.raMoveUp:
                case RecordAction.raMoveDown:
                    {
                        int idx = dataOwner.Notes.IndexOf(notes);
                        switch (eArgs.Action) {
                            case RecordAction.raMoveUp:
                                dataOwner.Notes.Exchange(idx - 1, idx);
                                break;

                            case RecordAction.raMoveDown:
                                dataOwner.Notes.Exchange(idx, idx + 1);
                                break;
                        }
                        result = true;
                    }
                    break;
            }

            if (result) {
                fBaseWin.Context.Modified = true;
                eArgs.IsChanged = true;
            }
        }
    }
}
