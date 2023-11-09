/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2022 by Sergey V. Zhdanovskih.
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
using System.Threading.Tasks;
using BSLib;
using GDModel;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Interfaces;
using GKCore.Operations;
using GKCore.Types;

namespace GKCore.Lists
{
    public sealed class NoteLinksListModel : SheetModel<GDMNotes>
    {
        private GDMLines fNoteLines;

        public NoteLinksListModel(IView owner, IBaseWindow baseWin, ChangeTracker undoman) : base(owner, baseWin, undoman)
        {
            AllowedActions = EnumSet<RecordAction>.Create(
                RecordAction.raAdd, RecordAction.raEdit, RecordAction.raDelete,
                RecordAction.raMoveUp, RecordAction.raMoveDown);

            fListColumns.AddColumn(LSID.NumberSym, 25, false);
            fListColumns.AddColumn(LSID.Note, 500, false);
            fListColumns.ResetDefaults();
        }

        public override void Fetch(GDMNotes aRec)
        {
            base.Fetch(aRec);
            fNoteLines = fBaseContext.Tree.GetNoteLines(fFetchedRec);
        }

        protected override object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
        {
            object result = null;
            switch (colType) {
                case 0:
                    result = fStructList.IndexOf(fFetchedRec) + 1;
                    break;
                case 1:
                    result = GKUtils.MergeStrings(fNoteLines, GKData.NOTE_NAME_MAX_LENGTH);
                    break;
            }
            return result;
        }

        public override void UpdateContents()
        {
            var dataOwner = fDataOwner as IGDMStructWithNotes;
            if (dataOwner == null) return;

            try {
                UpdateStructList(dataOwner.Notes);
            } catch (Exception ex) {
                Logger.WriteError("NoteLinksListModel.UpdateContents()", ex);
            }
        }

        public override async Task Modify(object sender, ModifyEventArgs eArgs)
        {
            var dataOwner = fDataOwner as IGDMStructWithNotes;
            if (fBaseWin == null || dataOwner == null) return;

            var notes = eArgs.ItemData as GDMNotes;

            bool result = false;

            GDMNoteRecord noteRec;
            switch (eArgs.Action) {
                case RecordAction.raAdd:
                    noteRec = await fBaseWin.Context.SelectRecord(fOwner, GDMRecordType.rtNote, null) as GDMNoteRecord;
                    if (noteRec != null) {
                        result = fUndoman.DoOrdinaryOperation(OperationType.otRecordNoteAdd, (GDMObject)dataOwner, noteRec);
                        notes = dataOwner.FindNotes(noteRec);
                    }
                    break;

                case RecordAction.raEdit:
                    if (notes != null) {
                        noteRec = fBaseContext.Tree.GetPtrValue<GDMNoteRecord>(notes);
                        var noteRes = await BaseController.ModifyNote(fOwner, fBaseWin, noteRec);
                        result = noteRes.Result;
                    }
                    break;

                case RecordAction.raDelete:
                    if (AppHost.StdDialogs.ShowQuestion(LangMan.LS(LSID.DetachNoteQuery))) {
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
                if (eArgs.Action == RecordAction.raAdd) {
                    eArgs.ItemData = notes;
                }

                fBaseWin.Context.Modified = true;
                eArgs.IsChanged = true;
            }
        }
    }
}
