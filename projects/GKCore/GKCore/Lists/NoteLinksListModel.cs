/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Threading.Tasks;
using BSLib;
using GDModel;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Locales;
using GKCore.Operations;
using GKCore.Options;

namespace GKCore.Lists
{
    public sealed class NoteLinksListModel : SheetModel<GDMNotes>
    {
        private GDMLines fNoteLines;

        public NoteLinksListModel(IView owner, IBaseWindow baseWin, ChangeTracker undoman) : base(owner, baseWin, undoman, CreateListColumns())
        {
            AllowedActions = EnumSet<RecordAction>.Create(
                RecordAction.raAdd, RecordAction.raEdit, RecordAction.raDelete,
                RecordAction.raMoveUp, RecordAction.raMoveDown,
                RecordAction.raCopy, RecordAction.raPaste);
        }

        public static ListColumns CreateListColumns()
        {
            var result = new ListColumns(GKListType.stNoteLinks);
            result.AddColumn(LSID.NumberSym, 25, false);
            result.AddColumn(LSID.Note, 500, false);
            return result;
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
            if (fDataOwner is IGDMStructWithNotes dataOwner)
                UpdateStructList(dataOwner.Notes);
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
                    noteRec = await BaseController.SelectRecord(fOwner, fBaseWin, GDMRecordType.rtNote, null) as GDMNoteRecord;
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
                    if (await AppHost.StdDialogs.ShowQuestion(LangMan.LS(LSID.DetachNoteQuery))) {
                        result = fUndoman.DoOrdinaryOperation(OperationType.otRecordNoteRemove, (GDMObject)dataOwner, notes);
                    }
                    break;

                case RecordAction.raMoveUp:
                case RecordAction.raMoveDown:
                    result = Exchange(dataOwner.Notes, notes, eArgs.Action);
                    break;

                case RecordAction.raCopy:
                    noteRec = fBaseContext.Tree.GetPtrValue<GDMNoteRecord>(notes);
                    AppHost.Instance.SetClipboardObj<GDMNoteRecord>(noteRec);
                    break;

                case RecordAction.raPaste:
                    noteRec = AppHost.Instance.GetClipboardObj<GDMNoteRecord>();
                    if (noteRec != null) {
                        result = fUndoman.DoOrdinaryOperation(OperationType.otRecordNoteAdd, fDataOwner, noteRec);
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
