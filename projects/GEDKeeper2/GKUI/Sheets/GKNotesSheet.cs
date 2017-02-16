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
using GKCore.Operations;
using GKCore.Types;
using GKUI.Controls;

namespace GKUI.Sheets
{
    public sealed class GKNotesSheet : GKCustomSheet
    {
        public GKNotesSheet(IBaseEditor baseEditor, Control owner, ChangeTracker undoman) : base(baseEditor, owner, undoman)
        {
            Columns_BeginUpdate();
            AddColumn(LangMan.LS(LSID.LSID_Note), 500, false);
            Columns_EndUpdate();

            OnModify += ListModify;
        }

        public override void UpdateSheet()
        {
            if (DataList == null) return;
            
            try
            {
                ClearItems();

                DataList.Reset();
                while (DataList.MoveNext()) {
                    GEDCOMNotes note = DataList.Current as GEDCOMNotes;
                    if (note == null) continue;

                    AddItem(note.Notes.Text.Trim(), note);
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("GKNotesSheet.UpdateSheet(): " + ex.Message);
            }
        }

        private void ListModify(object sender, ModifyEventArgs eArgs)
        {
            if (DataList == null) return;
            
            IBaseWindow baseWin = Editor.Base;
            if (baseWin == null) return;

            IGEDCOMStructWithLists _struct = DataList.Owner as IGEDCOMStructWithLists;
            if (_struct == null) return;

            GEDCOMNotes notes = eArgs.ItemData as GEDCOMNotes;

            bool result = false;

            GEDCOMNoteRecord noteRec;
            switch (eArgs.Action)
            {
                case RecordAction.raAdd:
                    noteRec = baseWin.SelectRecord(GEDCOMRecordType.rtNote, null) as GEDCOMNoteRecord;
                    if (noteRec != null) {
                        //result = (_struct.AddNote(noteRec) != null);
                        result = fUndoman.DoOrdinaryOperation(OperationType.otRecordNoteAdd, (GEDCOMObject)_struct, noteRec);
                    }
                    break;

                case RecordAction.raEdit:
                    if (notes != null)
                    {
                        noteRec = notes.Value as GEDCOMNoteRecord;
                        result = baseWin.ModifyNote(ref noteRec);
                    }
                    break;

                case RecordAction.raDelete:
                    if (GKUtils.ShowQuestion(LangMan.LS(LSID.LSID_DetachNoteQuery)) != DialogResult.No)
                    {
                        //_struct.Notes.Delete(notes);
                        //result = true;
                        result = fUndoman.DoOrdinaryOperation(OperationType.otRecordNoteRemove, (GEDCOMObject)_struct, notes);
                    }
                    break;
            }
            
            if (result)
            {
                baseWin.Modified = true;
                UpdateSheet();
            }
        }
    }
}
