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

using GKCommon;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKCore.Types;
using GKUI.Controls;

namespace GKUI.Sheets
{
    public sealed class GKNotesSheet : GKCustomSheet
    {
        public GKNotesSheet(IBaseEditor baseEditor, Control aOwner) : base(baseEditor, aOwner)
        {
            this.Columns_BeginUpdate();
            this.AddColumn(LangMan.LS(LSID.LSID_Note), 500, false);
            this.Columns_EndUpdate();

            this.OnModify += this.ListModify;
        }

        public override void UpdateSheet()
        {
            if (this.DataList == null) return;
            
            try
            {
                this.ClearItems();

                this.DataList.Reset();
                while (this.DataList.MoveNext()) {
                    GEDCOMNotes note = this.DataList.Current as GEDCOMNotes;
                    if (note == null) continue;

                    this.AddItem(note.Notes.Text.Trim(), note);
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("GKNotesSheet.UpdateSheet(): " + ex.Message);
            }
        }

        private void ListModify(object sender, ModifyEventArgs eArgs)
        {
            if (this.DataList == null) return;
            
            IBaseWindow aBase = this.Editor.Base;
            if (aBase == null) return;

            IGEDCOMStructWithLists _struct = this.DataList.Owner as IGEDCOMStructWithLists;
            if (_struct == null) return;

            GEDCOMNotes aNote = eArgs.ItemData as GEDCOMNotes;
            
            bool result = false;

            GEDCOMNoteRecord noteRec;
            switch (eArgs.Action)
            {
                case RecordAction.raAdd:
                    noteRec = aBase.SelectRecord(GEDCOMRecordType.rtNote, null) as GEDCOMNoteRecord;
                    if (noteRec != null)
                    {
                        GEDCOMNotes note = new GEDCOMNotes(aBase.Tree, _struct as GEDCOMObject, "", "");
                        note.Value = noteRec;
                        _struct.Notes.Add(note);
                        result = true;
                    }
                    break;

                case RecordAction.raEdit:
                    if (aNote != null)
                    {
                        noteRec = aNote.Value as GEDCOMNoteRecord;
                        result = aBase.ModifyNote(ref noteRec);
                    }
                    break;

                case RecordAction.raDelete:
                    if (GKUtils.ShowQuestion(LangMan.LS(LSID.LSID_DetachNoteQuery)) != DialogResult.No)
                    {
                        _struct.Notes.Delete(aNote);
                        result = true;
                    }
                    break;
            }
            
            if (result)
            {
                aBase.Modified = true;
                this.UpdateSheet();
            }
        }
    }
}
