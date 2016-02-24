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
