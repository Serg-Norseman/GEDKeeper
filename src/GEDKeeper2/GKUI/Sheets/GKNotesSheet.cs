using System;
using System.Windows.Forms;

using ExtUtils;
using GedCom551;
using GKCore;
using GKCore.Interfaces;
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
                this.List.Items.Clear();

                this.DataList.Reset();
                while (this.DataList.MoveNext()) {
                	TGEDCOMNotes note = this.DataList.Current as TGEDCOMNotes;
                	this.List.AddItem(note.Notes.Text.Trim(), note);
                }
            }
            catch (Exception ex)
            {
                SysUtils.LogWrite("GKNotesSheet.UpdateSheet(): " + ex.Message);
            }
        }

        private void ListModify(object sender, ModifyEventArgs eArgs)
        {
        	if (this.DataList == null) return;
        	
            IBase aBase = this.Editor.Base;
            if (aBase == null) return;

            IGEDCOMStructWithLists _struct = this.DataList.Owner as IGEDCOMStructWithLists;
            TGEDCOMNotes aNote = eArgs.ItemData as TGEDCOMNotes;
            
            bool result = false;

            TGEDCOMNoteRecord noteRec;
            switch (eArgs.Action)
            {
                case RecordAction.raAdd:
                    noteRec = aBase.SelectRecord(TGEDCOMRecordType.rtNote, null) as TGEDCOMNoteRecord;
                    if (noteRec != null)
                    {
                        TGEDCOMNotes note = new TGEDCOMNotes(aBase.Tree, _struct as GEDCOMObject, "", "");
                        note.Value = noteRec;
                        _struct.Notes.Add(note);
                        result = true;
                    }
                    break;

                case RecordAction.raEdit:
                    if (aNote != null)
                    {
                        noteRec = aNote.Value as TGEDCOMNoteRecord;
                        result = aBase.ModifyNote(ref noteRec);
                    }
                    break;

                case RecordAction.raDelete:
                    if (GKUtils.ShowQuestion(LangMan.LS(LSID.LSID_DetachNoteQuery)) != DialogResult.No)
                    {
                        _struct.Notes.DeleteObject(aNote);
                        result = true;
                        aBase.Modified = true;
                    }
                    break;
            }
            
            if (result) this.UpdateSheet();
        }

    }
}
