using System;
using System.Windows.Forms;

using GKCommon;
using GKCommon.GEDCOM;
using GKCommon.GEDCOM.Enums;
using GKCore;
using GKCore.Interfaces;
using GKCore.Types;
using GKUI.Controls;

namespace GKUI.Sheets
{
    public sealed class GKMediaSheet : GKCustomSheet
	{
        public GKMediaSheet(IBaseEditor baseEditor, Control aOwner) : base(baseEditor, aOwner)
        {
            this.Columns_BeginUpdate();
            this.AddColumn(LangMan.LS(LSID.LSID_RPMultimedia), 300, false);
            this.AddColumn(LangMan.LS(LSID.LSID_Type), 300, false);
            this.Columns_EndUpdate();

            this.Buttons = EnumSet<SheetButton>.Create(SheetButton.lbAdd, SheetButton.lbEdit, SheetButton.lbDelete, 
				SheetButton.lbMoveUp, SheetButton.lbMoveDown);
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
                    GEDCOMMultimediaLink mmLink = this.DataList.Current as GEDCOMMultimediaLink;
                    GEDCOMMultimediaRecord mmRec = mmLink.Value as GEDCOMMultimediaRecord;
                    if (mmRec == null) continue;

                    if (mmRec.FileReferences.Count != 0)
                    {
                        GEDCOMFileReferenceWithTitle fileRef = mmRec.FileReferences[0];

                        GKListItem item = this.AddItem(fileRef.Title, mmLink);
                        item.AddSubItem(LangMan.LS(GKData.MediaTypes[(int) fileRef.MediaType]));
                    }
                }
            }
            catch (Exception ex)
            {
                SysUtils.LogWrite("GKMediaSheet.UpdateSheet(): " + ex.Message);
            }
        }

        private void ListModify(object sender, ModifyEventArgs eArgs)
        {
        	if (this.DataList == null) return;

            IBaseWindow aBase = this.Editor.Base;
            if (aBase == null) return;

            IGEDCOMStructWithLists _struct = this.DataList.Owner as IGEDCOMStructWithLists;
            GEDCOMMultimediaLink mmLink = eArgs.ItemData as GEDCOMMultimediaLink;
            
            bool result = false;

            GEDCOMMultimediaRecord mmRec;
            switch (eArgs.Action)
            {
                case RecordAction.raAdd:
                    mmRec = aBase.SelectRecord(GEDCOMRecordType.rtMultimedia, new object[0]) as GEDCOMMultimediaRecord;
                    result = (_struct.AddMultimedia(mmRec) != null);
                    break;

                case RecordAction.raEdit:
                    if (mmLink != null)
                    {
                        mmRec = mmLink.Value as GEDCOMMultimediaRecord;
                        result = aBase.ModifyMedia(ref mmRec);
                    }
                    break;

                case RecordAction.raDelete:
                    if (GKUtils.ShowQuestion(LangMan.LS(LSID.LSID_DetachMultimediaQuery)) != DialogResult.No)
                    {
                        _struct.MultimediaLinks.Delete(mmLink);
                        result = true;
                    }
                    break;

                case RecordAction.raMoveUp:
                case RecordAction.raMoveDown:
                    {
                        int idx = _struct.MultimediaLinks.IndexOf(mmLink);

                        switch (eArgs.Action)
                        {
                            case RecordAction.raMoveUp:
                                _struct.MultimediaLinks.Exchange(idx - 1, idx);
                                break;

                            case RecordAction.raMoveDown:
                                _struct.MultimediaLinks.Exchange(idx, idx + 1);
                                break;
                        }

                        result = true;
                    }
                    break;
            }

            if (result) {
            	aBase.Modified = true;
            	this.UpdateSheet();
            }
        }

    }
}
