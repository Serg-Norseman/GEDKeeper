using System;
using System.Windows.Forms;

using ExtUtils;
using GedCom551;
using GKCore;
using GKCore.Interfaces;
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

            this.Buttons = EnumSet<GKSheetList.SheetButton>.Create(
				GKSheetList.SheetButton.lbAdd, 
				GKSheetList.SheetButton.lbEdit, 
				GKSheetList.SheetButton.lbDelete, 
				GKSheetList.SheetButton.lbMoveUp, 
				GKSheetList.SheetButton.lbMoveDown
			);

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
                    TGEDCOMMultimediaLink mmLink = this.DataList.Current as TGEDCOMMultimediaLink;
                    TGEDCOMMultimediaRecord mmRec = mmLink.Value as TGEDCOMMultimediaRecord;
                    if (mmRec == null) continue;

                    if (mmRec.FileReferences.Count != 0)
                    {
                        TGEDCOMFileReferenceWithTitle fileRef = mmRec.FileReferences[0];

                        GKListItem item = this.List.AddItem(fileRef.Title, mmLink);
                        item.SubItems.Add(LangMan.LS(GKData.MediaTypes[(int) fileRef.MediaType]));
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

            IBase aBase = this.Editor.Base;
            if (aBase == null) return;

            IGEDCOMStructWithLists _struct = this.DataList.Owner as IGEDCOMStructWithLists;
            TGEDCOMMultimediaLink mmLink = eArgs.ItemData as TGEDCOMMultimediaLink;
            
            bool result = false;

            TGEDCOMMultimediaRecord mmRec;
            switch (eArgs.Action)
            {
                case RecordAction.raAdd:
                    mmRec = aBase.SelectRecord(TGEDCOMRecordType.rtMultimedia, new object[0]) as TGEDCOMMultimediaRecord;
                    result = (_struct.aux_AddMultimedia(mmRec) != null);
                    break;

                case RecordAction.raEdit:
                    if (mmLink != null)
                    {
                        mmRec = mmLink.Value as TGEDCOMMultimediaRecord;
                        result = aBase.ModifyMedia(ref mmRec);
                    }
                    break;

                case RecordAction.raDelete:
                    if (GKUtils.ShowQuestion(LangMan.LS(LSID.LSID_DetachMultimediaQuery)) != DialogResult.No)
                    {
                        _struct.MultimediaLinks.DeleteObject(mmLink);
                        result = true;
                    }
                    break;

                case RecordAction.raMoveUp:
                case RecordAction.raMoveDown:
                    {
                        int idx = _struct.MultimediaLinks.IndexOfObject(mmLink);

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

            if (result) aBase.Modified = true;
            if (result) this.UpdateSheet();
        }

    }
}
