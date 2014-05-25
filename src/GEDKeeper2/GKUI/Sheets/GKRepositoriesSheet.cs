using System;
using System.Windows.Forms;

using ExtUtils;
using GedCom551;
using GKCore;
using GKCore.Interfaces;
using GKUI.Controls;

namespace GKUI.Sheets
{
    public sealed class GKRepositoriesSheet : GKCustomSheet
	{
        public GKRepositoriesSheet(IBaseEditor baseEditor, Control aOwner) : base(baseEditor, aOwner)
        {
            this.Columns_BeginUpdate();
            this.List.AddListColumn(LangMan.LS(LSID.LSID_Repository), 300, false);
            this.Columns_EndUpdate();

            this.Buttons = EnumSet.Create(new Enum[]
			{
				GKSheetList.SheetButton.lbAdd, 
				GKSheetList.SheetButton.lbDelete, 
				GKSheetList.SheetButton.lbJump
			});

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
                    TGEDCOMRepositoryCitation repCit = this.DataList.Current as TGEDCOMRepositoryCitation;
                    TGEDCOMRepositoryRecord rep = repCit.Value as TGEDCOMRepositoryRecord;
                    this.List.AddItem(rep.RepositoryName, repCit);
                }
            }
            catch (Exception ex)
            {
                SysUtils.LogWrite("GKRepositoriesSheet.UpdateSheet(): " + ex.Message);
            }
        }

        private void ListModify(object sender, ModifyEventArgs eArgs)
        {
        	if (this.DataList == null) return;

            IBase aBase = this.Editor.Base;
            if (aBase == null) return;

            TGEDCOMSourceRecord sourceRecord = this.DataList.Owner as TGEDCOMSourceRecord;
            TGEDCOMRepositoryCitation cit = eArgs.ItemData as TGEDCOMRepositoryCitation;

            bool result = false;

            switch (eArgs.Action)
            {
                case RecordAction.raAdd:
                    TGEDCOMRepositoryRecord rep = aBase.SelectRecord(TGEDCOMRecordType.rtRepository, null) as TGEDCOMRepositoryRecord;
                    if (rep != null) {
                        sourceRecord.aux_AddRepository(rep);
                        result = true;
                    }
                    break;

                case RecordAction.raDelete:
                    if (cit != null && GKUtils.ShowQuestion(LangMan.LS(LSID.LSID_DetachRepositoryQuery)) != DialogResult.No) {
                        sourceRecord.RepositoryCitations.DeleteObject(cit);
                        result = true;
                    }
                    break;
            }

            if (result) this.UpdateSheet();
        }

    }
}
