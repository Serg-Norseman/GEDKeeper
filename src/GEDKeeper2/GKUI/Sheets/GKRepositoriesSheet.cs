using System;
using System.Windows.Forms;

using ExtUtils;
using GKCommon;
using GKCommon.GEDCOM;
using GKCommon.GEDCOM.Enums;
using GKCore;
using GKCore.Interfaces;
using GKCore.Types;
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

            this.Buttons = EnumSet<GKSheetList.SheetButton>.Create(
				GKSheetList.SheetButton.lbAdd, 
				GKSheetList.SheetButton.lbDelete, 
				GKSheetList.SheetButton.lbJump
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
                    GEDCOMRepositoryCitation repCit = this.DataList.Current as GEDCOMRepositoryCitation;
                    GEDCOMRepositoryRecord rep = repCit.Value as GEDCOMRepositoryRecord;
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

            GEDCOMSourceRecord sourceRecord = this.DataList.Owner as GEDCOMSourceRecord;
            GEDCOMRepositoryCitation cit = eArgs.ItemData as GEDCOMRepositoryCitation;

            bool result = false;

            switch (eArgs.Action)
            {
                case RecordAction.raAdd:
                    GEDCOMRepositoryRecord rep = aBase.SelectRecord(GEDCOMRecordType.rtRepository, null) as GEDCOMRepositoryRecord;
                    if (rep != null) {
                        sourceRecord.AddRepository(rep);
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
