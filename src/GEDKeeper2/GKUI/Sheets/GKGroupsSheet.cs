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
    public sealed class GKGroupsSheet : GKCustomSheet
	{
        public GKGroupsSheet(IBaseEditor baseEditor, Control aOwner) : base(baseEditor, aOwner)
        {
            this.Columns_BeginUpdate();
            this.List.AddListColumn(LangMan.LS(LSID.LSID_Group), 350, false);
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
                    GEDCOMPointer ptr = this.DataList.Current as GEDCOMPointer;
                    GEDCOMGroupRecord grp = ptr.Value as GEDCOMGroupRecord;
                    if (grp != null)
                    {
                        this.List.AddItem(grp.GroupName, grp);
                    }
                }
            }
            catch (Exception ex)
            {
                SysUtils.LogWrite("GKGroupsSheet.UpdateSheet(): " + ex.Message);
            }
        }

        private void ListModify(object sender, ModifyEventArgs eArgs)
        {
        	if (this.DataList == null) return;

            IBase aBase = this.Editor.Base;
            if (aBase == null) return;

            GEDCOMObject dataOwner = this.DataList.Owner;
            GEDCOMIndividualRecord iRec = dataOwner as GEDCOMIndividualRecord;
            GEDCOMGroupRecord groupRec = eArgs.ItemData as GEDCOMGroupRecord;

            bool result = false;

            switch (eArgs.Action)
            {
                case RecordAction.raAdd:
                    groupRec = aBase.SelectRecord(GEDCOMRecordType.rtGroup, null) as GEDCOMGroupRecord;
                    result = (groupRec != null && groupRec.aux_AddMember(iRec));
                    break;

                case RecordAction.raDelete:
                    result = (GKUtils.ShowQuestion(LangMan.LS(LSID.LSID_DetachGroupQuery)) != DialogResult.No && groupRec.aux_RemoveMember(iRec));
                    break;
            }

            if (result) this.UpdateSheet();
        }

    }
}
