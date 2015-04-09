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
    public sealed class GKMembersSheet : GKCustomSheet
	{
        public GKMembersSheet(IBaseEditor baseEditor, Control aOwner) : base(baseEditor, aOwner)
        {
            this.Columns_BeginUpdate();
            this.List.AddListColumn(LangMan.LS(LSID.LSID_Name), 300, false);
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
                    GEDCOMPointer ptrMember = this.DataList.Current as GEDCOMPointer;

                    GEDCOMIndividualRecord member = ptrMember.Value as GEDCOMIndividualRecord;
                    this.List.AddItem(member.aux_GetNameStr(true, false), member);
                }
            }
            catch (Exception ex)
            {
                SysUtils.LogWrite("GKMembersSheet.UpdateSheet(): " + ex.Message);
            }
        }

        private void ListModify(object sender, ModifyEventArgs eArgs)
        {
        	if (this.DataList == null) return;

            IBase aBase = this.Editor.Base;
            if (aBase == null) return;

            bool result = false;

            GEDCOMGroupRecord groupRecord = this.DataList.Owner as GEDCOMGroupRecord;
            GEDCOMIndividualRecord member = eArgs.ItemData as GEDCOMIndividualRecord;

            switch (eArgs.Action)
            {
                case RecordAction.raAdd:
                    member = aBase.SelectPerson(null, TargetMode.tmNone, GEDCOMSex.svNone);
                    result = (member != null && groupRecord.aux_AddMember(member));
                    break;

                case RecordAction.raDelete:
                    result = (member != null && GKUtils.ShowQuestion(LangMan.LS(LSID.LSID_DetachMemberQuery)) != DialogResult.No && groupRecord.aux_RemoveMember(member));
                    break;
            }

            if (result) this.UpdateSheet();
        }

    }
}
