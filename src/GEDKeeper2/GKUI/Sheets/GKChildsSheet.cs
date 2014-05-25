using System;
using System.Windows.Forms;

using ExtUtils;
using GedCom551;
using GKCore;
using GKCore.Interfaces;
using GKUI.Controls;

namespace GKUI.Sheets
{
    public sealed class GKChildsSheet : GKCustomSheet
	{
        public GKChildsSheet(IBaseEditor baseEditor, Control aOwner) : base(baseEditor, aOwner)
        {
            this.Columns_BeginUpdate();
            this.List.AddListColumn("№", 25, false);
            this.List.AddListColumn(LangMan.LS(LSID.LSID_Name), 300, false);
            this.List.AddListColumn(LangMan.LS(LSID.LSID_BirthDate), 100, false);
            this.Columns_EndUpdate();

            this.Buttons = EnumSet.Create(new Enum[]
			{
				GKSheetList.SheetButton.lbAdd, 
				GKSheetList.SheetButton.lbEdit, 
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
                this.List.SwitchSorter();
                this.List.BeginUpdate();
                this.List.Items.Clear();

                int idx = 0;
                this.DataList.Reset();
                while (this.DataList.MoveNext()) {
                    TGEDCOMPointer ptr = this.DataList.Current as TGEDCOMPointer;
                    idx += 1;

                    TGEDCOMIndividualRecord child = ptr.Value as TGEDCOMIndividualRecord;
                    ListViewItem item = this.List.AddItem(idx.ToString(), child);
                    item.SubItems.Add(child.aux_GetNameStr(true, false));
                    item.SubItems.Add(GKUtils.GetBirthDate(child, TfmGEDKeeper.Instance.Options.DefDateFormat, false));
                }

                this.List.EndUpdate();
                this.List.SwitchSorter();
            }
            catch (Exception ex)
            {
                SysUtils.LogWrite("GKChildsSheet.UpdateSheet(): " + ex.Message);
            }
        }

        private void ListModify(object sender, ModifyEventArgs eArgs)
        {
        	if (this.DataList == null) return;

            IBase aBase = this.Editor.Base;
            if (aBase == null) return;

            bool result = false;

            TGEDCOMFamilyRecord familyRecord = this.DataList.Owner as TGEDCOMFamilyRecord;
            TGEDCOMIndividualRecord child = eArgs.ItemData as TGEDCOMIndividualRecord;

            switch (eArgs.Action)
            {
                case RecordAction.raAdd:
                    child = aBase.SelectPerson(familyRecord.Husband.Value as TGEDCOMIndividualRecord, TargetMode.tmParent, TGEDCOMSex.svNone);
                    result = (child != null && familyRecord.aux_AddChild(child));
                    break;

                case RecordAction.raEdit:
                    result = (aBase.ModifyPerson(ref child));
                    break;

                case RecordAction.raDelete:
                    result = (child != null && GKUtils.ShowQuestion(LangMan.LS(LSID.LSID_DetachChildQuery)) != DialogResult.No && familyRecord.aux_RemoveChild(child));
                    break;
            }

            if (result) this.UpdateSheet();
        }

    }
}
