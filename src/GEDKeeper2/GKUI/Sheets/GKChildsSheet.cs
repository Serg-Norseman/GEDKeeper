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
    public sealed class GKChildsSheet : GKCustomSheet
	{
        public GKChildsSheet(IBaseEditor baseEditor, Control aOwner) : base(baseEditor, aOwner)
        {
            this.Columns_BeginUpdate();
            this.List.AddListColumn("№", 25, false);
            this.List.AddListColumn(LangMan.LS(LSID.LSID_Name), 300, false);
            this.List.AddListColumn(LangMan.LS(LSID.LSID_BirthDate), 100, false);
            this.Columns_EndUpdate();

            this.Buttons = EnumSet<SheetButton>.Create(
				SheetButton.lbAdd, 
				SheetButton.lbEdit, 
				SheetButton.lbDelete, 
				SheetButton.lbJump
			);

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
                    GEDCOMPointer ptr = this.DataList.Current as GEDCOMPointer;
                    idx += 1;

                    GEDCOMIndividualRecord child = ptr.Value as GEDCOMIndividualRecord;
                    ListViewItem item = this.List.AddItem(idx.ToString(), child);
                    item.SubItems.Add(child.GetNameString(true, false));
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

            GEDCOMFamilyRecord familyRecord = this.DataList.Owner as GEDCOMFamilyRecord;
            GEDCOMIndividualRecord child = eArgs.ItemData as GEDCOMIndividualRecord;

            switch (eArgs.Action)
            {
                case RecordAction.raAdd:
                    child = aBase.SelectPerson(familyRecord.Husband.Value as GEDCOMIndividualRecord, TargetMode.tmParent, GEDCOMSex.svNone);
                    result = (child != null && familyRecord.AddChild(child));
                    break;

                case RecordAction.raEdit:
                    result = (aBase.ModifyPerson(ref child));
                    break;

                case RecordAction.raDelete:
                    result = (child != null && GKUtils.ShowQuestion(LangMan.LS(LSID.LSID_DetachChildQuery)) != DialogResult.No && familyRecord.RemoveChild(child));
                    break;
            }

            if (result) this.UpdateSheet();
        }

    }
}
