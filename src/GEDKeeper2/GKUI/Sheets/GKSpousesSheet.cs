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
    public sealed class GKSpousesSheet : GKCustomSheet
	{
        public GKSpousesSheet(IBaseEditor baseEditor, Control aOwner) : base(baseEditor, aOwner)
        {
            this.Columns_BeginUpdate();
            this.List.AddListColumn("№", 25, false);
            this.List.AddListColumn(LangMan.LS(LSID.LSID_Spouse), 300, false);
            this.List.AddListColumn(LangMan.LS(LSID.LSID_MarriageDate), 100, false);
            this.Columns_EndUpdate();

            this.Buttons = EnumSet<SheetButton>.Create(
				SheetButton.lbAdd, 
				SheetButton.lbEdit, 
				SheetButton.lbDelete, 
				SheetButton.lbJump, 
				SheetButton.lbMoveUp, 
				SheetButton.lbMoveDown
			);

            this.OnModify += this.ListModify;
        }

        public override void UpdateSheet()
        {
        	if (this.DataList == null) return;
        	
            try
            {
                this.List.Items.Clear();

                GEDCOMIndividualRecord person = this.DataList.Owner as GEDCOMIndividualRecord;

                int idx = 0;
                this.DataList.Reset();
                while (this.DataList.MoveNext()) {
                    GEDCOMSpouseToFamilyLink spLink = this.DataList.Current as GEDCOMSpouseToFamilyLink;
                    idx += 1;

                    GEDCOMFamilyRecord family = spLink.Family;
                    if (family != null)
                    {
                        GEDCOMIndividualRecord relPerson;
                        string relName;

                        if (person.Sex == GEDCOMSex.svMale)
                        {
                            relPerson = (family.Wife.Value as GEDCOMIndividualRecord);
                            relName = LangMan.LS(LSID.LSID_UnkFemale);
                        }
                        else
                        {
                            relPerson = (family.Husband.Value as GEDCOMIndividualRecord);
                            relName = LangMan.LS(LSID.LSID_UnkMale);
                        }

                        if (relPerson != null)
                        {
                            relName = relPerson.aux_GetNameStr(true, false);
                        }

                        ListViewItem item = this.List.AddItem(idx.ToString(), family);
                        item.SubItems.Add(relName);
                        item.SubItems.Add(GKUtils.GetMarriageDate(family, TfmGEDKeeper.Instance.Options.DefDateFormat));
                    }

                }
            }
            catch (Exception ex)
            {
                SysUtils.LogWrite("GKSpousesSheet.UpdateSheet(): " + ex.Message);
            }
        }

        private void ListModify(object sender, ModifyEventArgs eArgs)
        {
        	if (this.DataList == null) return;

            IBase aBase = this.Editor.Base;
            if (aBase == null) return;

            bool result = false;

            GEDCOMIndividualRecord person = this.DataList.Owner as GEDCOMIndividualRecord;
            GEDCOMFamilyRecord family = eArgs.ItemData as GEDCOMFamilyRecord;

            switch (eArgs.Action)
            {
                case RecordAction.raAdd:
                    result = (aBase.ModifyFamily(ref family, FamilyTarget.ftSpouse, person));
                    if (result) eArgs.ItemData = family;
                    break;

                case RecordAction.raEdit:
                    result = (aBase.ModifyFamily(ref family, FamilyTarget.ftNone, null));
                    break;

                case RecordAction.raDelete:
                    if (family != null && GKUtils.ShowQuestion(LangMan.LS(LSID.LSID_DetachSpouseQuery)) != DialogResult.No)
                    {
                        family.aux_RemoveSpouse(person);
                        result = true;
                    }
                    break;

                case RecordAction.raMoveUp:
                case RecordAction.raMoveDown:
                    {
                        int idx = person.IndexOfSpouse(family);

                        switch (eArgs.Action)
                        {
                            case RecordAction.raMoveUp:
                                person.ExchangeSpouses(idx - 1, idx);
                                break;

                            case RecordAction.raMoveDown:
                                person.ExchangeSpouses(idx, idx + 1);
                                break;
                        }

                        result = true;
                        break;
                    }
            }

            if (result) this.UpdateSheet();
        }

    }
}
