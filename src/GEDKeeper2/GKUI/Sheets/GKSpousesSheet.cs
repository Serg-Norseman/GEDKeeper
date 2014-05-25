using System;
using System.Windows.Forms;

using ExtUtils;
using GedCom551;
using GKCore;
using GKCore.Interfaces;
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

            this.Buttons = EnumSet.Create(new Enum[]
			{
				GKSheetList.SheetButton.lbAdd, 
				GKSheetList.SheetButton.lbEdit, 
				GKSheetList.SheetButton.lbDelete, 
				GKSheetList.SheetButton.lbJump, 
				GKSheetList.SheetButton.lbMoveUp, 
				GKSheetList.SheetButton.lbMoveDown
			});

            this.OnModify += this.ListModify;
        }

        public override void UpdateSheet()
        {
        	if (this.DataList == null) return;
        	
            try
            {
                this.List.Items.Clear();

                TGEDCOMIndividualRecord person = this.DataList.Owner as TGEDCOMIndividualRecord;

                int idx = 0;
                this.DataList.Reset();
                while (this.DataList.MoveNext()) {
                    TGEDCOMSpouseToFamilyLink spLink = this.DataList.Current as TGEDCOMSpouseToFamilyLink;
                    idx += 1;

                    TGEDCOMFamilyRecord family = spLink.Family;
                    if (family != null)
                    {
                        TGEDCOMIndividualRecord relPerson;
                        string relName;

                        if (person.Sex == TGEDCOMSex.svMale)
                        {
                            relPerson = (family.Wife.Value as TGEDCOMIndividualRecord);
                            relName = LangMan.LS(LSID.LSID_UnkFemale);
                        }
                        else
                        {
                            relPerson = (family.Husband.Value as TGEDCOMIndividualRecord);
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

            TGEDCOMIndividualRecord person = this.DataList.Owner as TGEDCOMIndividualRecord;
            TGEDCOMFamilyRecord family = eArgs.ItemData as TGEDCOMFamilyRecord;

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
