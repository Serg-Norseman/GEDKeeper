using System;
using System.Windows.Forms;

using ExtUtils;
using GedCom551;
using GKCore;
using GKCore.Interfaces;
using GKUI.Controls;
using GKUI.Dialogs;

namespace GKUI.Sheets
{
    public sealed class GKAssociationsSheet : GKCustomSheet
	{
        public GKAssociationsSheet(IBaseEditor baseEditor, Control aOwner) : base(baseEditor, aOwner)
        {
            this.Columns_BeginUpdate();
            this.List.AddListColumn(LangMan.LS(LSID.LSID_Relation), 300, false);
            this.List.AddListColumn(LangMan.LS(LSID.LSID_Person), 200, false);
            this.Columns_EndUpdate();

            this.Buttons = EnumSet<GKSheetList.SheetButton>.Create(
				GKSheetList.SheetButton.lbAdd, 
				GKSheetList.SheetButton.lbEdit, 
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
                    TGEDCOMAssociation ast = this.DataList.Current as TGEDCOMAssociation;
                    if (ast == null) continue;

                    string nm = ((ast.Individual == null) ? "" : ast.Individual.aux_GetNameStr(true, false));

                    GKListItem item = this.List.AddItem(ast.Relation, ast);
                    item.SubItems.Add(nm);
                }
            }
            catch (Exception ex)
            {
                SysUtils.LogWrite("GKAssociationsSheet.UpdateSheet(): " + ex.Message);
            }
        }

        private void ListModify(object sender, ModifyEventArgs eArgs)
        {
        	if (this.DataList == null) return;

            IBase aBase = this.Editor.Base;
            if (aBase == null) return;

            TGEDCOMIndividualRecord iRec = this.DataList.Owner as TGEDCOMIndividualRecord;
            TGEDCOMAssociation ast = eArgs.ItemData as TGEDCOMAssociation;

            bool result = false;

            switch (eArgs.Action)
            {
                case RecordAction.raAdd:
                case RecordAction.raEdit:
                    TfmAssociationEdit fmAstEdit = new TfmAssociationEdit(aBase);
                    try
                    {
                        if (eArgs.Action == RecordAction.raAdd) {
                            ast = new TGEDCOMAssociation(aBase.Tree, iRec, "", "");
                        }

                        fmAstEdit.Association = ast;
                        DialogResult res = TfmGEDKeeper.Instance.ShowModalEx(fmAstEdit, false);
                    
                        if (eArgs.Action == RecordAction.raAdd) {
                            if (res == DialogResult.OK) {
                                iRec.Associations.Add(ast);
                            } else {
                                ast.Dispose();
                            }
                        }

                        result = (res == DialogResult.OK);
                    }
                    finally
                    {
                        fmAstEdit.Dispose();
                    }
                    break;

                case RecordAction.raDelete:
                    if (GKUtils.ShowQuestion(LangMan.LS(LSID.LSID_RemoveAssociationQuery)) != DialogResult.No)
                    {
                        iRec.Associations.DeleteObject(ast);
                        result = true;
                        aBase.Modified = true;
                    }
                    break;
            }

            if (result) this.UpdateSheet();
        }

    }
}
