using System;
using System.Windows.Forms;

using GKCommon;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKCore.Types;
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
                this.List.Items.Clear();

                this.DataList.Reset();
                while (this.DataList.MoveNext()) {
                    GEDCOMAssociation ast = this.DataList.Current as GEDCOMAssociation;
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

            GEDCOMIndividualRecord iRec = this.DataList.Owner as GEDCOMIndividualRecord;
            GEDCOMAssociation ast = eArgs.ItemData as GEDCOMAssociation;

            bool result = false;

            switch (eArgs.Action)
            {
                case RecordAction.raAdd:
                case RecordAction.raEdit:
                    TfmAssociationEdit fmAstEdit = new TfmAssociationEdit(aBase);
                    try
                    {
                        if (eArgs.Action == RecordAction.raAdd) {
                            ast = new GEDCOMAssociation(aBase.Tree, iRec, "", "");
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
