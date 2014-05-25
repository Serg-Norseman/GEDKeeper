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
    public sealed class GKUserRefsSheet : GKCustomSheet
	{
        public GKUserRefsSheet(IBaseEditor baseEditor, Control aOwner) : base(baseEditor, aOwner)
        {
            this.Columns_BeginUpdate();
            this.List.AddListColumn(LangMan.LS(LSID.LSID_Reference), 300, false);
            this.List.AddListColumn(LangMan.LS(LSID.LSID_Type), 200, false);
            this.Columns_EndUpdate();

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
                    TGEDCOMUserReference uref = this.DataList.Current as TGEDCOMUserReference;
                    ListViewItem item = this.List.AddItem(uref.StringValue, uref);
                    item.SubItems.Add(uref.ReferenceType);
                }
            }
            catch (Exception ex)
            {
                SysUtils.LogWrite("GKUserRefsSheet.UpdateSheet(): " + ex.Message);
            }
        }

        private void ListModify(object sender, ModifyEventArgs eArgs)
        {
        	if (this.DataList == null) return;

            IBase aBase = this.Editor.Base;
            if (aBase == null) return;

            TGEDCOMRecord record = this.DataList.Owner as TGEDCOMRecord;
            TGEDCOMUserReference userRef = eArgs.ItemData as TGEDCOMUserReference;

            bool result = false;

            switch (eArgs.Action) {
            	case RecordAction.raAdd:
            	case RecordAction.raEdit:
            		TfmUserRefEdit dlg = new TfmUserRefEdit(aBase);
            		try
            		{
            			if (eArgs.Action == RecordAction.raAdd) {
            				userRef = new TGEDCOMUserReference(aBase.Tree, record, "", "");
            			}

            			dlg.UserRef = userRef;
            			DialogResult res = TfmGEDKeeper.Instance.ShowModalEx(dlg, false);

            			if (eArgs.Action == RecordAction.raAdd) {
            				if (res == DialogResult.OK) {
            					record.UserReferences.Add(userRef);
            				} else {
            					userRef.Dispose();
            				}
            			}

            			result = (res == DialogResult.OK);
            		}
            		finally
            		{
            			dlg.Dispose();
            		}
            		break;
            		
            	case RecordAction.raDelete:
            		if (GKUtils.ShowQuestion(LangMan.LS(LSID.LSID_RemoveUserRefQuery)) != DialogResult.No)
            		{
            			record.UserReferences.DeleteObject(userRef);
            			result = true;
            			aBase.Modified = true;
            		}
            		break;
            }
            
            if (result) this.UpdateSheet();
        }

    }
}
