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
    public sealed class GKSourcesSheet : GKCustomSheet
	{
        public GKSourcesSheet(IBaseEditor baseEditor, Control aOwner) : base(baseEditor, aOwner)
        {
            this.Columns_BeginUpdate();
            this.AddColumn(LangMan.LS(LSID.LSID_Author), 70, false);
            this.AddColumn(LangMan.LS(LSID.LSID_Title), 180, false);
            this.AddColumn(LangMan.LS(LSID.LSID_Page), 90, false);
            this.AddColumn(LangMan.LS(LSID.LSID_Certainty), 220, false);
            this.Columns_EndUpdate();

            this.Buttons = EnumSet.Create(new Enum[]
			{
				GKSheetList.SheetButton.lbAdd, 
				GKSheetList.SheetButton.lbEdit, 
				GKSheetList.SheetButton.lbDelete, 
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

                this.DataList.Reset();
                while (this.DataList.MoveNext()) {
                	TGEDCOMSourceCitation cit = this.DataList.Current as TGEDCOMSourceCitation;
                	TGEDCOMSourceRecord sourceRec = cit.Value as TGEDCOMSourceRecord;

                	if (sourceRec != null)
                	{
                		GKListItem item = this.List.AddItem(sourceRec.Originator.Text.Trim(), cit);
                		item.SubItems.Add(sourceRec.FiledByEntry);
                		item.SubItems.Add(cit.Page);
                		
                		int ca = cit.CertaintyAssessment;
                		item.SubItems.Add(LangMan.LS(GKData.CertaintyAssessments[ca]));
                	}
                }

				this.List.ResizeColumn(1);
            }
            catch (Exception ex)
            {
                SysUtils.LogWrite("GKSourcesSheet.UpdateSheet(): " + ex.Message);
            }
        }

        private void ListModify(object sender, ModifyEventArgs eArgs)
        {
        	if (this.DataList == null) return;

            IBase aBase = this.Editor.Base;
            if (aBase == null) return;

            IGEDCOMStructWithLists _struct = this.DataList.Owner as IGEDCOMStructWithLists;
            TGEDCOMSourceCitation aCit = eArgs.ItemData as TGEDCOMSourceCitation;
            
			bool result = false;

            switch (eArgs.Action)
            {
            	case RecordAction.raAdd:
            	case RecordAction.raEdit:
            		{
            			TfmSourceCitEdit fmSrcCitEdit = new TfmSourceCitEdit(aBase);
            			try
            			{
            				if (eArgs.Action == RecordAction.raAdd)
            				{
            					aCit = new TGEDCOMSourceCitation(aBase.Tree, _struct as GEDCOMObject, "", "");
            				}

            				fmSrcCitEdit.SourceCitation = aCit;
            				DialogResult res = TfmGEDKeeper.Instance.ShowModalEx(fmSrcCitEdit, false);

            				if (eArgs.Action == RecordAction.raAdd)
            				{
            					if (res == DialogResult.OK)
            					{
            						_struct.SourceCitations.Add(aCit);
            					}
            					else
            					{
            						aCit.Dispose();
            					}
            				}

            				result = (res == DialogResult.OK);
            			}
            			finally
            			{
            				fmSrcCitEdit.Dispose();
            			}
            		}
            		break;

            	case RecordAction.raDelete:
            		if (GKUtils.ShowQuestion(LangMan.LS(LSID.LSID_DetachSourceQuery)) != DialogResult.No)
            		{
            			_struct.SourceCitations.DeleteObject(aCit);
            			result = true;
            			aBase.Modified = true;
            		}
            		break;

            	case RecordAction.raMoveUp:
            	case RecordAction.raMoveDown:
            		{
            			int idx = _struct.SourceCitations.IndexOfObject(aCit);

            			switch (eArgs.Action)
            			{
            				case RecordAction.raMoveUp:
            					_struct.SourceCitations.Exchange(idx - 1, idx);
            					break;

            				case RecordAction.raMoveDown:
            					_struct.SourceCitations.Exchange(idx, idx + 1);
            					break;
            			}

            			result = true;
            			aBase.Modified = true;
            		}
            		break;
            }

            if (result) this.UpdateSheet();
        }

    }
}
