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

            this.Buttons = EnumSet<SheetButton>.Create(SheetButton.lbAdd, SheetButton.lbEdit, SheetButton.lbDelete, 
				SheetButton.lbMoveUp, SheetButton.lbMoveDown);
            this.OnModify += this.ListModify;
        }

        public override void UpdateSheet()
        {
        	if (this.DataList == null) return;
        	
            try
            {
                this.ClearItems();

                this.DataList.Reset();
                while (this.DataList.MoveNext()) {
                	GEDCOMSourceCitation cit = this.DataList.Current as GEDCOMSourceCitation;
                    if (cit == null) continue;
                	
                    GEDCOMSourceRecord sourceRec = cit.Value as GEDCOMSourceRecord;
                    if (sourceRec == null) continue;

                    GKListItem item = this.AddItem(sourceRec.Originator.Text.Trim(), cit);
                    item.AddSubItem(sourceRec.FiledByEntry);
                    item.AddSubItem(cit.Page);
                    item.AddSubItem(LangMan.LS(GKData.CertaintyAssessments[cit.CertaintyAssessment]));
                }

				this.ResizeColumn(1);
            }
            catch (Exception ex)
            {
                Logger.LogWrite("GKSourcesSheet.UpdateSheet(): " + ex.Message);
            }
        }

        private void ListModify(object sender, ModifyEventArgs eArgs)
        {
        	if (this.DataList == null) return;

            IBaseWindow aBase = this.Editor.Base;
            if (aBase == null) return;

            IGEDCOMStructWithLists _struct = this.DataList.Owner as IGEDCOMStructWithLists;
            if (_struct == null) return;

            GEDCOMSourceCitation aCit = eArgs.ItemData as GEDCOMSourceCitation;
            
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
            					aCit = new GEDCOMSourceCitation(aBase.Tree, _struct as GEDCOMObject, "", "");
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
            			_struct.SourceCitations.Delete(aCit);
            			result = true;
            		}
            		break;

            	case RecordAction.raMoveUp:
            	case RecordAction.raMoveDown:
            		{
            			int idx = _struct.SourceCitations.IndexOf(aCit);

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
            		}
            		break;
            }

            if (result)
            {
            	aBase.Modified = true;
            	this.UpdateSheet();
            }
        }
    }
}
