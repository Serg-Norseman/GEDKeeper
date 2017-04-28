/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
 *
 *  This file is part of "GEDKeeper".
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

using System;
using GKCommon;
using GKCommon.GEDCOM;
using GKCore.Interfaces;
using GKCore.Operations;
using GKCore.Types;

namespace GKCore.Lists
{
    public sealed class SourceCitationsListModel : ListModel
    {
        public SourceCitationsListModel(IBaseWindow baseWin, ChangeTracker undoman) : base(baseWin, undoman)
        {
        }

        public override void InitView()
        {
            fSheetList.AddColumn(LangMan.LS(LSID.LSID_Author), 70, false);
            fSheetList.AddColumn(LangMan.LS(LSID.LSID_Title), 180, false);
            fSheetList.AddColumn(LangMan.LS(LSID.LSID_Page), 90, false);
            fSheetList.AddColumn(LangMan.LS(LSID.LSID_Certainty), 220, false);

            fSheetList.Buttons = EnumSet<SheetButton>.Create(SheetButton.lbAdd, SheetButton.lbEdit, SheetButton.lbDelete,
                                                             SheetButton.lbMoveUp, SheetButton.lbMoveDown);
        }

        public override void UpdateContent()
        {
            var dataOwner = fDataOwner as IGEDCOMStructWithLists;
            if (fSheetList == null || dataOwner == null) return;

            try
            {
                fSheetList.ClearItems();

                foreach (GEDCOMSourceCitation cit in dataOwner.SourceCitations)
                {
                    GEDCOMSourceRecord sourceRec = cit.Value as GEDCOMSourceRecord;
                    if (sourceRec == null) continue;

                    IListItem item = fSheetList.AddItem(sourceRec.Originator.Text.Trim(), cit);
                    item.AddSubItem(sourceRec.FiledByEntry);
                    item.AddSubItem(cit.Page);
                    item.AddSubItem(LangMan.LS(GKData.CertaintyAssessments[cit.CertaintyAssessment]));
                }

                fSheetList.ResizeColumn(1);
            }
            catch (Exception ex)
            {
                Logger.LogWrite("GKSourcesSheet.UpdateSheet(): " + ex.Message);
            }
        }

        public override void Modify(object sender, ModifyEventArgs eArgs)
        {
            var dataOwner = fDataOwner as IGEDCOMStructWithLists;
            if (fBaseWin == null || fSheetList == null || dataOwner == null) return;

            GEDCOMSourceCitation aCit = eArgs.ItemData as GEDCOMSourceCitation;

            bool result = false;

            switch (eArgs.Action)
            {
                case RecordAction.raAdd:
                case RecordAction.raEdit:
                    result = BaseController.ModifySourceCitation(fBaseWin, fUndoman, dataOwner, ref aCit);
                    break;

                case RecordAction.raDelete:
                    if (AppHost.StdDialogs.ShowQuestionYN(LangMan.LS(LSID.LSID_DetachSourceQuery)) != false)
                    {
                        result = fUndoman.DoOrdinaryOperation(OperationType.otRecordSourceCitRemove, fDataOwner, aCit);
                    }
                    break;

                case RecordAction.raMoveUp:
                case RecordAction.raMoveDown:
                    {
                        int idx = dataOwner.SourceCitations.IndexOf(aCit);

                        switch (eArgs.Action)
                        {
                            case RecordAction.raMoveUp:
                                dataOwner.SourceCitations.Exchange(idx - 1, idx);
                                break;

                            case RecordAction.raMoveDown:
                                dataOwner.SourceCitations.Exchange(idx, idx + 1);
                                break;
                        }

                        result = true;
                    }
                    break;
            }

            if (result)
            {
                fBaseWin.Modified = true;
                UpdateContent();
            }
        }
    }
}
