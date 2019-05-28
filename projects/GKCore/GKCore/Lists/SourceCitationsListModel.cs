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
using BSLib;
using GDModel;
using GDModel.Providers.GEDCOM;
using GKCore.Interfaces;
using GKCore.Operations;
using GKCore.Types;

namespace GKCore.Lists
{
    public sealed class SourceCitationsListModel : ListModel
    {
        public SourceCitationsListModel(IBaseWindow baseWin, ChangeTracker undoman) : base(baseWin, undoman)
        {
            AllowedActions = EnumSet<RecordAction>.Create(
                RecordAction.raAdd, RecordAction.raEdit, RecordAction.raDelete,
                RecordAction.raMoveUp, RecordAction.raMoveDown);

            fListColumns.AddColumn(LSID.LSID_Author, 70, false);
            fListColumns.AddColumn(LSID.LSID_Title, 180, false);
            fListColumns.AddColumn(LSID.LSID_Page, 90, false);
            fListColumns.AddColumn(LSID.LSID_Certainty, 220, false);
            fListColumns.ResetDefaults();
        }

        public override void UpdateContents()
        {
            var dataOwner = fDataOwner as IGEDCOMStructWithLists;
            if (fSheetList == null || dataOwner == null) return;

            try {
                fSheetList.ClearItems();

                foreach (GDMSourceCitation cit in dataOwner.SourceCitations) {
                    GDMSourceRecord sourceRec = cit.Value as GDMSourceRecord;
                    if (sourceRec == null) continue;

                    int ca = cit.GetValidCertaintyAssessment();

                    fSheetList.AddItem(cit, new object[] { sourceRec.Originator.Text.Trim(),
                        sourceRec.ShortTitle, cit.Page,
                        LangMan.LS(GKData.CertaintyAssessments[ca])
                    });
                }

                fSheetList.ResizeColumn(1);
            } catch (Exception ex) {
                Logger.LogWrite("SourceCitationsListModel.UpdateContents(): " + ex.Message);
            }
        }

        public override void Modify(object sender, ModifyEventArgs eArgs)
        {
            var dataOwner = fDataOwner as IGEDCOMStructWithLists;
            if (fBaseWin == null || fSheetList == null || dataOwner == null) return;

            GDMSourceCitation aCit = eArgs.ItemData as GDMSourceCitation;

            bool result = false;

            switch (eArgs.Action)
            {
                case RecordAction.raAdd:
                case RecordAction.raEdit:
                    result = BaseController.ModifySourceCitation(fBaseWin, fUndoman, dataOwner, ref aCit);
                    break;

                case RecordAction.raDelete:
                    if (AppHost.StdDialogs.ShowQuestionYN(LangMan.LS(LSID.LSID_DetachSourceQuery)))
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

            if (result) {
                fBaseWin.Context.Modified = true;
                eArgs.IsChanged = true;
            }
        }
    }
}
