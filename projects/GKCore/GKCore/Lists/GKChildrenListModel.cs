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
    /// <summary>
    /// 
    /// </summary>
    public sealed class GKChildrenListModel : GKListModel
    {
        public GKChildrenListModel(IBaseWindow baseWin, ChangeTracker undoman) : base(baseWin, undoman)
        {
        }

        public override void InitView()
        {
            fSheetList.AddColumn("№", 25, false);
            fSheetList.AddColumn(LangMan.LS(LSID.LSID_Name), 300, false);
            fSheetList.AddColumn(LangMan.LS(LSID.LSID_BirthDate), 100, false);
        }

        public override void UpdateContent()
        {
            var family = fDataOwner as GEDCOMFamilyRecord;
            if (fSheetList == null || family == null) return;

            try
            {
                fSheetList.BeginUpdate();
                fSheetList.ClearItems();

                int idx = 0;
                foreach (GEDCOMPointer ptr in family.Children)
                {
                    idx += 1;

                    GEDCOMIndividualRecord child = (GEDCOMIndividualRecord)ptr.Value;

                    IListItem item = fSheetList.AddItem(idx, child);
                    item.AddSubItem(GKUtils.GetNameString(child, true, false));
                    item.AddSubItem(new GEDCOMDateItem(GKUtils.GetBirthDate(child)));
                }

                fSheetList.EndUpdate();
            }
            catch (Exception ex)
            {
                Logger.LogWrite("GKChildrenListModel.UpdateContent(): " + ex.Message);
            }
        }

        public override void Modify(object sender, ModifyEventArgs eArgs)
        {
            var family = fDataOwner as GEDCOMFamilyRecord;
            if (fBaseWin == null || fSheetList == null || family == null) return;

            GEDCOMIndividualRecord child = eArgs.ItemData as GEDCOMIndividualRecord;

            bool result = false;

            switch (eArgs.Action) {
                case RecordAction.raAdd:
                    child = AppHub.BaseController.SelectPerson(fBaseWin, family.GetHusband(), TargetMode.tmParent, GEDCOMSex.svNone);
                    result = (child != null && fBaseWin.Context.IsAvailableRecord(child));
                    if (result) {
                        result = fUndoman.DoOrdinaryOperation(OperationType.otIndividualParentsAttach, child, family);
                    }
                    break;

                case RecordAction.raEdit:
                    result = (AppHub.BaseController.ModifyIndividual(fBaseWin, ref child, null, TargetMode.tmNone, GEDCOMSex.svNone));
                    break;

                case RecordAction.raDelete:
                    result = (child != null && AppHub.StdDialogs.ShowQuestionYN(LangMan.LS(LSID.LSID_DetachChildQuery)) != false);
                    if (result) {
                        result = fUndoman.DoOrdinaryOperation(OperationType.otIndividualParentsDetach, child, family);
                    }
                    break;
            }

            if (result) {
                fBaseWin.Modified = true;
                fSheetList.UpdateSheet();
            }
        }
    }
}
