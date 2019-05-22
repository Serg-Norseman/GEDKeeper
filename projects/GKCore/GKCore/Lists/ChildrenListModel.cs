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
using GKCommon.GEDCOM;
using GKCore.Interfaces;
using GKCore.Operations;
using GKCore.Types;

namespace GKCore.Lists
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class ChildrenListModel : ListModel
    {
        public ChildrenListModel(IBaseWindow baseWin, ChangeTracker undoman) : base(baseWin, undoman)
        {
            AllowedActions = EnumSet<RecordAction>.Create(
                RecordAction.raAdd, RecordAction.raEdit, RecordAction.raDelete, RecordAction.raJump);

            fListColumns.AddColumn(LSID.LSID_NumberSym, 25, false);
            fListColumns.AddColumn(LSID.LSID_Name, 300, false);
            fListColumns.AddColumn(LSID.LSID_BirthDate, 100, false);
            fListColumns.ResetDefaults();
        }

        public override void UpdateContents()
        {
            var family = fDataOwner as GDMFamilyRecord;
            if (fSheetList == null || family == null) return;

            try
            {
                fSheetList.BeginUpdate();
                fSheetList.ClearItems();

                int idx = 0;
                foreach (GDMPointer ptr in family.Children)
                {
                    idx += 1;

                    GDMIndividualRecord child = (GDMIndividualRecord)ptr.Value;

                    fSheetList.AddItem(child, new object[] {
                                           idx, GKUtils.GetNameString(child, true, false),
                                           new GEDCOMDateItem(GKUtils.GetBirthDate(child)) });
                }

                fSheetList.EndUpdate();
            }
            catch (Exception ex)
            {
                Logger.LogWrite("ChildrenListModel.UpdateContents(): " + ex.Message);
            }
        }

        public override void Modify(object sender, ModifyEventArgs eArgs)
        {
            var family = fDataOwner as GDMFamilyRecord;
            if (fBaseWin == null || fSheetList == null || family == null) return;

            GDMIndividualRecord child = eArgs.ItemData as GDMIndividualRecord;

            bool result = false;

            switch (eArgs.Action) {
                case RecordAction.raAdd:
                    child = fBaseWin.Context.SelectPerson(family.GetHusband(), TargetMode.tmParent, GEDCOMSex.svNone);
                    result = (child != null && fBaseWin.Context.IsAvailableRecord(child));
                    if (result) {
                        result = fUndoman.DoOrdinaryOperation(OperationType.otIndividualParentsAttach, child, family);
                    }
                    break;

                case RecordAction.raEdit:
                    result = (BaseController.ModifyIndividual(fBaseWin, ref child, null, TargetMode.tmNone, GEDCOMSex.svNone));
                    break;

                case RecordAction.raDelete:
                    result = (child != null && AppHost.StdDialogs.ShowQuestionYN(LangMan.LS(LSID.LSID_DetachChildQuery)));
                    if (result) {
                        result = fUndoman.DoOrdinaryOperation(OperationType.otIndividualParentsDetach, child, family);
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
