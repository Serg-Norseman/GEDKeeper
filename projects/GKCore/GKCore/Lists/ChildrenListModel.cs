﻿/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2021 by Sergey V. Zhdanovskih.
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
using GKCore.Controllers;
using GKCore.Interfaces;
using GKCore.Operations;
using GKCore.Types;

namespace GKCore.Lists
{
    public abstract class ChildrenListModel : ListModel
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

        protected void FillFamilyChildren(GDMTree tree, GDMFamilyRecord family)
        {
            int idx = 0;
            foreach (GDMIndividualLink ptr in family.Children) {
                idx += 1;
                GDMIndividualRecord child = tree.GetPtrValue(ptr);

                fSheetList.AddItem(child, new object[] {
                        idx, GKUtils.GetNameString(child, true, false),
                        new GDMDateItem(GKUtils.GetBirthDate(child))
                    });
            }
        }
    }


    /// <summary>
    /// 
    /// </summary>
    public sealed class FamilyChildrenListModel : ChildrenListModel
    {
        public FamilyChildrenListModel(IBaseWindow baseWin, ChangeTracker undoman) : base(baseWin, undoman)
        {
        }

        public override void UpdateContents()
        {
            var family = fDataOwner as GDMFamilyRecord;
            if (fSheetList == null || family == null) return;

            try {
                fSheetList.BeginUpdate();
                fSheetList.ClearItems();

                var tree = fBaseWin.Context.Tree;
                FillFamilyChildren(tree, family);

                fSheetList.EndUpdate();
            } catch (Exception ex) {
                Logger.WriteError("FamilyChildrenListModel.UpdateContents()", ex);
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
                    child = fBaseWin.Context.SelectPerson(fBaseWin.Context.Tree.GetPtrValue(family.Husband), TargetMode.tmParent, GDMSex.svUnknown);
                    result = (child != null && fBaseWin.Context.IsAvailableRecord(child));
                    if (result) {
                        result = fUndoman.DoOrdinaryOperation(OperationType.otIndividualParentsAttach, child, family);
                    }
                    break;

                case RecordAction.raEdit:
                    result = (BaseController.ModifyIndividual(fBaseWin, ref child, null, TargetMode.tmNone, GDMSex.svUnknown));
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


    /// <summary>
    /// 
    /// </summary>
    public sealed class IndividualChildrenListModel : ChildrenListModel
    {
        public IndividualChildrenListModel(IBaseWindow baseWin, ChangeTracker undoman) : base(baseWin, undoman)
        {
        }

        public override void UpdateContents()
        {
            var indiRec = fDataOwner as GDMIndividualRecord;
            if (fSheetList == null || indiRec == null)
                return;

            try {
                fSheetList.BeginUpdate();
                fSheetList.ClearItems();

                var tree = fBaseWin.Context.Tree;
                foreach (GDMSpouseToFamilyLink spouseLink in indiRec.SpouseToFamilyLinks) {
                    GDMFamilyRecord family = tree.GetPtrValue<GDMFamilyRecord>(spouseLink);
                    FillFamilyChildren(tree, family);
                }

                fSheetList.EndUpdate();
            } catch (Exception ex) {
                Logger.WriteError("IndividualChildrenListModel.UpdateContents()", ex);
            }
        }

        public override void Modify(object sender, ModifyEventArgs eArgs)
        {
            var indiRec = fDataOwner as GDMIndividualRecord;
            if (fBaseWin == null || fSheetList == null || indiRec == null)
                return;

            GDMIndividualRecord child = eArgs.ItemData as GDMIndividualRecord;
            GDMTree tree = fBaseWin.Context.Tree;

            bool result = false;

            switch (eArgs.Action) {
                case RecordAction.raAdd:
                    GDMFamilyRecord family = fBaseWin.Context.SelectFamily(indiRec, TargetMode.tmFamilySpouse);
                    if (family != null && fBaseWin.Context.IsAvailableRecord(family)) {
                        GDMIndividualRecord target = (indiRec.Sex == GDMSex.svMale) ? indiRec : null;
                        child = fBaseWin.Context.SelectPerson(target, TargetMode.tmParent, GDMSex.svUnknown);
                        result = (child != null && fBaseWin.Context.IsAvailableRecord(child));
                        if (result) {
                            result = fUndoman.DoOrdinaryOperation(OperationType.otIndividualParentsAttach, child, family);
                        }
                    }
                    break;

                case RecordAction.raEdit:
                    result = (BaseController.ModifyIndividual(fBaseWin, ref child, null, TargetMode.tmNone, GDMSex.svUnknown));
                    break;

                case RecordAction.raDelete:
                    result = (child != null && AppHost.StdDialogs.ShowQuestionYN(LangMan.LS(LSID.LSID_DetachChildQuery)));
                    if (result) {
                        GDMFamilyRecord family2 = tree.FindChildFamily(indiRec, child);
                        result = (family2 != null) && fUndoman.DoOrdinaryOperation(OperationType.otIndividualParentsDetach, child, family2);
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
