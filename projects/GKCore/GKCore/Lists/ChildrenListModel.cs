/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
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
using System.Threading.Tasks;
using BSLib;
using GDModel;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Interfaces;
using GKCore.Operations;
using GKCore.Options;
using GKCore.Types;

namespace GKCore.Lists
{
    public abstract class ChildrenListModel : SheetModel<GDMChildLink>
    {
        private GDMIndividualRecord fChildRec;

        protected ChildrenListModel(IView owner, IBaseWindow baseWin, ChangeTracker undoman) : base(owner, baseWin, undoman, CreateListColumns())
        {
            AllowedActions = EnumSet<RecordAction>.Create(
                RecordAction.raAdd, RecordAction.raEdit, RecordAction.raDelete, RecordAction.raJump, RecordAction.raDetails);
        }

        public static ListColumns CreateListColumns()
        {
            var result = new ListColumns(GKListType.stChildren);

            result.AddColumn(LSID.NumberSym, 25, false);
            result.AddColumn(LSID.GeneralName, 300, false);
            result.AddColumn(LSID.BirthDate, 100, false);

            result.ResetDefaults();
            return result;
        }

        protected override GDMRecord GetReferenceRecord(object itemData)
        {
            var child = itemData as GDMIndividualLink;
            return (child == null) ? null : fBaseContext.Tree.GetPtrValue(child);
        }

        public override void Fetch(GDMChildLink aRec)
        {
            base.Fetch(aRec);
            fChildRec = fBaseContext.Tree.GetPtrValue(fFetchedRec);
        }

        protected override object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
        {
            object result = null;
            switch (colType) {
                case 0:
                    result = fStructList.IndexOf(fFetchedRec) + 1;
                    break;
                case 1:
                    result = GKUtils.GetNameString(fChildRec, false);
                    break;
                case 2:
                    result = new GDMDateItem(GKUtils.GetBirthDate(fChildRec));
                    break;
            }
            return result;
        }
    }


    /// <summary>
    /// 
    /// </summary>
    public sealed class FamilyChildrenListModel : ChildrenListModel
    {
        public FamilyChildrenListModel(IView owner, IBaseWindow baseWin, ChangeTracker undoman) : base(owner, baseWin, undoman)
        {
        }

        public override void UpdateContents()
        {
            var family = fDataOwner as GDMFamilyRecord;
            if (family != null)
                UpdateStructList(family.Children);
        }

        public override async Task Modify(object sender, ModifyEventArgs eArgs)
        {
            var family = fDataOwner as GDMFamilyRecord;
            if (fBaseWin == null || family == null) return;

            GDMTree tree = fBaseWin.Context.Tree;
            GDMIndividualRecord child = tree.GetPtrValue(eArgs.ItemData as GDMIndividualLink);

            bool result = false;

            switch (eArgs.Action) {
                case RecordAction.raAdd:
                    child = await fBaseWin.Context.SelectPerson(fOwner, tree.GetPtrValue(family.Husband), TargetMode.tmParent, GDMSex.svUnknown);
                    result = (child != null && fBaseWin.Context.IsAvailableRecord(child));
                    if (result) {
                        if (family.HasMember(child)) {
                            AppHost.StdDialogs.ShowAlert(LangMan.LS(LSID.InvalidLink));
                            return;
                        }

                        result = fUndoman.DoOrdinaryOperation(OperationType.otIndividualParentsAttach, child, family);
                    }
                    break;

                case RecordAction.raEdit: {
                        var indiRes = await BaseController.ModifyIndividual(fOwner, fBaseWin, child, null, TargetMode.tmNone, GDMSex.svUnknown);
                        result = indiRes.Result;
                    }
                    break;

                case RecordAction.raDelete:
                    result = (child != null && await AppHost.StdDialogs.ShowQuestion(LangMan.LS(LSID.DetachChildQuery)));
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
        private readonly GDMList<GDMChildLink> fTotalChildren;

        public IndividualChildrenListModel(IView owner, IBaseWindow baseWin, ChangeTracker undoman) : base(owner, baseWin, undoman)
        {
            fTotalChildren = new GDMList<GDMChildLink>();
        }

        public override void UpdateContents()
        {
            var indiRec = fDataOwner as GDMIndividualRecord;
            if (indiRec == null) return;

            try {
                fTotalChildren.Clear();

                var tree = fBaseWin.Context.Tree;
                foreach (GDMSpouseToFamilyLink spouseLink in indiRec.SpouseToFamilyLinks) {
                    GDMFamilyRecord family = tree.GetPtrValue<GDMFamilyRecord>(spouseLink);
                    fTotalChildren.AddRange(family.Children);
                }

                UpdateStructList(fTotalChildren);
            } catch (Exception ex) {
                Logger.WriteError("IndividualChildrenListModel.UpdateContents()", ex);
            }
        }

        public override async Task Modify(object sender, ModifyEventArgs eArgs)
        {
            var indiRec = fDataOwner as GDMIndividualRecord;
            if (fBaseWin == null || indiRec == null) return;

            GDMTree tree = fBaseWin.Context.Tree;
            GDMIndividualRecord child = tree.GetPtrValue(eArgs.ItemData as GDMIndividualLink);

            bool result = false;

            switch (eArgs.Action) {
                case RecordAction.raAdd:
                    GDMFamilyRecord family = await fBaseWin.Context.SelectFamily(fOwner, indiRec, TargetMode.tmFamilySpouse);
                    if (family != null && fBaseWin.Context.IsAvailableRecord(family)) {
                        GDMIndividualRecord target = (indiRec.Sex == GDMSex.svMale) ? indiRec : null;
                        child = await fBaseWin.Context.SelectPerson(fOwner, target, TargetMode.tmParent, GDMSex.svUnknown);
                        result = (child != null && fBaseWin.Context.IsAvailableRecord(child));
                        if (result) {
                            if (family.HasMember(child)) {
                                AppHost.StdDialogs.ShowAlert(LangMan.LS(LSID.InvalidLink));
                                return;
                            }

                            result = fUndoman.DoOrdinaryOperation(OperationType.otIndividualParentsAttach, child, family);
                        }
                    }
                    break;

                case RecordAction.raEdit: {
                        var indiRes = await BaseController.ModifyIndividual(fOwner, fBaseWin, child, null, TargetMode.tmNone, GDMSex.svUnknown);
                        result = indiRes.Result;
                    }
                    break;

                case RecordAction.raDelete:
                    result = (child != null && await AppHost.StdDialogs.ShowQuestion(LangMan.LS(LSID.DetachChildQuery)));
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
