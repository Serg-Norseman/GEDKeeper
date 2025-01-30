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
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using BSLib;
using GDModel;
using GDModel.Providers.GEDCOM;
using GKCore.Charts;
using GKCore.Design;
using GKCore.Design.Views;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.Maps;
using GKCore.Names;
using GKCore.Operations;
using GKCore.Options;
using GKCore.Tools;
using GKCore.Types;

namespace GKCore.Controllers
{
    public sealed class ModificationResult<T> where T : IGDMObject
    {
        public bool Result;
        public T Record;
    }

    /// <summary>
    /// 
    /// </summary>
    public static class BaseController
    {
        public static async void ViewRecordInfo(IView owner, IBaseWindow baseWin, GDMRecord record)
        {
            if (record == null) return;

            using (var dlg = AppHost.ResolveDialog<IRecordInfoDlg>(baseWin)) {
                dlg.Record = record;
                await AppHost.Instance.ShowModalAsync(dlg, owner, false);
            }
        }

        public static async void ViewTextInfo(IView owner, IBaseWindow baseWin, string text)
        {
            if (string.IsNullOrEmpty(text)) return;

            using (var dlg = AppHost.ResolveDialog<IRecordInfoDlg>(baseWin)) {
                dlg.HyperView.Lines.Text = text;
                await AppHost.Instance.ShowModalAsync(dlg, owner, false);
            }
        }

        #region Modify routines

        public static async Task<ModificationResult<GDMMultimediaRecord>> ModifyMedia(IView owner, IBaseWindow baseWin, GDMMultimediaRecord mediaRec)
        {
            var result = new ModificationResult<GDMMultimediaRecord>();

            try {
                baseWin.Context.BeginUpdate();
                GDMTree tree = baseWin.Context.Tree;

                bool exists = mediaRec != null;
                if (!exists) {
                    mediaRec = new GDMMultimediaRecord(tree);
                    mediaRec.FileReferences.Add(new GDMFileReferenceWithTitle());
                    tree.NewXRef(mediaRec);
                }

                try {
                    baseWin.Context.LockRecord(mediaRec);

                    using (var dlg = AppHost.ResolveDialog<IMediaEditDlg>(baseWin)) {
                        dlg.MultimediaRecord = mediaRec;
                        result.Result = await AppHost.Instance.ShowModalAsync(dlg, owner, false);
                    }
                } finally {
                    baseWin.Context.UnlockRecord(mediaRec);
                }

                if (!exists) {
                    if (result.Result) {
                        tree.AddRecord(mediaRec);
                    } else {
                        mediaRec.Dispose();
                        mediaRec = null;
                    }
                }

                result.Record = mediaRec;
            } finally {
                baseWin.Context.EndUpdate();
            }

            return result;
        }

        public static async Task<ModificationResult<GDMNoteRecord>> ModifyNote(IView owner, IBaseWindow baseWin, GDMNoteRecord noteRec)
        {
            var result = new ModificationResult<GDMNoteRecord>();

            try {
                baseWin.Context.BeginUpdate();
                GDMTree tree = baseWin.Context.Tree;

                bool exists = noteRec != null;
                if (!exists) {
                    noteRec = new GDMNoteRecord(tree);
                    tree.NewXRef(noteRec);
                }

                try {
                    baseWin.Context.LockRecord(noteRec);

                    if (GlobalOptions.Instance.UseExtendedNotes) {
                        using (var dlg = AppHost.ResolveDialog<INoteEditDlgEx>(baseWin)) {
                            dlg.NoteRecord = noteRec;
                            result.Result = await AppHost.Instance.ShowModalAsync(dlg, owner, false);
                        }
                    } else {
                        using (var dlg = AppHost.ResolveDialog<INoteEditDlg>(baseWin)) {
                            dlg.NoteRecord = noteRec;
                            result.Result = await AppHost.Instance.ShowModalAsync(dlg, owner, false);
                        }
                    }
                } finally {
                    baseWin.Context.UnlockRecord(noteRec);
                }

                if (!exists) {
                    if (result.Result) {
                        tree.AddRecord(noteRec);
                    } else {
                        noteRec.Dispose();
                        noteRec = null;
                    }
                }

                result.Record = noteRec;
            } finally {
                baseWin.Context.EndUpdate();
            }

            return result;
        }

        public static async Task<ModificationResult<GDMSourceRecord>> ModifySource(IView owner, IBaseWindow baseWin, GDMSourceRecord sourceRec)
        {
            var result = new ModificationResult<GDMSourceRecord>();

            try {
                baseWin.Context.BeginUpdate();
                GDMTree tree = baseWin.Context.Tree;

                bool exists = sourceRec != null;
                if (!exists) {
                    sourceRec = new GDMSourceRecord(tree);
                    tree.NewXRef(sourceRec);
                }

                try {
                    baseWin.Context.LockRecord(sourceRec);

                    using (var dlg = AppHost.ResolveDialog<ISourceEditDlg>(baseWin)) {
                        dlg.SourceRecord = sourceRec;
                        result.Result = await AppHost.Instance.ShowModalAsync(dlg, owner, false);
                    }
                } finally {
                    baseWin.Context.UnlockRecord(sourceRec);
                }

                if (!exists) {
                    if (result.Result) {
                        tree.AddRecord(sourceRec);
                    } else {
                        sourceRec.Dispose();
                        sourceRec = null;
                    }
                }

                result.Record = sourceRec;
            } finally {
                baseWin.Context.EndUpdate();
            }

            return result;
        }

        public static async Task<ModificationResult<GDMSourceCitation>> ModifySourceCitation(IView owner, IBaseWindow baseWin, ChangeTracker undoman,
                                                IGDMStructWithSourceCitations _struct, GDMSourceCitation cit)
        {
            var result = new ModificationResult<GDMSourceCitation>();

            try {
                baseWin.Context.BeginUpdate();

                bool exists = cit != null;
                if (!exists) {
                    cit = new GDMSourceCitation();
                }

                using (var dlg = AppHost.ResolveDialog<ISourceCitEditDlg>(baseWin)) {
                    dlg.SourceCitation = cit;
                    result.Result = await AppHost.Instance.ShowModalAsync(dlg, owner, false);
                }

                if (!exists) {
                    if (result.Result) {
                        result.Result = undoman.DoOrdinaryOperation(OperationType.otRecordSourceCitAdd, (GDMObject)_struct, cit);
                    } else {
                        cit.Dispose();
                        cit = null;
                    }
                }

                result.Record = cit;
            } finally {
                baseWin.Context.EndUpdate();
            }

            return result;
        }

        public static async Task<ModificationResult<GDMRepositoryCitation>> ModifyRepositoryCitation(IView owner, IBaseWindow baseWin, ChangeTracker undoman,
                                                GDMSourceRecord _struct, GDMRepositoryCitation cit)
        {
            var result = new ModificationResult<GDMRepositoryCitation>();

            try {
                baseWin.Context.BeginUpdate();

                bool exists = cit != null;
                if (!exists) {
                    cit = new GDMRepositoryCitation();
                }

                using (var dlg = AppHost.ResolveDialog<IRepositoryCitEditDlg>(baseWin)) {
                    dlg.RepositoryCitation = cit;
                    result.Result = await AppHost.Instance.ShowModalAsync(dlg, owner, false);
                }

                if (!exists) {
                    if (result.Result) {
                        result.Result = undoman.DoOrdinaryOperation(OperationType.otSourceRepositoryCitationAdd, _struct, cit);
                    } else {
                        cit.Dispose();
                        cit = null;
                    }
                }

                result.Record = cit;
            } finally {
                baseWin.Context.EndUpdate();
            }

            return result;
        }

        public static async Task<ModificationResult<GDMSourceCallNumber>> ModifyCallNumber(IView owner, IBaseWindow baseWin, ChangeTracker undoman,
                                                GDMRepositoryCitation _struct, GDMSourceCallNumber callNum)
        {
            var result = new ModificationResult<GDMSourceCallNumber>();

            try {
                baseWin.Context.BeginUpdate();

                bool exists = callNum != null;
                if (!exists) {
                    callNum = new GDMSourceCallNumber();
                }

                using (var dlg = AppHost.ResolveDialog<ISourceCallNumberEditDlg>(baseWin)) {
                    dlg.CallNumber = callNum;
                    result.Result = await AppHost.Instance.ShowModalAsync(dlg, owner, false);
                }

                if (!exists) {
                    if (result.Result) {
                        result.Result = undoman.DoOrdinaryOperation(OperationType.otCallNumberAdd, _struct, callNum);
                    } else {
                        callNum.Dispose();
                        callNum = null;
                    }
                }

                result.Record = callNum;
            } finally {
                baseWin.Context.EndUpdate();
            }

            return result;
        }

        public static async Task<ModificationResult<GDMRepositoryRecord>> ModifyRepository(IView owner, IBaseWindow baseWin, GDMRepositoryRecord repRec)
        {
            var result = new ModificationResult<GDMRepositoryRecord>();

            try {
                baseWin.Context.BeginUpdate();
                GDMTree tree = baseWin.Context.Tree;

                bool exists = repRec != null;
                if (!exists) {
                    repRec = new GDMRepositoryRecord(tree);
                    tree.NewXRef(repRec);
                }

                try {
                    baseWin.Context.LockRecord(repRec);

                    using (var dlg = AppHost.ResolveDialog<IRepositoryEditDlg>(baseWin)) {
                        dlg.RepositoryRecord = repRec;
                        result.Result = await AppHost.Instance.ShowModalAsync(dlg, owner, false);
                    }
                } finally {
                    baseWin.Context.UnlockRecord(repRec);
                }

                if (!exists) {
                    if (result.Result) {
                        tree.AddRecord(repRec);
                    } else {
                        repRec.Dispose();
                        repRec = null;
                    }
                }

                result.Record = repRec;
            } finally {
                baseWin.Context.EndUpdate();
            }

            return result;
        }

        public static async Task<ModificationResult<GDMGroupRecord>> ModifyGroup(IView owner, IBaseWindow baseWin, GDMGroupRecord groupRec)
        {
            var result = new ModificationResult<GDMGroupRecord>();

            try {
                baseWin.Context.BeginUpdate();
                GDMTree tree = baseWin.Context.Tree;

                bool exists = groupRec != null;
                if (!exists) {
                    groupRec = new GDMGroupRecord(tree);
                    tree.NewXRef(groupRec);
                }

                try {
                    baseWin.Context.LockRecord(groupRec);

                    using (var dlg = AppHost.ResolveDialog<IGroupEditDlg>(baseWin)) {
                        dlg.GroupRecord = groupRec;
                        result.Result = await AppHost.Instance.ShowModalAsync(dlg, owner, false);
                    }
                } finally {
                    baseWin.Context.UnlockRecord(groupRec);
                }

                if (!exists) {
                    if (result.Result) {
                        tree.AddRecord(groupRec);
                    } else {
                        groupRec.Dispose();
                        groupRec = null;
                    }
                }

                result.Record = groupRec;
            } finally {
                baseWin.Context.EndUpdate();
            }

            return result;
        }

        public static async Task<ModificationResult<GDMResearchRecord>> ModifyResearch(IView owner, IBaseWindow baseWin, GDMResearchRecord researchRec)
        {
            var result = new ModificationResult<GDMResearchRecord>();

            try {
                baseWin.Context.BeginUpdate();
                GDMTree tree = baseWin.Context.Tree;

                bool exists = researchRec != null;
                if (!exists) {
                    researchRec = new GDMResearchRecord(tree);
                    tree.NewXRef(researchRec);
                }

                try {
                    baseWin.Context.LockRecord(researchRec);

                    using (var dlg = AppHost.ResolveDialog<IResearchEditDlg>(baseWin)) {
                        dlg.ResearchRecord = researchRec;
                        result.Result = await AppHost.Instance.ShowModalAsync(dlg, owner, false);
                    }
                } finally {
                    baseWin.Context.UnlockRecord(researchRec);
                }

                if (!exists) {
                    if (result.Result) {
                        tree.AddRecord(researchRec);
                    } else {
                        researchRec.Dispose();
                        researchRec = null;
                    }
                }

                result.Record = researchRec;
            } finally {
                baseWin.Context.EndUpdate();
            }

            return result;
        }

        public static async Task<ModificationResult<GDMTaskRecord>> ModifyTask(IView owner, IBaseWindow baseWin, GDMTaskRecord taskRec)
        {
            var result = new ModificationResult<GDMTaskRecord>();

            try {
                baseWin.Context.BeginUpdate();
                GDMTree tree = baseWin.Context.Tree;

                bool exists = taskRec != null;
                if (!exists) {
                    taskRec = new GDMTaskRecord(tree);
                    tree.NewXRef(taskRec);
                }

                try {
                    baseWin.Context.LockRecord(taskRec);

                    using (var dlg = AppHost.ResolveDialog<ITaskEditDlg>(baseWin)) {
                        dlg.TaskRecord = taskRec;
                        result.Result = await AppHost.Instance.ShowModalAsync(dlg, owner, false);
                    }
                } finally {
                    baseWin.Context.UnlockRecord(taskRec);
                }

                if (!exists) {
                    if (result.Result) {
                        tree.AddRecord(taskRec);
                    } else {
                        taskRec.Dispose();
                        taskRec = null;
                    }
                }

                result.Record = taskRec;
            } finally {
                baseWin.Context.EndUpdate();
            }

            return result;
        }

        public static async Task<ModificationResult<GDMCommunicationRecord>> ModifyCommunication(IView owner, IBaseWindow baseWin, GDMCommunicationRecord commRec)
        {
            var result = new ModificationResult<GDMCommunicationRecord>();

            try {
                baseWin.Context.BeginUpdate();
                GDMTree tree = baseWin.Context.Tree;

                bool exists = commRec != null;
                if (!exists) {
                    commRec = new GDMCommunicationRecord(tree);
                    tree.NewXRef(commRec);
                }

                try {
                    baseWin.Context.LockRecord(commRec);

                    using (var dlg = AppHost.ResolveDialog<ICommunicationEditDlg>(baseWin)) {
                        dlg.CommunicationRecord = commRec;
                        result.Result = await AppHost.Instance.ShowModalAsync(dlg, owner, false);
                    }
                } finally {
                    baseWin.Context.UnlockRecord(commRec);
                }

                if (!exists) {
                    if (result.Result) {
                        tree.AddRecord(commRec);
                    } else {
                        commRec.Dispose();
                        commRec = null;
                    }
                }

                result.Record = commRec;
            } finally {
                baseWin.Context.EndUpdate();
            }

            return result;
        }

        public static async Task<ModificationResult<GDMLocationRecord>> ModifyLocation(IView owner, IBaseWindow baseWin, GDMLocationRecord locRec, string proposedName = "")
        {
            var result = new ModificationResult<GDMLocationRecord>();

            try {
                baseWin.Context.BeginUpdate();
                GDMTree tree = baseWin.Context.Tree;

                bool exists = locRec != null;
                if (!exists) {
                    locRec = new GDMLocationRecord(tree);
                    locRec.LocationName = proposedName;
                    tree.NewXRef(locRec);
                }

                try {
                    baseWin.Context.LockRecord(locRec);

                    using (var dlg = AppHost.ResolveDialog<ILocationEditDlg>(baseWin)) {
                        dlg.LocationRecord = locRec;
                        result.Result = await AppHost.Instance.ShowModalAsync(dlg, owner, false);
                    }
                } finally {
                    baseWin.Context.UnlockRecord(locRec);
                }

                if (!exists) {
                    if (result.Result) {
                        tree.AddRecord(locRec);
                    } else {
                        locRec.Dispose();
                        locRec = null;
                    }
                }

                result.Record = locRec;
            } finally {
                baseWin.Context.EndUpdate();
            }

            return result;
        }

        public static async Task<ModificationResult<GDMLocationName>> ModifyLocationName(IView owner, IBaseWindow baseWin, ChangeTracker undoman,
                                                GDMLocationRecord locRec, GDMLocationName locName)
        {
            var result = new ModificationResult<GDMLocationName>();

            try {
                baseWin.Context.BeginUpdate();

                bool exists = locName != null;
                if (!exists) {
                    locName = new GDMLocationName();
                }

                using (var dlg = AppHost.ResolveDialog<ILocationNameEditDlg>(baseWin)) {
                    dlg.LocationName = locName;
                    result.Result = await AppHost.Instance.ShowModalAsync(dlg, owner, false);
                }

                if (!exists) {
                    if (result.Result) {
                        result.Result = undoman.DoOrdinaryOperation(OperationType.otLocationNameAdd, locRec, locName);
                    } else {
                        locName.Dispose();
                        locName = null;
                    }
                }

                result.Record = locName;
            } finally {
                baseWin.Context.EndUpdate();
            }

            return result;
        }

        public static async Task<ModificationResult<GDMLocationLink>> ModifyLocationLink(IView owner, IBaseWindow baseWin, ChangeTracker undoman,
                                                GDMLocationRecord locRec, GDMLocationLink locLink, GDMLocationRecord proposedTopLocation = null)
        {
            var result = new ModificationResult<GDMLocationLink>();

            try {
                baseWin.Context.BeginUpdate();

                bool exists = locLink != null;
                if (!exists) {
                    locLink = new GDMLocationLink();
                    if (proposedTopLocation != null) {
                        locLink.XRef = proposedTopLocation.XRef;
                    }
                }

                using (var dlg = AppHost.ResolveDialog<ILocationLinkEditDlg>(baseWin)) {
                    dlg.LocationLink = locLink;
                    result.Result = await AppHost.Instance.ShowModalAsync(dlg, owner, false);
                }

                if (locRec.XRef == locLink.XRef) {
                    AppHost.StdDialogs.ShowAlert(LangMan.LS(LSID.InvalidLink));
                    result.Result = false;
                    return result;
                }

                if (!exists) {
                    if (result.Result) {
                        result.Result = undoman.DoOrdinaryOperation(OperationType.otLocationLinkAdd, locRec, locLink);
                    } else {
                        locLink.Dispose();
                        locLink = null;
                    }
                }

                result.Record = locLink;
            } finally {
                baseWin.Context.EndUpdate();
            }

            return result;
        }

        public static async Task<ModificationResult<EventDef>> ModifyEventDef(IView owner, EventDefList list, EventDef eventDef)
        {
            var result = new ModificationResult<EventDef>();

            try {
                bool exists = eventDef != null;
                if (!exists) {
                    eventDef = new EventDef();
                }

                using (var dlg = AppHost.ResolveDialog<IEventDefEditDlg>()) {
                    dlg.EventDef = eventDef;
                    result.Result = await AppHost.Instance.ShowModalAsync(dlg, owner, false);
                }

                if (!exists) {
                    if (result.Result) {
                        list.Add(eventDef);
                        result.Result = true;
                    } else {
                        eventDef = null;
                    }
                }

                result.Record = eventDef;
            } finally {
            }

            return result;
        }

        private static async Task PostProcessPerson(IBaseWindow baseWin, GDMIndividualRecord indivRec)
        {
            baseWin.Context.ImportNames(indivRec);

            IRecordsListModel listMan = baseWin.GetRecordsListManByType(GDMRecordType.rtIndividual);
            if (listMan == null) return;

            IndividualListFilter iFilter = (IndividualListFilter)listMan.Filter;

            if (iFilter.SourceMode == FilterGroupMode.Selected) {
                var src = baseWin.Context.Tree.FindXRef<GDMSourceRecord>(iFilter.SourceRef);
                if (src != null && await AppHost.StdDialogs.ShowQuestion(LangMan.LS(LSID.IncludedSourceFilter))) {
                    indivRec.AddSource(src, "", 0);
                }
            }

            if (iFilter.FilterGroupMode == FilterGroupMode.Selected) {
                var grp = baseWin.Context.Tree.FindXRef<GDMGroupRecord>(iFilter.GroupRef);
                if (grp != null && await AppHost.StdDialogs.ShowQuestion(LangMan.LS(LSID.IncludedGroupFilter))) {
                    grp.AddMember(indivRec);
                }
            }
        }

        public static async Task<ModificationResult<GDMIndividualRecord>> ModifyIndividual(IView owner, IBaseWindow baseWin, GDMIndividualRecord indivRec,
                                     GDMIndividualRecord target, TargetMode targetMode, GDMSex needSex)
        {
            var result = new ModificationResult<GDMIndividualRecord>();

            try {
                baseWin.Context.BeginUpdate();
                GDMTree tree = baseWin.Context.Tree;

                bool exists = (indivRec != null);
                if (!exists) {
                    indivRec = new GDMIndividualRecord(tree);
                    tree.NewXRef(indivRec);

                    indivRec.AddPersonalName(new GDMPersonalName());
                    baseWin.Context.CreateEventEx(indivRec, GEDCOMTagName.BIRT, "", "");
                }

                try {
                    baseWin.Context.LockRecord(indivRec);

                    using (var dlg = AppHost.ResolveDialog<IPersonEditDlg>(baseWin)) {
                        dlg.IndividualRecord = indivRec;

                        if (targetMode != TargetMode.tmNone) {
                            if (needSex == GDMSex.svMale || needSex == GDMSex.svFemale) {
                                dlg.SetNeedSex(needSex);
                            }
                            dlg.TargetMode = targetMode;
                            dlg.Target = target;
                        }

                        result.Result = await AppHost.Instance.ShowModalAsync(dlg, owner, false);
                    }
                } finally {
                    baseWin.Context.UnlockRecord(indivRec);
                }

                if (!exists) {
                    if (result.Result) {
                        await PostProcessPerson(baseWin, indivRec);

                        tree.AddRecord(indivRec);
                    } else {
                        indivRec.Clear();
                        indivRec.Dispose();
                        indivRec = null;
                    }
                }

                result.Record = indivRec;
            } finally {
                baseWin.Context.EndUpdate();
            }

            return result;
        }

        public static async Task<ModificationResult<GDMFamilyRecord>> ModifyFamily(IView owner, IBaseWindow baseWin, GDMFamilyRecord familyRec,
            TargetMode targetType, GDMIndividualRecord target)
        {
            var result = new ModificationResult<GDMFamilyRecord>();

            try {
                baseWin.Context.BeginUpdate();
                GDMTree tree = baseWin.Context.Tree;

                if (targetType == TargetMode.tmSpouse && target != null) {
                    GDMSex sex = target.Sex;
                    if (sex < GDMSex.svMale || sex > GDMSex.svFemale) {
                        AppHost.StdDialogs.ShowError(LangMan.LS(LSID.IsNotDefinedSex));
                        result.Record = familyRec;
                        result.Result = false;
                        return result;
                    }
                }

                bool exists = (familyRec != null);
                if (!exists) {
                    familyRec = new GDMFamilyRecord(tree);
                    tree.NewXRef(familyRec);
                }

                try {
                    baseWin.Context.LockRecord(familyRec);

                    using (var dlg = AppHost.ResolveDialog<IFamilyEditDlg>(baseWin)) {
                        dlg.FamilyRecord = familyRec;
                        dlg.SetTarget(targetType, target);

                        result.Result = await AppHost.Instance.ShowModalAsync(dlg, owner, false);
                    }
                } finally {
                    baseWin.Context.UnlockRecord(familyRec);
                }

                if (!exists) {
                    if (result.Result) {
                        tree.AddRecord(familyRec);
                    } else {
                        familyRec.Clear();
                        familyRec.Dispose();
                        familyRec = null;
                    }
                }

                result.Record = familyRec;
            } finally {
                baseWin.Context.EndUpdate();
            }

            return result;
        }

        public static async Task<bool> ModifyAddress(IView owner, IBaseWindow baseWin, GDMAddress address)
        {
            bool result;

            try {
                baseWin.Context.BeginUpdate();

                using (var dlg = AppHost.ResolveDialog<IAddressEditDlg>(baseWin)) {
                    dlg.Address = address;
                    result = await AppHost.Instance.ShowModalAsync(dlg, owner, false);
                }
            } finally {
                baseWin.Context.EndUpdate();
            }

            return result;
        }

        public static async Task<bool> ModifyName(IView owner, IBaseContext context, NameEntry nameEntry)
        {
            bool result;

            try {
                context.BeginUpdate();

                using (var dlg = AppHost.ResolveDialog<INameEditDlg>()) {
                    dlg.IName = nameEntry;
                    result = await AppHost.Instance.ShowModalAsync(dlg, owner, false);
                }
            } finally {
                context.EndUpdate();
            }

            return result;
        }

        #endregion

        #region Data modification functions for UI

        public static async Task<GDMRecord> AddRecord(IView owner, IBaseWindow baseWin, GDMRecordType rt, Target target)
        {
            bool result = false;
            GDMRecord rec = null;

            switch (rt) {
                case GDMRecordType.rtIndividual: {
                        // FIXME: legacy code, checkit
                        if (target == null) {
                            target = new Target();
                            target.TargetMode = TargetMode.tmParent;
                        }

                        var indiRes = await ModifyIndividual(owner, baseWin, null, target.TargetIndividual, target.TargetMode, target.NeedSex);
                        rec = indiRes.Record;
                        result = indiRes.Result;
                        break;
                    }

                case GDMRecordType.rtFamily: {
                        if (target == null) {
                            target = new Target();
                        }

                        TargetMode famTarget = (target.TargetMode != TargetMode.tmFamilyChild) ? TargetMode.tmNone : target.TargetMode;

                        var famRes = await ModifyFamily(owner, baseWin, null, famTarget, target.TargetIndividual);
                        rec = famRes.Record;
                        result = famRes.Result;
                        break;
                    }

                case GDMRecordType.rtNote: {
                        var noteRes = await ModifyNote(owner, baseWin, null);
                        rec = noteRes.Record;
                        result = noteRes.Result;
                        break;
                    }

                case GDMRecordType.rtMultimedia: {
                        var mmRes = await ModifyMedia(owner, baseWin, null);
                        rec = mmRes.Record;
                        result = mmRes.Result;
                        break;
                    }

                case GDMRecordType.rtSource: {
                        var srcRes = await ModifySource(owner, baseWin, null);
                        rec = srcRes.Record;
                        result = srcRes.Result;
                        break;
                    }

                case GDMRecordType.rtRepository: {
                        var repRes = await ModifyRepository(owner, baseWin, null);
                        rec = repRes.Record;
                        result = repRes.Result;
                        break;
                    }

                case GDMRecordType.rtGroup: {
                        var grpRes = await ModifyGroup(owner, baseWin, null);
                        rec = grpRes.Record;
                        result = grpRes.Result;
                        break;
                    }

                case GDMRecordType.rtResearch: {
                        var resRes = await ModifyResearch(owner, baseWin, null);
                        rec = resRes.Record;
                        result = resRes.Result;
                        break;
                    }

                case GDMRecordType.rtTask: {
                        var tskRes = await ModifyTask(owner, baseWin, null);
                        rec = tskRes.Record;
                        result = tskRes.Result;
                        break;
                    }

                case GDMRecordType.rtCommunication: {
                        var commRes = await ModifyCommunication(owner, baseWin, null);
                        rec = commRes.Record;
                        result = commRes.Result;
                        break;
                    }

                case GDMRecordType.rtLocation: {
                        var locRes = await ModifyLocation(owner, baseWin, null);
                        rec = locRes.Record;
                        result = locRes.Result;
                        break;
                    }
            }

            return (result) ? rec : null;
        }

        public static async Task<bool> EditRecord(IView owner, IBaseWindow baseWin, GDMRecord rec)
        {
            bool result = false;

            switch (rec.RecordType) {
                case GDMRecordType.rtIndividual: {
                        var indiRes = await ModifyIndividual(owner, baseWin, rec as GDMIndividualRecord, null, TargetMode.tmNone, GDMSex.svUnknown);
                        result = indiRes.Result;
                    }
                    break;

                case GDMRecordType.rtFamily: {
                        var famRes = await ModifyFamily(owner, baseWin, rec as GDMFamilyRecord, TargetMode.tmNone, null);
                        result = famRes.Result;
                    }
                    break;

                case GDMRecordType.rtNote: {
                        var noteRes = await ModifyNote(owner, baseWin, rec as GDMNoteRecord);
                        result = noteRes.Result;
                    }
                    break;

                case GDMRecordType.rtMultimedia: {
                        var mmRes = await ModifyMedia(owner, baseWin, rec as GDMMultimediaRecord);
                        result = mmRes.Result;
                    }
                    break;

                case GDMRecordType.rtSource: {
                        var srcRes = await ModifySource(owner, baseWin, rec as GDMSourceRecord);
                        result = srcRes.Result;
                    }
                    break;

                case GDMRecordType.rtRepository: {
                        var repRes = await ModifyRepository(owner, baseWin, rec as GDMRepositoryRecord);
                        result = repRes.Result;
                    }
                    break;

                case GDMRecordType.rtGroup: {
                        var grpRes = await ModifyGroup(owner, baseWin, rec as GDMGroupRecord);
                        result = grpRes.Result;
                    }
                    break;

                case GDMRecordType.rtResearch: {
                        var resRes = await ModifyResearch(owner, baseWin, rec as GDMResearchRecord);
                        result = resRes.Result;
                    }
                    break;

                case GDMRecordType.rtTask: {
                        var taskRes = await ModifyTask(owner, baseWin, rec as GDMTaskRecord);
                        result = taskRes.Result;
                    }
                    break;

                case GDMRecordType.rtCommunication: {
                        var commRes = await ModifyCommunication(owner, baseWin, rec as GDMCommunicationRecord);
                        result = commRes.Result;
                    }
                    break;

                case GDMRecordType.rtLocation: {
                        var locRes = await ModifyLocation(owner, baseWin, rec as GDMLocationRecord);
                        result = locRes.Result;
                    }
                    break;
            }

            return result;
        }

        private static string GetDeleteMessage(IBaseWindow baseWin, GDMRecord record)
        {
            string msg = "";
            switch (record.RecordType) {
                case GDMRecordType.rtIndividual:
                    msg = string.Format(LangMan.LS(LSID.PersonDeleteQuery), GKUtils.GetNameString(((GDMIndividualRecord)record), false));
                    break;

                case GDMRecordType.rtFamily:
                    msg = string.Format(LangMan.LS(LSID.FamilyDeleteQuery), GKUtils.GetFamilyString(baseWin.Context.Tree, (GDMFamilyRecord)record));
                    break;

                case GDMRecordType.rtNote: {
                        string value = GKUtils.TruncateStrings(((GDMNoteRecord)(record)).Lines, GKData.NOTE_NAME_MAX_LENGTH);
                        if (string.IsNullOrEmpty(value)) {
                            value = string.Format("#{0}", record.GetId().ToString());
                        }
                        msg = string.Format(LangMan.LS(LSID.NoteDeleteQuery), value);
                        break;
                    }

                case GDMRecordType.rtMultimedia:
                    msg = string.Format(LangMan.LS(LSID.MediaDeleteQuery), ((GDMMultimediaRecord)record).GetFileTitle());
                    break;

                case GDMRecordType.rtSource:
                    msg = string.Format(LangMan.LS(LSID.SourceDeleteQuery), ((GDMSourceRecord)record).ShortTitle);
                    break;

                case GDMRecordType.rtRepository:
                    msg = string.Format(LangMan.LS(LSID.RepositoryDeleteQuery), ((GDMRepositoryRecord)record).RepositoryName);
                    break;

                case GDMRecordType.rtGroup:
                    msg = string.Format(LangMan.LS(LSID.GroupDeleteQuery), ((GDMGroupRecord)record).GroupName);
                    break;

                case GDMRecordType.rtResearch:
                    msg = string.Format(LangMan.LS(LSID.ResearchDeleteQuery), ((GDMResearchRecord)record).ResearchName);
                    break;

                case GDMRecordType.rtTask:
                    msg = string.Format(LangMan.LS(LSID.TaskDeleteQuery),
                                        GKUtils.GetTaskGoalStr(baseWin.Context.Tree, (GDMTaskRecord)record));
                    break;

                case GDMRecordType.rtCommunication:
                    msg = string.Format(LangMan.LS(LSID.CommunicationDeleteQuery), ((GDMCommunicationRecord)record).CommName);
                    break;

                case GDMRecordType.rtLocation:
                    msg = string.Format(LangMan.LS(LSID.LocationDeleteQuery), ((GDMLocationRecord)record).LocationName);
                    break;
            }
            return msg;
        }

        public static async Task<bool> DeleteRecord(IBaseWindow baseWin, GDMRecord record, bool confirm)
        {
            bool result;

            if (record == null) {
                result = false;
            } else {
                if (confirm && !await AppHost.StdDialogs.ShowQuestion(GetDeleteMessage(baseWin, record))) {
                    result = false;
                } else {
                    result = await baseWin.Context.DeleteRecord(record);
                }
            }

            return result;
        }

        public static GDMRecord DuplicateRecord(IBaseContext context, GDMRecord original)
        {
            if (original == null) return null;

            if (original.RecordType != GDMRecordType.rtIndividual && original.RecordType != GDMRecordType.rtLocation) return null;

            AppHost.StdDialogs.ShowWarning(LangMan.LS(LSID.DuplicateWarning));

            try {
                context.BeginUpdate();

                GDMRecord result;
                switch (original.RecordType) {
                    case GDMRecordType.rtIndividual:
                        result = context.Tree.CreateIndividual();
                        break;
                    case GDMRecordType.rtLocation:
                        result = context.Tree.CreateLocation();
                        break;
                    default:
                        return null;
                }

                result.Assign(original);

                return result;
            } finally {
                context.EndUpdate();
            }
        }

        public static void NotifyRecord(IBaseWindow baseWin, GDMRecord record, RecordAction action)
        {
            if (baseWin == null || record == null) return;

            switch (action) {
                case RecordAction.raAdd:
                case RecordAction.raEdit:
                    record.ChangeDate.ChangeDateTime = DateTime.Now;
                    break;

                case RecordAction.raDelete:
                    break;

                case RecordAction.raJump:
                    break;

                case RecordAction.raMoveUp:
                case RecordAction.raMoveDown:
                    break;
            }

            if (action != RecordAction.raJump) {
                baseWin.Context.SetModified();
                AppHost.Instance.NotifyRecord(baseWin, record, action);
            }
        }

        public static async Task<bool> AddIndividualFather(IView owner, IBaseWindow baseWin, ChangeTracker localUndoman, GDMIndividualRecord person)
        {
            bool result = false;

            GDMIndividualRecord father = await baseWin.Context.SelectPerson(owner, person, TargetMode.tmChild, GDMSex.svMale);
            if (father == null) return result;

            if (father == person) {
                AppHost.StdDialogs.ShowWarning(LangMan.LS(LSID.FatherAsChild));
                return result;
            }

            GDMFamilyRecord family = await baseWin.Context.GetChildFamily(person, true, father);
            if (family == null) return result;

            var husb = baseWin.Context.Tree.GetPtrValue<GDMIndividualRecord>(family.Husband);
            if (husb != null) {
                // selected family with husband
                Logger.WriteError("BaseController.AddFather(): fail, because family already has father");
                result = true;
            } else {
                // new family
                result = localUndoman.DoOrdinaryOperation(OperationType.otFamilySpouseAttach, family, father);
            }

            return result;
        }

        public static async Task<bool> DeleteIndividualFather(IBaseWindow baseWin, ChangeTracker localUndoman, GDMIndividualRecord person)
        {
            bool result = false;

            if (await AppHost.StdDialogs.ShowQuestion(LangMan.LS(LSID.DetachFatherQuery))) {
                GDMFamilyRecord family = await baseWin.Context.GetChildFamily(person, false, null);
                if (family != null) {
                    GDMIndividualRecord father = baseWin.Context.Tree.GetPtrValue(family.Husband);
                    result = localUndoman.DoOrdinaryOperation(OperationType.otFamilySpouseDetach, family, father);
                }
            }

            return result;
        }

        public static async Task<bool> AddIndividualMother(IView owner, IBaseWindow baseWin, ChangeTracker localUndoman, GDMIndividualRecord person)
        {
            bool result = false;

            GDMIndividualRecord mother = await baseWin.Context.SelectPerson(owner, person, TargetMode.tmChild, GDMSex.svFemale);
            if (mother == null) return result;

            if (mother == person) {
                AppHost.StdDialogs.ShowWarning(LangMan.LS(LSID.MotherAsChild));
                return result;
            }

            GDMFamilyRecord family = await baseWin.Context.GetChildFamily(person, true, mother);
            if (family == null) return result;

            var wife = baseWin.Context.Tree.GetPtrValue<GDMIndividualRecord>(family.Wife);
            if (wife != null) {
                // selected family with wife
                Logger.WriteError("BaseController.AddMother(): fail, because family already has mother");
                result = true;
            } else {
                // new family
                result = localUndoman.DoOrdinaryOperation(OperationType.otFamilySpouseAttach, family, mother);
            }
            return result;
        }

        public static async Task<bool> DeleteIndividualMother(IBaseWindow baseWin, ChangeTracker localUndoman, GDMIndividualRecord person)
        {
            bool result = false;

            if (await AppHost.StdDialogs.ShowQuestion(LangMan.LS(LSID.DetachMotherQuery))) {
                GDMFamilyRecord family = await baseWin.Context.GetChildFamily(person, false, null);
                if (family != null) {
                    GDMIndividualRecord mother = baseWin.Context.Tree.GetPtrValue(family.Wife);
                    result = localUndoman.DoOrdinaryOperation(OperationType.otFamilySpouseDetach, family, mother);
                }
            }

            return result;
        }


        public static async Task<bool> AddFamilyHusband(IView owner, IBaseWindow baseWin, ChangeTracker localUndoman, GDMFamilyRecord family)
        {
            bool result = false;

            var wife = baseWin.Context.Tree.GetPtrValue(family.Wife);
            GDMIndividualRecord husband = await baseWin.Context.SelectPerson(owner, wife, TargetMode.tmSpouse, GDMSex.svMale);
            if (husband != null && family.Husband.IsEmpty()) {
                if (family.HasChild(husband)) {
                    AppHost.StdDialogs.ShowWarning(LangMan.LS(LSID.FatherAsChild));
                    return result;
                }

                result = localUndoman.DoOrdinaryOperation(OperationType.otFamilySpouseAttach, family, husband);
            }

            return result;
        }

        public static async Task<bool> DeleteFamilyHusband(IBaseWindow baseWin, ChangeTracker localUndoman, GDMFamilyRecord family)
        {
            bool result = false;

            GDMIndividualRecord husband = baseWin.Context.Tree.GetPtrValue(family.Husband);
            if (!baseWin.Context.IsAvailableRecord(husband)) return false;

            if (await AppHost.StdDialogs.ShowQuestion(LangMan.LS(LSID.DetachHusbandQuery))) {
                result = localUndoman.DoOrdinaryOperation(OperationType.otFamilySpouseDetach, family, husband);
            }

            return result;
        }

        public static async Task<bool> AddFamilyWife(IView owner, IBaseWindow baseWin, ChangeTracker localUndoman, GDMFamilyRecord family)
        {
            bool result = false;

            var husband = baseWin.Context.Tree.GetPtrValue(family.Husband);
            GDMIndividualRecord wife = await baseWin.Context.SelectPerson(owner, husband, TargetMode.tmSpouse, GDMSex.svFemale);
            if (wife != null && family.Wife.IsEmpty()) {
                if (family.HasChild(wife)) {
                    AppHost.StdDialogs.ShowWarning(LangMan.LS(LSID.MotherAsChild));
                    return result;
                }

                result = localUndoman.DoOrdinaryOperation(OperationType.otFamilySpouseAttach, family, wife);
            }

            return result;
        }

        public static async Task<bool> DeleteFamilyWife(IBaseWindow baseWin, ChangeTracker localUndoman, GDMFamilyRecord family)
        {
            bool result = false;

            GDMIndividualRecord wife = baseWin.Context.Tree.GetPtrValue(family.Wife);
            if (!baseWin.Context.IsAvailableRecord(wife)) return false;

            if (await AppHost.StdDialogs.ShowQuestion(LangMan.LS(LSID.DetachWifeQuery))) {
                result = localUndoman.DoOrdinaryOperation(OperationType.otFamilySpouseDetach, family, wife);
            }

            return result;
        }

        public static async Task<bool> SelectPhotoRegion(IView owner, IBaseWindow baseWin, GDMMultimediaRecord mediaRecord, ExtRect region)
        {
            var indiRec = await baseWin.Context.SelectPerson(owner, null, TargetMode.tmNone, GDMSex.svUnknown);
            if (indiRec == null) return false;

            GDMMultimediaLink mmLink = indiRec.GetPrimaryMultimediaLink();
            if (mmLink == null) {
                mmLink = indiRec.SetPrimaryMultimediaLink(mediaRecord);
                SetMultimediaLinkRegion(mmLink, region, true);
            } else {
                mmLink = indiRec.AddMultimedia(mediaRecord);
                SetMultimediaLinkRegion(mmLink, region, false);
            }

            return true;
        }

        public static void SetMultimediaLinkRegion(GDMMultimediaLink mmLink, ExtRect selectionRegion, bool isPrimary)
        {
            if (mmLink != null) {
                mmLink.CutoutPosition.Value = selectionRegion;
                mmLink.IsPrimaryCutout = isPrimary;
            }
        }

        public static async Task<bool> SelectPortraitRegion(IView owner, IBaseWindow baseWin, GDMMultimediaLink mmLink)
        {
            bool result;
            using (var dlg = AppHost.ResolveDialog<IPortraitSelectDlg>(baseWin)) {
                dlg.MultimediaLink = mmLink;
                result = await AppHost.Instance.ShowModalAsync(dlg, owner, false);
            }
            return result;
        }

        public static async Task<bool> AddIndividualPortrait(IView owner, IBaseWindow baseWin, ChangeTracker localUndoman, GDMIndividualRecord iRec)
        {
            GDMMultimediaRecord mmRec = await baseWin.Context.SelectRecord(owner, GDMRecordType.rtMultimedia, null) as GDMMultimediaRecord;
            if (mmRec == null) return false;

            // remove previous portrait link
            GDMMultimediaLink mmLink = iRec.GetPrimaryMultimediaLink();
            if (mmLink != null) {
                mmLink.IsPrimary = false;
            }

            // set new portrait link
            mmLink = iRec.SetPrimaryMultimediaLink(mmRec);

            // select portrait area
            bool result = await SelectPortraitRegion(owner, baseWin, mmLink);

            if (result) {
                result = localUndoman.DoOrdinaryOperation(OperationType.otIndividualPortraitAttach, iRec, mmLink);
            }

            return result;
        }

        public static bool DeleteIndividualPortrait(IBaseWindow baseWin, ChangeTracker localUndoman, GDMIndividualRecord iRec)
        {
            GDMMultimediaLink mmLink = iRec.GetPrimaryMultimediaLink();
            if (mmLink != null) {
                return localUndoman.DoOrdinaryOperation(OperationType.otIndividualPortraitDetach, iRec, mmLink);
            }
            return false;
        }

        public static async void ShowRecMerge(IView owner, IBaseWindow baseWin, GDMRecord rec1, GDMRecord rec2)
        {
            try {
                baseWin.Context.BeginUpdate();
                using (var dlg = AppHost.Container.Resolve<IRecMergeDlg>(baseWin)) {
                    dlg.SetRec1(rec1);
                    dlg.SetRec2(rec2);
                    await AppHost.Instance.ShowModalAsync(dlg, owner, false);
                }
            } finally {
                baseWin.Context.EndUpdate();
            }
        }

        #endregion

        #region Aux

        public static bool DetectCycle(GDMTree tree, GDMIndividualRecord iRec)
        {
            string res = TreeInspector.DetectCycle(tree, iRec);
            if (!string.IsNullOrEmpty(res)) {
                AppHost.StdDialogs.ShowError(string.Format(LangMan.LS(LSID.DetectedDataLoop), res));
                return true;
            }
            return false;
        }

        public static void ShowTreeChart(IBaseWindow baseWin, GDMIndividualRecord selPerson, TreeChartKind chartKind)
        {
            if (baseWin == null) return;

            if (selPerson == null) {
                var mruFile = AppHost.Instance.GetMRUFile(baseWin);
                if (mruFile != null) {
                    selPerson = baseWin.Context.Tree.FindXRef<GDMIndividualRecord>(mruFile.LastTreeRecord);
                }
            }

            if (selPerson == null) return;

            if (DetectCycle(baseWin.Context.Tree, selPerson)) return;

            if (TreeChartModel.CheckTreeChartSize(baseWin.Context.Tree, selPerson, chartKind)) {
                var fmChart = AppHost.Container.Resolve<ITreeChartWin>(baseWin);
                fmChart.GenChart(selPerson, chartKind);
                AppHost.Instance.ShowWindow(fmChart);
            }
        }

        public static void ShowCircleChart(IBaseWindow baseWin, CircleChartType chartKind)
        {
            var selPerson = baseWin.GetSelectedPerson();
            if (selPerson == null) return;

            if (BaseController.DetectCycle(baseWin.Context.Tree, selPerson)) return;

            var fmChart = AppHost.Container.Resolve<ICircleChartWin>(baseWin, selPerson, chartKind);
            AppHost.Instance.ShowWindow(fmChart);
        }

        #endregion

        public static void ShowMedia(IBaseWindow baseWin, GDMMultimediaRecord mediaRec, bool modal)
        {
            if (mediaRec == null)
                throw new ArgumentNullException("mediaRec");

            GDMFileReferenceWithTitle fileRef = mediaRec.FileReferences[0];
            if (fileRef == null) return;

            if (!GKUtils.UseEmbeddedViewer(fileRef.GetMultimediaFormat())) {
                string targetFile = baseWin.Context.MediaLoad(fileRef);
                GKUtils.LoadExtFile(targetFile);
            } else {
                var mediaViewer = AppHost.Container.Resolve<IMediaViewerWin>(baseWin);
                try {
                    mediaViewer.MultimediaRecord = mediaRec;
                    mediaViewer.Show(true);
                } catch (Exception ex) {
                    if (mediaViewer != null) mediaViewer.Dispose();
                    Logger.WriteError("BaseController.ShowMedia()", ex);
                }
            }
        }

        public static void ShowMap(IBaseWindow baseWin, List<GeoPoint> fixedPoints = null)
        {
            var mapsWin = AppHost.Container.Resolve<IMapsViewerWin>(baseWin);
            if (fixedPoints != null) {
                mapsWin.ShowFixedPoints(fixedPoints);
            }
            AppHost.Instance.ShowWindow(mapsWin);
        }

        public static void ShowMap_Sub(IBaseWindow baseWin, GDMLocationRecord locRec)
        {
            var tree = baseWin.Context.Tree;
            var subLinks = new HashSet<GDMLocationRecord>();
            GKUtils.GetLocationRecursiveSubordinateLinks(tree, locRec, subLinks, true);

            var geoPoints = new List<GeoPoint>();
            foreach (var location in subLinks) {
                string locName = GKUtils.GetRecordName(tree, location, false);
                var mapPt = location.Map;
                geoPoints.Add(new GeoPoint(mapPt.Lati, mapPt.Long, locName));
            }

            ShowMap(baseWin, geoPoints);
        }

        public static void ShowMap_Indi(IBaseWindow baseWin, GDMLocationRecord locRec)
        {
            var tree = baseWin.Context.Tree;
            var subLinks = new HashSet<GDMLocationRecord>();
            GKUtils.GetLocationRecursiveSubordinateLinks(tree, locRec, subLinks, true);

            var selectedIndividuals = baseWin.GetContentList(GDMRecordType.rtIndividual).Cast<GDMIndividualRecord>();

            var locName = new StringBuilder();

            var geoPoints = new List<GeoPoint>();
            foreach (var location in subLinks) {
                var individualRecords = new HashSet<GDMIndividualRecord>();
                GKUtils.GetLocationIndividuals(tree, location, individualRecords);

                individualRecords.IntersectWith(selectedIndividuals);
                if (individualRecords.Count == 0) continue;

                locName.AppendLine(GKUtils.GetRecordName(tree, location, false));
                var mapPt = location.Map;

                foreach (var iRec in individualRecords) {
                    string iName = GKUtils.GetRecordName(tree, iRec, false);
                    locName.AppendLine(iName);
                }

                geoPoints.Add(new GeoPoint(mapPt.Lati, mapPt.Long, locName.ToString()));
                locName.Clear();
            }

            ShowMap(baseWin, geoPoints);
        }

        public static void ShowMap_IndiList(IBaseWindow baseWin, List<GDMIndividualRecord> indiList)
        {
            var tree = baseWin.Context.Tree;

            var locations = new Dictionary<GDMLocationRecord, HashSet<GDMIndividualRecord>>();

            for (int i = 0; i < indiList.Count; i++) {
                var indiRec = indiList[i];

                var locs = new HashSet<GDMLocationRecord>();
                GKUtils.GetIndividualLocations(tree, indiRec, locs);

                foreach (var locRec in locs) {
                    if (locRec.Map.IsEmpty()) continue;

                    HashSet<GDMIndividualRecord> indiSet = null;
                    if (!locations.TryGetValue(locRec, out indiSet)) {
                        indiSet = new HashSet<GDMIndividualRecord>();
                        locations.Add(locRec, indiSet);
                    }
                    indiSet.Add(indiRec);
                }
            }

            var locName = new StringBuilder();
            var geoPoints = new List<GeoPoint>();
            foreach (var location in locations) {
                var locRec = location.Key;
                var indiSet = location.Value;

                locName.AppendLine(GKUtils.GetRecordName(tree, locRec, false));
                var mapPt = locRec.Map;

                foreach (var iRec in indiSet) {
                    string iName = GKUtils.GetRecordName(tree, iRec, false);
                    locName.AppendLine(iName);
                }

                geoPoints.Add(new GeoPoint(mapPt.Lati, mapPt.Long, locName.ToString()));
                locName.Clear();
            }

            ShowMap(baseWin, geoPoints);
        }
    }
}
