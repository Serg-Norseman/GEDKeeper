/*
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

using GDModel;
using GDModel.Providers.GEDCOM;
using GKCore.Charts;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.Design.Views;
using GKCore.Names;
using GKCore.Operations;
using GKCore.Options;
using GKCore.Tools;
using GKCore.Types;
using GKCore.Design;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public static class BaseController
    {
        public static void ViewRecordInfo(IView owner, IBaseWindow baseWin, GDMRecord record)
        {
            if (record != null) {
                using (var dlg = AppHost.ResolveDialog<IRecordInfoDlg>(baseWin)) {
                    dlg.Record = record;
                    AppHost.Instance.ShowModalX(dlg, owner, false);
                }
            }
        }

        #region Modify routines

        public static bool ModifyMedia(IView owner, IBaseWindow baseWin, ref GDMMultimediaRecord mediaRec)
        {
            bool result;

            try {
                baseWin.Context.BeginUpdate();
                GDMTree tree = baseWin.Context.Tree;

                using (var dlg = AppHost.ResolveDialog<IMediaEditDlg>(baseWin)) {
                    bool exists = mediaRec != null;
                    if (!exists) {
                        mediaRec = new GDMMultimediaRecord(tree);
                        mediaRec.FileReferences.Add(new GDMFileReferenceWithTitle());
                        tree.NewXRef(mediaRec);
                    }

                    try {
                        baseWin.Context.LockRecord(mediaRec);

                        dlg.MultimediaRecord = mediaRec;
                        result = (AppHost.Instance.ShowModalX(dlg, owner, false));
                    } finally {
                        baseWin.Context.UnlockRecord(mediaRec);
                    }

                    if (!exists) {
                        if (result) {
                            tree.AddRecord(mediaRec);
                        } else {
                            mediaRec.Dispose();
                            mediaRec = null;
                        }
                    }
                }
            } finally {
                baseWin.Context.EndUpdate();
            }

            return result;
        }

        public static bool ModifyNote(IView owner, IBaseWindow baseWin, ref GDMNoteRecord noteRec)
        {
            bool result;

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
                            result = (AppHost.Instance.ShowModalX(dlg, owner, false));
                        }
                    } else {
                        using (var dlg = AppHost.ResolveDialog<INoteEditDlg>(baseWin)) {
                            dlg.NoteRecord = noteRec;
                            result = (AppHost.Instance.ShowModalX(dlg, owner, false));
                        }
                    }
                } finally {
                    baseWin.Context.UnlockRecord(noteRec);
                }

                if (!exists) {
                    if (result) {
                        tree.AddRecord(noteRec);
                    } else {
                        noteRec.Dispose();
                        noteRec = null;
                    }
                }
            } finally {
                baseWin.Context.EndUpdate();
            }

            return result;
        }

        public static bool ModifySource(IView owner, IBaseWindow baseWin, ref GDMSourceRecord sourceRec)
        {
            bool result;

            try {
                baseWin.Context.BeginUpdate();
                GDMTree tree = baseWin.Context.Tree;

                using (var dlg = AppHost.ResolveDialog<ISourceEditDlg>(baseWin)) {
                    bool exists = sourceRec != null;
                    if (!exists) {
                        sourceRec = new GDMSourceRecord(tree);
                        tree.NewXRef(sourceRec);
                    }

                    try {
                        baseWin.Context.LockRecord(sourceRec);

                        dlg.SourceRecord = sourceRec;
                        result = (AppHost.Instance.ShowModalX(dlg, owner, false));
                    } finally {
                        baseWin.Context.UnlockRecord(sourceRec);
                    }

                    if (!exists) {
                        if (result) {
                            tree.AddRecord(sourceRec);
                        } else {
                            sourceRec.Dispose();
                            sourceRec = null;
                        }
                    }
                }
            } finally {
                baseWin.Context.EndUpdate();
            }

            return result;
        }

        public static bool ModifySourceCitation(IView owner, IBaseWindow baseWin, ChangeTracker undoman,
                                                IGDMStructWithSourceCitations _struct, ref GDMSourceCitation cit)
        {
            bool result;

            try {
                baseWin.Context.BeginUpdate();

                using (var dlg = AppHost.ResolveDialog<ISourceCitEditDlg>(baseWin)) {
                    bool exists = cit != null;
                    if (!exists) {
                        cit = new GDMSourceCitation();
                    }

                    dlg.SourceCitation = cit;
                    result = AppHost.Instance.ShowModalX(dlg, owner, false);

                    if (!exists) {
                        if (result) {
                            result = undoman.DoOrdinaryOperation(OperationType.otRecordSourceCitAdd, (GDMObject)_struct, cit);
                        } else {
                            cit.Dispose();
                        }
                    }
                }
            } finally {
                baseWin.Context.EndUpdate();
            }

            return result;
        }

        public static bool ModifyRepository(IView owner, IBaseWindow baseWin, ref GDMRepositoryRecord repRec)
        {
            bool result;

            try {
                baseWin.Context.BeginUpdate();
                GDMTree tree = baseWin.Context.Tree;

                using (var dlg = AppHost.ResolveDialog<IRepositoryEditDlg>(baseWin)) {
                    bool exists = repRec != null;
                    if (!exists) {
                        repRec = new GDMRepositoryRecord(tree);
                        tree.NewXRef(repRec);
                    }

                    try {
                        baseWin.Context.LockRecord(repRec);

                        dlg.RepositoryRecord = repRec;
                        result = AppHost.Instance.ShowModalX(dlg, owner, false);
                    } finally {
                        baseWin.Context.UnlockRecord(repRec);
                    }

                    if (!exists) {
                        if (result) {
                            tree.AddRecord(repRec);
                        } else {
                            repRec.Dispose();
                            repRec = null;
                        }
                    }
                }
            } finally {
                baseWin.Context.EndUpdate();
            }

            return result;
        }

        public static bool ModifyGroup(IView owner, IBaseWindow baseWin, ref GDMGroupRecord groupRec)
        {
            bool result;

            try {
                baseWin.Context.BeginUpdate();
                GDMTree tree = baseWin.Context.Tree;

                using (var dlg = AppHost.ResolveDialog<IGroupEditDlg>(baseWin)) {
                    bool exists = groupRec != null;
                    if (!exists) {
                        groupRec = new GDMGroupRecord(tree);
                        tree.NewXRef(groupRec);
                    }

                    try {
                        baseWin.Context.LockRecord(groupRec);

                        dlg.GroupRecord = groupRec;
                        result = (AppHost.Instance.ShowModalX(dlg, owner, false));
                    } finally {
                        baseWin.Context.UnlockRecord(groupRec);
                    }

                    if (!exists) {
                        if (result) {
                            tree.AddRecord(groupRec);
                        } else {
                            groupRec.Dispose();
                            groupRec = null;
                        }
                    }
                }
            } finally {
                baseWin.Context.EndUpdate();
            }

            return result;
        }

        public static bool ModifyResearch(IView owner, IBaseWindow baseWin, ref GDMResearchRecord researchRec)
        {
            bool result;

            try {
                baseWin.Context.BeginUpdate();
                GDMTree tree = baseWin.Context.Tree;

                using (var dlg = AppHost.ResolveDialog<IResearchEditDlg>(baseWin)) {
                    bool exists = researchRec != null;
                    if (!exists) {
                        researchRec = new GDMResearchRecord(tree);
                        tree.NewXRef(researchRec);
                    }

                    try {
                        baseWin.Context.LockRecord(researchRec);

                        dlg.ResearchRecord = researchRec;
                        result = AppHost.Instance.ShowModalX(dlg, owner, false);
                    } finally {
                        baseWin.Context.UnlockRecord(researchRec);
                    }

                    if (!exists) {
                        if (result) {
                            tree.AddRecord(researchRec);
                        } else {
                            researchRec.Dispose();
                            researchRec = null;
                        }
                    }
                }
            } finally {
                baseWin.Context.EndUpdate();
            }

            return result;
        }

        public static bool ModifyTask(IView owner, IBaseWindow baseWin, ref GDMTaskRecord taskRec)
        {
            bool result;

            try {
                baseWin.Context.BeginUpdate();
                GDMTree tree = baseWin.Context.Tree;

                using (var dlg = AppHost.ResolveDialog<ITaskEditDlg>(baseWin)) {
                    bool exists = taskRec != null;
                    if (!exists) {
                        taskRec = new GDMTaskRecord(tree);
                        tree.NewXRef(taskRec);
                    }

                    try {
                        baseWin.Context.LockRecord(taskRec);

                        dlg.TaskRecord = taskRec;
                        result = AppHost.Instance.ShowModalX(dlg, owner, false);
                    } finally {
                        baseWin.Context.UnlockRecord(taskRec);
                    }

                    if (!exists) {
                        if (result) {
                            tree.AddRecord(taskRec);
                        } else {
                            taskRec.Dispose();
                            taskRec = null;
                        }
                    }
                }
            } finally {
                baseWin.Context.EndUpdate();
            }

            return result;
        }

        public static bool ModifyCommunication(IView owner, IBaseWindow baseWin, ref GDMCommunicationRecord commRec)
        {
            bool result;

            try {
                baseWin.Context.BeginUpdate();
                GDMTree tree = baseWin.Context.Tree;

                using (var dlg = AppHost.ResolveDialog<ICommunicationEditDlg>(baseWin)) {
                    bool exists = commRec != null;
                    if (!exists) {
                        commRec = new GDMCommunicationRecord(tree);
                        tree.NewXRef(commRec);
                    }

                    try {
                        baseWin.Context.LockRecord(commRec);

                        dlg.CommunicationRecord = commRec;
                        result = AppHost.Instance.ShowModalX(dlg, owner, false);
                    } finally {
                        baseWin.Context.UnlockRecord(commRec);
                    }

                    if (!exists) {
                        if (result) {
                            tree.AddRecord(commRec);
                        } else {
                            commRec.Dispose();
                            commRec = null;
                        }
                    }
                }
            } finally {
                baseWin.Context.EndUpdate();
            }

            return result;
        }

        public static bool ModifyLocation(IView owner, IBaseWindow baseWin, ref GDMLocationRecord locRec)
        {
            bool result;

            try {
                baseWin.Context.BeginUpdate();
                GDMTree tree = baseWin.Context.Tree;

                using (var dlg = AppHost.ResolveDialog<ILocationEditDlg>(baseWin)) {
                    bool exists = locRec != null;
                    if (!exists) {
                        locRec = new GDMLocationRecord(tree);
                        tree.NewXRef(locRec);
                    }

                    try {
                        baseWin.Context.LockRecord(locRec);

                        dlg.LocationRecord = locRec;
                        result = AppHost.Instance.ShowModalX(dlg, owner, false);
                    } finally {
                        baseWin.Context.UnlockRecord(locRec);
                    }

                    if (!exists) {
                        if (result) {
                            tree.AddRecord(locRec);
                        } else {
                            locRec.Dispose();
                            locRec = null;
                        }
                    }
                }
            } finally {
                baseWin.Context.EndUpdate();
            }

            return result;
        }

        private static void PostProcessPerson(IBaseWindow baseWin, GDMIndividualRecord indivRec)
        {
            baseWin.Context.ImportNames(indivRec);

            IRecordsListModel listMan = baseWin.GetRecordsListManByType(GDMRecordType.rtIndividual);
            if (listMan == null) return;

            IndividualListFilter iFilter = (IndividualListFilter)listMan.Filter;

            if (iFilter.SourceMode == FilterGroupMode.Selected) {
                var src = baseWin.Context.Tree.FindXRef<GDMSourceRecord>(iFilter.SourceRef);
                if (src != null && AppHost.StdDialogs.ShowQuestionYN(LangMan.LS(LSID.LSID_IncludedSourceFilter))) {
                    indivRec.AddSource(src, "", 0);
                }
            }

            if (iFilter.FilterGroupMode == FilterGroupMode.Selected) {
                var grp = baseWin.Context.Tree.FindXRef<GDMGroupRecord>(iFilter.GroupRef);
                if (grp != null && AppHost.StdDialogs.ShowQuestionYN(LangMan.LS(LSID.LSID_IncludedGroupFilter))) {
                    grp.AddMember(indivRec);
                }
            }
        }

        public static bool ModifyIndividual(IView owner, IBaseWindow baseWin, ref GDMIndividualRecord indivRec,
                                     GDMIndividualRecord target, TargetMode targetMode, GDMSex needSex)
        {
            bool result;

            try {
                baseWin.Context.BeginUpdate();
                GDMTree tree = baseWin.Context.Tree;

                using (var dlg = AppHost.ResolveDialog<IPersonEditDlg>(baseWin)) {
                    bool exists = (indivRec != null);
                    if (!exists) {
                        indivRec = new GDMIndividualRecord(tree);
                        tree.NewXRef(indivRec);

                        indivRec.AddPersonalName(new GDMPersonalName());
                        baseWin.Context.CreateEventEx(indivRec, GEDCOMTagName.BIRT, "", "");
                    }

                    try {
                        baseWin.Context.LockRecord(indivRec);

                        dlg.IndividualRecord = indivRec;

                        if (targetMode != TargetMode.tmNone) {
                            if (needSex == GDMSex.svMale || needSex == GDMSex.svFemale) {
                                dlg.SetNeedSex(needSex);
                            }
                            dlg.TargetMode = targetMode;
                            dlg.Target = target;
                        }

                        result = (AppHost.Instance.ShowModalX(dlg, owner, false));
                    } finally {
                        baseWin.Context.UnlockRecord(indivRec);
                    }

                    if (!exists) {
                        if (result) {
                            PostProcessPerson(baseWin, indivRec);

                            tree.AddRecord(indivRec);
                        } else {
                            indivRec.Clear();
                            indivRec.Dispose();
                            indivRec = null;
                        }
                    }
                }
            } finally {
                baseWin.Context.EndUpdate();
            }

            return result;
        }

        public static bool ModifyFamily(IView owner, IBaseWindow baseWin, ref GDMFamilyRecord familyRec, TargetMode targetType, GDMIndividualRecord target)
        {
            bool result;

            try {
                baseWin.Context.BeginUpdate();
                GDMTree tree = baseWin.Context.Tree;

                if (targetType == TargetMode.tmSpouse && target != null) {
                    GDMSex sex = target.Sex;
                    if (sex < GDMSex.svMale || sex > GDMSex.svFemale) {
                        AppHost.StdDialogs.ShowError(LangMan.LS(LSID.LSID_IsNotDefinedSex));
                        return false;
                    }
                }

                using (var dlg = AppHost.ResolveDialog<IFamilyEditDlg>(baseWin)) {
                    bool exists = (familyRec != null);
                    if (!exists) {
                        familyRec = new GDMFamilyRecord(tree);
                        tree.NewXRef(familyRec);
                    }

                    try {
                        baseWin.Context.LockRecord(familyRec);

                        dlg.FamilyRecord = familyRec;
                        dlg.SetTarget(targetType, target);

                        result = (AppHost.Instance.ShowModalX(dlg, owner, false));
                    } finally {
                        baseWin.Context.UnlockRecord(familyRec);
                    }

                    if (!exists) {
                        if (result) {
                            tree.AddRecord(familyRec);
                        } else {
                            familyRec.Clear();
                            familyRec.Dispose();
                            familyRec = null;
                        }
                    }
                }
            } finally {
                baseWin.Context.EndUpdate();
            }

            return result;
        }

        public static bool ModifyAddress(IView owner, IBaseWindow baseWin, GDMAddress address)
        {
            bool result;

            try {
                baseWin.Context.BeginUpdate();

                using (var dlg = AppHost.ResolveDialog<IAddressEditDlg>(baseWin)) {
                    dlg.Address = address;
                    result = (AppHost.Instance.ShowModalX(dlg, owner, false));
                }
            } finally {
                baseWin.Context.EndUpdate();
            }

            return result;
        }

        public static bool ModifyName(IView owner, IBaseContext context, ref NameEntry nameEntry)
        {
            bool result;

            try {
                context.BeginUpdate();

                using (var dlg = AppHost.ResolveDialog<INameEditDlg>()) {
                    dlg.IName = nameEntry;
                    result = AppHost.Instance.ShowModalX(dlg, owner, false);
                }
            } finally {
                context.EndUpdate();
            }

            return result;
        }

        #endregion

        #region Data modification functions for UI

        public static GDMRecord AddRecord(IView owner, IBaseWindow baseWin, GDMRecordType rt, Target target)
        {
            bool result = false;
            GDMRecord rec = null;

            switch (rt) {
                case GDMRecordType.rtIndividual:
                    {
                        // FIXME: legacy code, checkit
                        if (target == null) {
                            target = new Target();
                            target.TargetMode = TargetMode.tmParent;
                        }

                        GDMIndividualRecord indivRec = null;
                        result = ModifyIndividual(owner, baseWin, ref indivRec, target.TargetIndividual, target.TargetMode, target.NeedSex);
                        rec = indivRec;
                        break;
                    }

                case GDMRecordType.rtFamily:
                    {
                        if (target == null) {
                            target = new Target();
                        }

                        TargetMode famTarget = (target.TargetMode != TargetMode.tmFamilyChild) ? TargetMode.tmNone : target.TargetMode;

                        GDMFamilyRecord fam = null;
                        result = ModifyFamily(owner, baseWin, ref fam, famTarget, target.TargetIndividual);
                        rec = fam;
                        break;
                    }

                case GDMRecordType.rtNote:
                    {
                        GDMNoteRecord note = null;
                        result = ModifyNote(owner, baseWin, ref note);
                        rec = note;
                        break;
                    }

                case GDMRecordType.rtMultimedia:
                    {
                        GDMMultimediaRecord mmRec = null;
                        result = ModifyMedia(owner, baseWin, ref mmRec);
                        rec = mmRec;
                        break;
                    }

                case GDMRecordType.rtSource:
                    {
                        GDMSourceRecord src = null;
                        result = ModifySource(owner, baseWin, ref src);
                        rec = src;
                        break;
                    }

                case GDMRecordType.rtRepository:
                    {
                        GDMRepositoryRecord rep = null;
                        result = ModifyRepository(owner, baseWin, ref rep);
                        rec = rep;
                        break;
                    }

                case GDMRecordType.rtGroup:
                    {
                        GDMGroupRecord grp = null;
                        result = ModifyGroup(owner, baseWin, ref grp);
                        rec = grp;
                        break;
                    }

                case GDMRecordType.rtResearch:
                    {
                        GDMResearchRecord rsr = null;
                        result = ModifyResearch(owner, baseWin, ref rsr);
                        rec = rsr;
                        break;
                    }

                case GDMRecordType.rtTask:
                    {
                        GDMTaskRecord tsk = null;
                        result = ModifyTask(owner, baseWin, ref tsk);
                        rec = tsk;
                        break;
                    }

                case GDMRecordType.rtCommunication:
                    {
                        GDMCommunicationRecord comm = null;
                        result = ModifyCommunication(owner, baseWin, ref comm);
                        rec = comm;
                        break;
                    }

                case GDMRecordType.rtLocation:
                    {
                        GDMLocationRecord loc = null;
                        result = ModifyLocation(owner, baseWin, ref loc);
                        rec = loc;
                        break;
                    }
            }

            return (result) ? rec : null;
        }

        public static bool EditRecord(IView owner, IBaseWindow baseWin, GDMRecord rec)
        {
            bool result = false;

            switch (rec.RecordType) {
                case GDMRecordType.rtIndividual:
                    GDMIndividualRecord ind = rec as GDMIndividualRecord;
                    result = ModifyIndividual(owner, baseWin, ref ind, null, TargetMode.tmNone, GDMSex.svUnknown);
                    break;

                case GDMRecordType.rtFamily:
                    GDMFamilyRecord fam = rec as GDMFamilyRecord;
                    result = ModifyFamily(owner, baseWin, ref fam, TargetMode.tmNone, null);
                    break;

                case GDMRecordType.rtNote:
                    GDMNoteRecord note = rec as GDMNoteRecord;
                    result = ModifyNote(owner, baseWin, ref note);
                    break;

                case GDMRecordType.rtMultimedia:
                    GDMMultimediaRecord mmRec = rec as GDMMultimediaRecord;
                    result = ModifyMedia(owner, baseWin, ref mmRec);
                    break;

                case GDMRecordType.rtSource:
                    GDMSourceRecord src = rec as GDMSourceRecord;
                    result = ModifySource(owner, baseWin, ref src);
                    break;

                case GDMRecordType.rtRepository:
                    GDMRepositoryRecord rep = rec as GDMRepositoryRecord;
                    result = ModifyRepository(owner, baseWin, ref rep);
                    break;

                case GDMRecordType.rtGroup:
                    GDMGroupRecord grp = rec as GDMGroupRecord;
                    result = ModifyGroup(owner, baseWin, ref grp);
                    break;

                case GDMRecordType.rtResearch:
                    GDMResearchRecord rsr = rec as GDMResearchRecord;
                    result = ModifyResearch(owner, baseWin, ref rsr);
                    break;

                case GDMRecordType.rtTask:
                    GDMTaskRecord tsk = rec as GDMTaskRecord;
                    result = ModifyTask(owner, baseWin, ref tsk);
                    break;

                case GDMRecordType.rtCommunication:
                    GDMCommunicationRecord comm = rec as GDMCommunicationRecord;
                    result = ModifyCommunication(owner, baseWin, ref comm);
                    break;

                case GDMRecordType.rtLocation:
                    GDMLocationRecord loc = rec as GDMLocationRecord;
                    result = ModifyLocation(owner, baseWin, ref loc);
                    break;
            }

            return result;
        }

        private static string GetDeleteMessage(IBaseWindow baseWin, GDMRecord record)
        {
            string msg = "";
            switch (record.RecordType) {
                case GDMRecordType.rtIndividual:
                    msg = string.Format(LangMan.LS(LSID.LSID_PersonDeleteQuery), GKUtils.GetNameString(((GDMIndividualRecord)record), true, false));
                    break;

                case GDMRecordType.rtFamily:
                    msg = string.Format(LangMan.LS(LSID.LSID_FamilyDeleteQuery), GKUtils.GetFamilyString(baseWin.Context.Tree, (GDMFamilyRecord)record));
                    break;

                case GDMRecordType.rtNote: {
                        string value = GKUtils.TruncateStrings(((GDMNoteRecord)(record)).Lines, GKData.NOTE_NAME_MAX_LENGTH);
                        if (string.IsNullOrEmpty(value)) {
                            value = string.Format("#{0}", record.GetId().ToString());
                        }
                        msg = string.Format(LangMan.LS(LSID.LSID_NoteDeleteQuery), value);
                        break;
                    }

                case GDMRecordType.rtMultimedia:
                    msg = string.Format(LangMan.LS(LSID.LSID_MediaDeleteQuery), ((GDMMultimediaRecord)record).GetFileTitle());
                    break;

                case GDMRecordType.rtSource:
                    msg = string.Format(LangMan.LS(LSID.LSID_SourceDeleteQuery), ((GDMSourceRecord)record).ShortTitle);
                    break;

                case GDMRecordType.rtRepository:
                    msg = string.Format(LangMan.LS(LSID.LSID_RepositoryDeleteQuery), ((GDMRepositoryRecord)record).RepositoryName);
                    break;

                case GDMRecordType.rtGroup:
                    msg = string.Format(LangMan.LS(LSID.LSID_GroupDeleteQuery), ((GDMGroupRecord)record).GroupName);
                    break;

                case GDMRecordType.rtResearch:
                    msg = string.Format(LangMan.LS(LSID.LSID_ResearchDeleteQuery), ((GDMResearchRecord)record).ResearchName);
                    break;

                case GDMRecordType.rtTask:
                    msg = string.Format(LangMan.LS(LSID.LSID_TaskDeleteQuery),
                                        GKUtils.GetTaskGoalStr(baseWin.Context.Tree, (GDMTaskRecord)record));
                    break;

                case GDMRecordType.rtCommunication:
                    msg = string.Format(LangMan.LS(LSID.LSID_CommunicationDeleteQuery), ((GDMCommunicationRecord)record).CommName);
                    break;

                case GDMRecordType.rtLocation:
                    msg = string.Format(LangMan.LS(LSID.LSID_LocationDeleteQuery), ((GDMLocationRecord)record).LocationName);
                    break;
            }
            return msg;
        }

        public static bool DeleteRecord(IBaseWindow baseWin, GDMRecord record, bool confirm)
        {
            bool result;

            if (record == null) {
                result = false;
            } else {
                if (confirm && !AppHost.StdDialogs.ShowQuestionYN(GetDeleteMessage(baseWin, record))) {
                    result = false;
                } else {
                    result = baseWin.Context.DeleteRecord(record);
                }
            }

            return result;
        }

        public static bool AddIndividualFather(IView owner, IBaseWindow baseWin, ChangeTracker localUndoman, GDMIndividualRecord person)
        {
            bool result = false;

            GDMIndividualRecord father = baseWin.Context.SelectPerson(owner, person, TargetMode.tmChild, GDMSex.svMale);
            if (father != null) {
                GDMFamilyRecord family = baseWin.Context.GetChildFamily(person, true, father);
                if (family != null) {
                    var husb = baseWin.Context.Tree.GetPtrValue<GDMIndividualRecord>(family.Husband);
                    if (husb == null) {
                        // new family
                        result = localUndoman.DoOrdinaryOperation(OperationType.otFamilySpouseAttach, family, father);
                    } else {
                        // selected family with husband
                        Logger.WriteError("BaseController.AddFather(): fail, because family already has father");
                        result = true;
                    }
                }
            }

            return result;
        }

        public static bool DeleteIndividualFather(IBaseWindow baseWin, ChangeTracker localUndoman, GDMIndividualRecord person)
        {
            bool result = false;

            if (AppHost.StdDialogs.ShowQuestionYN(LangMan.LS(LSID.LSID_DetachFatherQuery))) {
                GDMFamilyRecord family = baseWin.Context.GetChildFamily(person, false, null);
                if (family != null) {
                    GDMIndividualRecord father = baseWin.Context.Tree.GetPtrValue(family.Husband);
                    result = localUndoman.DoOrdinaryOperation(OperationType.otFamilySpouseDetach, family, father);
                }
            }

            return result;
        }

        public static bool AddIndividualMother(IView owner, IBaseWindow baseWin, ChangeTracker localUndoman, GDMIndividualRecord person)
        {
            bool result = false;

            GDMIndividualRecord mother = baseWin.Context.SelectPerson(owner, person, TargetMode.tmChild, GDMSex.svFemale);
            if (mother != null) {
                GDMFamilyRecord family = baseWin.Context.GetChildFamily(person, true, mother);
                if (family != null) {
                    var wife = baseWin.Context.Tree.GetPtrValue<GDMIndividualRecord>(family.Wife);
                    if (wife == null) {
                        // new family
                        result = localUndoman.DoOrdinaryOperation(OperationType.otFamilySpouseAttach, family, mother);
                    } else {
                        // selected family with wife
                        Logger.WriteError("BaseController.AddMother(): fail, because family already has mother");
                        result = true;
                    }
                }
            }

            return result;
        }

        public static bool DeleteIndividualMother(IBaseWindow baseWin, ChangeTracker localUndoman, GDMIndividualRecord person)
        {
            bool result = false;

            if (AppHost.StdDialogs.ShowQuestionYN(LangMan.LS(LSID.LSID_DetachMotherQuery))) {
                GDMFamilyRecord family = baseWin.Context.GetChildFamily(person, false, null);
                if (family != null) {
                    GDMIndividualRecord mother = baseWin.Context.Tree.GetPtrValue(family.Wife);
                    result = localUndoman.DoOrdinaryOperation(OperationType.otFamilySpouseDetach, family, mother);
                }
            }

            return result;
        }


        public static bool AddFamilyHusband(IView owner, IBaseWindow baseWin, ChangeTracker localUndoman, GDMFamilyRecord family)
        {
            bool result = false;

            var wife = baseWin.Context.Tree.GetPtrValue(family.Wife);
            GDMIndividualRecord husband = baseWin.Context.SelectPerson(owner, wife, TargetMode.tmSpouse, GDMSex.svMale);
            if (husband != null && family.Husband.IsEmpty()) {
                result = localUndoman.DoOrdinaryOperation(OperationType.otFamilySpouseAttach, family, husband);
            }

            return result;
        }

        public static bool DeleteFamilyHusband(IBaseWindow baseWin, ChangeTracker localUndoman, GDMFamilyRecord family)
        {
            bool result = false;

            GDMIndividualRecord husband = baseWin.Context.Tree.GetPtrValue(family.Husband);
            if (!baseWin.Context.IsAvailableRecord(husband)) return false;

            if (AppHost.StdDialogs.ShowQuestionYN(LangMan.LS(LSID.LSID_DetachHusbandQuery))) {
                result = localUndoman.DoOrdinaryOperation(OperationType.otFamilySpouseDetach, family, husband);
            }

            return result;
        }

        public static bool AddFamilyWife(IView owner, IBaseWindow baseWin, ChangeTracker localUndoman, GDMFamilyRecord family)
        {
            bool result = false;

            var husband = baseWin.Context.Tree.GetPtrValue(family.Husband);
            GDMIndividualRecord wife = baseWin.Context.SelectPerson(owner, husband, TargetMode.tmSpouse, GDMSex.svFemale);
            if (wife != null && family.Wife.IsEmpty()) {
                result = localUndoman.DoOrdinaryOperation(OperationType.otFamilySpouseAttach, family, wife);
            }

            return result;
        }

        public static bool DeleteFamilyWife(IBaseWindow baseWin, ChangeTracker localUndoman, GDMFamilyRecord family)
        {
            bool result = false;

            GDMIndividualRecord wife = baseWin.Context.Tree.GetPtrValue(family.Wife);
            if (!baseWin.Context.IsAvailableRecord(wife)) return false;

            if (AppHost.StdDialogs.ShowQuestionYN(LangMan.LS(LSID.LSID_DetachWifeQuery))) {
                result = localUndoman.DoOrdinaryOperation(OperationType.otFamilySpouseDetach, family, wife);
            }

            return result;
        }

        public static bool SelectPortraitRegion(IView owner, IBaseWindow baseWin, GDMMultimediaLink mmLink)
        {
            bool result;
            using (var selectDlg = AppHost.ResolveDialog<IPortraitSelectDlg>(baseWin)) {
                selectDlg.MultimediaLink = mmLink;
                result = AppHost.Instance.ShowModalX(selectDlg, owner, false);
            }
            return result;
        }

        public static bool AddIndividualPortrait(IView owner, IBaseWindow baseWin, ChangeTracker localUndoman, GDMIndividualRecord iRec)
        {
            bool result = false;

            GDMMultimediaRecord mmRec = baseWin.Context.SelectRecord(owner, GDMRecordType.rtMultimedia, null) as GDMMultimediaRecord;
            if (mmRec == null) return false;

            // remove previous portrait link
            GDMMultimediaLink mmLink = iRec.GetPrimaryMultimediaLink();
            if (mmLink != null) {
                mmLink.IsPrimary = false;
            }

            // set new portrait link
            mmLink = iRec.SetPrimaryMultimediaLink(mmRec);

            // select portrait area
            result = SelectPortraitRegion(owner, baseWin, mmLink);

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

        public static void ShowRecMerge(IView owner, IBaseWindow baseWin, GDMRecord rec1, GDMRecord rec2)
        {
            try {
                baseWin.Context.BeginUpdate();
                using (var dlg = AppHost.Container.Resolve<IRecMergeDlg>(baseWin)) {
                    dlg.SetRec1(rec1);
                    dlg.SetRec2(rec2);
                    AppHost.Instance.ShowModalX(dlg, owner, false);
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
                AppHost.StdDialogs.ShowError(string.Format(LangMan.LS(LSID.LSID_DetectedDataLoop), res));
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

        #endregion
    }
}
