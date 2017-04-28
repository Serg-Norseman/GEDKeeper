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
using GKCore.Lists;
using GKCore.Operations;
using GKCore.Options;
using GKCore.Types;
using GKCore.UIContracts;

namespace GKCore
{
    /// <summary>
    /// 
    /// </summary>
    public static class BaseController
    {
        #region Modify routines

        public static bool ModifyMedia(IBaseWindow baseWin, ref GEDCOMMultimediaRecord mediaRec)
        {
            bool result;

            try {
                baseWin.Context.BeginUpdate();
                GEDCOMTree tree = baseWin.Context.Tree;

                using (var dlg = AppHost.Container.Resolve<IMediaEditDlg>())
                {
                    dlg.InitDialog(baseWin);

                    bool exists = mediaRec != null;
                    if (!exists) {
                        mediaRec = new GEDCOMMultimediaRecord(tree, tree, "", "");
                        mediaRec.FileReferences.Add(new GEDCOMFileReferenceWithTitle(tree, mediaRec, "", ""));
                        mediaRec.InitNew();
                    }

                    try {
                        baseWin.Context.LockRecord(mediaRec);

                        dlg.MediaRec = mediaRec;
                        result = (AppHost.Instance.ShowModalX(dlg, false));
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

        public static bool ModifyNote(IBaseWindow baseWin, ref GEDCOMNoteRecord noteRec)
        {
            bool result;

            try {
                baseWin.Context.BeginUpdate();
                GEDCOMTree tree = baseWin.Context.Tree;

                bool exists = noteRec != null;
                if (!exists) {
                    noteRec = new GEDCOMNoteRecord(tree, tree, "", "");
                    noteRec.InitNew();
                }

                try {
                    baseWin.Context.LockRecord(noteRec);

                    if (GlobalOptions.Instance.UseExtendedNotes) {
                        using (var dlg = AppHost.Container.Resolve<INoteEditDlgEx>())
                        {
                            dlg.InitDialog(baseWin);

                            dlg.NoteRecord = noteRec;
                            result = (AppHost.Instance.ShowModalX(dlg, false));
                        }
                    } else {
                        using (var dlg = AppHost.Container.Resolve<INoteEditDlg>())
                        {
                            dlg.InitDialog(baseWin);

                            dlg.NoteRecord = noteRec;
                            result = (AppHost.Instance.ShowModalX(dlg, false));
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

        public static bool ModifySource(IBaseWindow baseWin, ref GEDCOMSourceRecord sourceRec)
        {
            bool result;

            try {
                baseWin.Context.BeginUpdate();
                GEDCOMTree tree = baseWin.Context.Tree;

                using (var dlg = AppHost.Container.Resolve<ISourceEditDlg>())
                {
                    dlg.InitDialog(baseWin);

                    bool exists = sourceRec != null;
                    if (!exists) {
                        sourceRec = new GEDCOMSourceRecord(tree, tree, "", "");
                        sourceRec.InitNew();
                    }

                    try {
                        baseWin.Context.LockRecord(sourceRec);

                        dlg.SourceRecord = sourceRec;
                        result = (AppHost.Instance.ShowModalX(dlg, false));
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

        public static bool ModifySourceCitation(IBaseWindow baseWin, ChangeTracker undoman, IGEDCOMStructWithLists _struct, ref GEDCOMSourceCitation cit)
        {
            bool result;

            try {
                baseWin.Context.BeginUpdate();
                GEDCOMTree tree = baseWin.Context.Tree;

                using (var dlg = AppHost.Container.Resolve<ISourceCitEditDlg>())
                {
                    dlg.InitDialog(baseWin);

                    bool exists = cit != null;
                    if (!exists) {
                        cit = new GEDCOMSourceCitation(tree, _struct as GEDCOMObject, "", "");
                    }

                    dlg.SourceCitation = cit;
                    result = AppHost.Instance.ShowModalX(dlg, false);

                    if (!exists) {
                        if (result) {
                            //_struct.SourceCitations.Add(cit);
                            result = undoman.DoOrdinaryOperation(OperationType.otRecordSourceCitAdd, (GEDCOMObject)_struct, cit);
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

        public static bool ModifyRepository(IBaseWindow baseWin, ref GEDCOMRepositoryRecord repRec)
        {
            bool result;

            try {
                baseWin.Context.BeginUpdate();
                GEDCOMTree tree = baseWin.Context.Tree;

                using (var dlg = AppHost.Container.Resolve<IRepositoryEditDlg>())
                {
                    dlg.InitDialog(baseWin);

                    bool exists = repRec != null;
                    if (!exists) {
                        repRec = new GEDCOMRepositoryRecord(tree, tree, "", "");
                        repRec.InitNew();
                    }

                    try {
                        baseWin.Context.LockRecord(repRec);

                        dlg.Repository = repRec;
                        result = AppHost.Instance.ShowModalX(dlg, false);
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

        public static bool ModifyGroup(IBaseWindow baseWin, ref GEDCOMGroupRecord groupRec)
        {
            bool result;

            try {
                baseWin.Context.BeginUpdate();
                GEDCOMTree tree = baseWin.Context.Tree;

                using (var dlg = AppHost.Container.Resolve<IGroupEditDlg>())
                {
                    dlg.InitDialog(baseWin);

                    bool exists = groupRec != null;
                    if (!exists) {
                        groupRec = new GEDCOMGroupRecord(tree, tree, "", "");
                        groupRec.InitNew();
                    }

                    try {
                        baseWin.Context.LockRecord(groupRec);

                        dlg.Group = groupRec;
                        result = (AppHost.Instance.ShowModalX(dlg, false));
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

        public static bool ModifyResearch(IBaseWindow baseWin, ref GEDCOMResearchRecord researchRec)
        {
            bool result;

            try {
                baseWin.Context.BeginUpdate();
                GEDCOMTree tree = baseWin.Context.Tree;

                using (var dlg = AppHost.Container.Resolve<IResearchEditDlg>())
                {
                    dlg.InitDialog(baseWin);

                    bool exists = researchRec != null;
                    if (!exists) {
                        researchRec = new GEDCOMResearchRecord(tree, tree, "", "");
                        researchRec.InitNew();
                    }

                    try {
                        baseWin.Context.LockRecord(researchRec);

                        dlg.Research = researchRec;
                        result = AppHost.Instance.ShowModalX(dlg, false);
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

        public static bool ModifyTask(IBaseWindow baseWin, ref GEDCOMTaskRecord taskRec)
        {
            bool result;

            try {
                baseWin.Context.BeginUpdate();
                GEDCOMTree tree = baseWin.Context.Tree;

                using (var dlg = AppHost.Container.Resolve<ITaskEditDlg>())
                {
                    dlg.InitDialog(baseWin);

                    bool exists = taskRec != null;
                    if (!exists) {
                        taskRec = new GEDCOMTaskRecord(tree, tree, "", "");
                        taskRec.InitNew();
                    }

                    try {
                        baseWin.Context.LockRecord(taskRec);

                        dlg.Task = taskRec;
                        result = AppHost.Instance.ShowModalX(dlg, false);
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

        public static bool ModifyCommunication(IBaseWindow baseWin, ref GEDCOMCommunicationRecord commRec)
        {
            bool result;

            try {
                baseWin.Context.BeginUpdate();
                GEDCOMTree tree = baseWin.Context.Tree;

                using (var dlg = AppHost.Container.Resolve<ICommunicationEditDlg>())
                {
                    dlg.InitDialog(baseWin);

                    bool exists = commRec != null;
                    if (!exists) {
                        commRec = new GEDCOMCommunicationRecord(tree, tree, "", "");
                        commRec.InitNew();
                    }

                    try {
                        baseWin.Context.LockRecord(commRec);

                        dlg.Communication = commRec;
                        result = AppHost.Instance.ShowModalX(dlg, false);
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

        public static bool ModifyLocation(IBaseWindow baseWin, ref GEDCOMLocationRecord locRec)
        {
            bool result;

            try {
                baseWin.Context.BeginUpdate();
                GEDCOMTree tree = baseWin.Context.Tree;

                using (var dlg = AppHost.Container.Resolve<ILocationEditDlg>())
                {
                    dlg.InitDialog(baseWin);

                    bool exists = locRec != null;
                    if (!exists) {
                        locRec = new GEDCOMLocationRecord(tree, tree, "", "");
                        locRec.InitNew();
                    }

                    try {
                        baseWin.Context.LockRecord(locRec);

                        dlg.LocationRecord = locRec;
                        result = AppHost.Instance.ShowModalX(dlg, false);
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

        private static void PostProcessPerson(IBaseWindow baseWin, GEDCOMIndividualRecord indivRec)
        {
            AppHost.NamesTable.ImportNames(indivRec);

            IListManager listMan = baseWin.GetRecordsListManByType(GEDCOMRecordType.rtIndividual);
            if (listMan == null) return;

            IndividualListFilter iFilter = (IndividualListFilter)listMan.Filter;

            if (iFilter.SourceMode == FilterGroupMode.Selected)
            {
                GEDCOMSourceRecord src = baseWin.Context.Tree.XRefIndex_Find(iFilter.SourceRef) as GEDCOMSourceRecord;
                if (src != null && AppHost.StdDialogs.ShowQuestionYN(LangMan.LS(LSID.LSID_IncludedSourceFilter)) == true)
                {
                    indivRec.AddSource(src, "", 0);
                }
            }

            if (iFilter.FilterGroupMode == FilterGroupMode.Selected)
            {
                GEDCOMGroupRecord grp = baseWin.Context.Tree.XRefIndex_Find(iFilter.GroupRef) as GEDCOMGroupRecord;
                if (grp != null && AppHost.StdDialogs.ShowQuestionYN(LangMan.LS(LSID.LSID_IncludedGroupFilter)) == true)
                {
                    grp.AddMember(indivRec);
                }
            }
        }

        public static bool ModifyIndividual(IBaseWindow baseWin, ref GEDCOMIndividualRecord indivRec,
                                     GEDCOMIndividualRecord target, TargetMode targetMode, GEDCOMSex needSex)
        {
            bool result;

            try {
                baseWin.Context.BeginUpdate();
                GEDCOMTree tree = baseWin.Context.Tree;

                using (var dlg = AppHost.Container.Resolve<IPersonEditDlg>())
                {
                    dlg.InitDialog(baseWin);

                    bool exists = (indivRec != null);
                    if (!exists) {
                        indivRec = new GEDCOMIndividualRecord(tree, tree, "", "");
                        indivRec.InitNew();

                        indivRec.AddPersonalName(new GEDCOMPersonalName(tree, indivRec, "", ""));
                        baseWin.Context.CreateEventEx(indivRec, "BIRT", "", "");
                    }

                    try {
                        baseWin.Context.LockRecord(indivRec);

                        dlg.Person = indivRec;

                        if (targetMode != TargetMode.tmNone) {
                            if (needSex == GEDCOMSex.svMale || needSex == GEDCOMSex.svFemale) {
                                dlg.SetNeedSex(needSex);
                            }
                            dlg.TargetMode = targetMode;
                            dlg.Target = target;
                        }

                        result = (AppHost.Instance.ShowModalX(dlg, false));
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

        public static bool ModifyFamily(IBaseWindow baseWin, ref GEDCOMFamilyRecord familyRec, TargetMode targetType, GEDCOMIndividualRecord target)
        {
            bool result;

            try {
                baseWin.Context.BeginUpdate();
                GEDCOMTree tree = baseWin.Context.Tree;

                if (targetType == TargetMode.tmFamilySpouse && target != null) {
                    GEDCOMSex sex = target.Sex;
                    if (sex < GEDCOMSex.svMale || sex >= GEDCOMSex.svUndetermined) {
                        AppHost.StdDialogs.ShowError(LangMan.LS(LSID.LSID_IsNotDefinedSex));
                        return false;
                    }
                }

                using (var dlg = AppHost.Container.Resolve<IFamilyEditDlg>())
                {
                    dlg.InitDialog(baseWin);

                    bool exists = (familyRec != null);
                    if (!exists) {
                        familyRec = new GEDCOMFamilyRecord(tree, tree, "", "");
                        familyRec.InitNew();
                    }

                    try {
                        baseWin.Context.LockRecord(familyRec);

                        dlg.Family = familyRec;

                        if (targetType != TargetMode.tmNone && target != null) {
                            dlg.SetTarget(targetType, target);
                        }

                        result = (AppHost.Instance.ShowModalX(dlg, false));
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

        public static bool ModifyAddress(IBaseWindow baseWin, GEDCOMAddress address)
        {
            bool result;

            try {
                baseWin.Context.BeginUpdate();

                using (var dlg = AppHost.Container.Resolve<IAddressEditDlg>())
                {
                    dlg.InitDialog(baseWin);

                    dlg.Address = address;
                    result = (AppHost.Instance.ShowModalX(dlg, false));
                }
            } finally {
                baseWin.Context.EndUpdate();
            }

            return result;
        }

        public static bool ModifyName(IBaseContext context, ref NameEntry nameEntry)
        {
            bool result;

            try {
                context.BeginUpdate();

                using (var dlg = AppHost.Container.Resolve<INameEditDlg>())
                {
                    dlg.IName = nameEntry;
                    result = AppHost.Instance.ShowModalX(dlg, false);
                }
            } finally {
                context.EndUpdate();
            }

            return result;
        }

        #endregion

        #region Data modification functions for UI

        public static bool AddRecord(IBaseWindow baseWin, GEDCOMRecordType rt, Target target, out GEDCOMRecord rec)
        {
            bool result = false;
            rec = null;

            switch (rt)
            {
                case GEDCOMRecordType.rtIndividual:
                    {
                        // FIXME: legacy code, checkit
                        if (target == null) {
                            target = new Target();
                            target.TargetMode = TargetMode.tmParent;
                        }

                        GEDCOMIndividualRecord indivRec = null;
                        result = ModifyIndividual(baseWin, ref indivRec, target.TargetIndividual, target.TargetMode, target.NeedSex);
                        rec = indivRec;
                        break;
                    }

                case GEDCOMRecordType.rtFamily:
                    {
                        if (target == null) {
                            target = new Target();
                        }

                        TargetMode famTarget = (target.TargetMode != TargetMode.tmFamilyChild) ? TargetMode.tmNone : target.TargetMode;

                        GEDCOMFamilyRecord fam = null;
                        result = ModifyFamily(baseWin, ref fam, famTarget, target.TargetIndividual);
                        rec = fam;
                        break;
                    }
                case GEDCOMRecordType.rtNote:
                    {
                        GEDCOMNoteRecord note = null;
                        result = ModifyNote(baseWin, ref note);
                        rec = note;
                        break;
                    }
                case GEDCOMRecordType.rtMultimedia:
                    {
                        GEDCOMMultimediaRecord mmRec = null;
                        result = ModifyMedia(baseWin, ref mmRec);
                        rec = mmRec;
                        break;
                    }
                case GEDCOMRecordType.rtSource:
                    {
                        GEDCOMSourceRecord src = null;
                        result = ModifySource(baseWin, ref src);
                        rec = src;
                        break;
                    }
                case GEDCOMRecordType.rtRepository:
                    {
                        GEDCOMRepositoryRecord rep = null;
                        result = ModifyRepository(baseWin, ref rep);
                        rec = rep;
                        break;
                    }
                case GEDCOMRecordType.rtGroup:
                    {
                        GEDCOMGroupRecord grp = null;
                        result = ModifyGroup(baseWin, ref grp);
                        rec = grp;
                        break;
                    }
                case GEDCOMRecordType.rtResearch:
                    {
                        GEDCOMResearchRecord rsr = null;
                        result = ModifyResearch(baseWin, ref rsr);
                        rec = rsr;
                        break;
                    }
                case GEDCOMRecordType.rtTask:
                    {
                        GEDCOMTaskRecord tsk = null;
                        result = ModifyTask(baseWin, ref tsk);
                        rec = tsk;
                        break;
                    }
                case GEDCOMRecordType.rtCommunication:
                    {
                        GEDCOMCommunicationRecord comm = null;
                        result = ModifyCommunication(baseWin, ref comm);
                        rec = comm;
                        break;
                    }
                case GEDCOMRecordType.rtLocation:
                    {
                        GEDCOMLocationRecord loc = null;
                        result = ModifyLocation(baseWin, ref loc);
                        rec = loc;
                        break;
                    }
            }

            return result;
        }

        public static bool EditRecord(IBaseWindow baseWin, GEDCOMRecord rec)
        {
            bool result = false;

            switch (rec.RecordType) {
                case GEDCOMRecordType.rtIndividual:
                    GEDCOMIndividualRecord ind = rec as GEDCOMIndividualRecord;
                    result = ModifyIndividual(baseWin, ref ind, null, TargetMode.tmNone, GEDCOMSex.svNone);
                    break;

                case GEDCOMRecordType.rtFamily:
                    GEDCOMFamilyRecord fam = rec as GEDCOMFamilyRecord;
                    result = ModifyFamily(baseWin, ref fam, TargetMode.tmNone, null);
                    break;

                case GEDCOMRecordType.rtNote:
                    GEDCOMNoteRecord note = rec as GEDCOMNoteRecord;
                    result = ModifyNote(baseWin, ref note);
                    break;

                case GEDCOMRecordType.rtMultimedia:
                    GEDCOMMultimediaRecord mmRec = rec as GEDCOMMultimediaRecord;
                    result = ModifyMedia(baseWin, ref mmRec);
                    break;

                case GEDCOMRecordType.rtSource:
                    GEDCOMSourceRecord src = rec as GEDCOMSourceRecord;
                    result = ModifySource(baseWin, ref src);
                    break;

                case GEDCOMRecordType.rtRepository:
                    GEDCOMRepositoryRecord rep = rec as GEDCOMRepositoryRecord;
                    result = ModifyRepository(baseWin, ref rep);
                    break;

                case GEDCOMRecordType.rtGroup:
                    GEDCOMGroupRecord grp = rec as GEDCOMGroupRecord;
                    result = ModifyGroup(baseWin, ref grp);
                    break;

                case GEDCOMRecordType.rtResearch:
                    GEDCOMResearchRecord rsr = rec as GEDCOMResearchRecord;
                    result = ModifyResearch(baseWin, ref rsr);
                    break;

                case GEDCOMRecordType.rtTask:
                    GEDCOMTaskRecord tsk = rec as GEDCOMTaskRecord;
                    result = ModifyTask(baseWin, ref tsk);
                    break;

                case GEDCOMRecordType.rtCommunication:
                    GEDCOMCommunicationRecord comm = rec as GEDCOMCommunicationRecord;
                    result = ModifyCommunication(baseWin, ref comm);
                    break;

                case GEDCOMRecordType.rtLocation:
                    GEDCOMLocationRecord loc = rec as GEDCOMLocationRecord;
                    result = ModifyLocation(baseWin, ref loc);
                    break;
            }

            return result;
        }

        public static bool DeleteRecord(IBaseWindow baseWin, GEDCOMRecord record, bool confirm)
        {
            bool result = false;

            if (record != null)
            {
                //string xref = record.XRef;
                string msg = "";
                switch (record.RecordType)
                {
                    case GEDCOMRecordType.rtIndividual:
                        msg = string.Format(LangMan.LS(LSID.LSID_PersonDeleteQuery), GKUtils.GetNameString(((GEDCOMIndividualRecord)record), true, false));
                        break;

                    case GEDCOMRecordType.rtFamily:
                        msg = string.Format(LangMan.LS(LSID.LSID_FamilyDeleteQuery), GKUtils.GetFamilyString((GEDCOMFamilyRecord)record));
                        break;

                    case GEDCOMRecordType.rtNote:
                        {
                            string value = GKUtils.TruncateStrings(((GEDCOMNoteRecord) (record)).Note, GKData.NOTE_NAME_MAX_LENGTH);
                            if (string.IsNullOrEmpty(value))
                            {
                                value = string.Format("#{0}", record.GetId().ToString());
                            }
                            msg = string.Format(LangMan.LS(LSID.LSID_NoteDeleteQuery), value);
                            break;
                        }

                    case GEDCOMRecordType.rtMultimedia:
                        msg = string.Format(LangMan.LS(LSID.LSID_MediaDeleteQuery), ((GEDCOMMultimediaRecord)record).GetFileTitle());
                        break;

                    case GEDCOMRecordType.rtSource:
                        msg = string.Format(LangMan.LS(LSID.LSID_SourceDeleteQuery), ((GEDCOMSourceRecord)record).FiledByEntry);
                        break;

                    case GEDCOMRecordType.rtRepository:
                        msg = string.Format(LangMan.LS(LSID.LSID_RepositoryDeleteQuery), ((GEDCOMRepositoryRecord)record).RepositoryName);
                        break;

                    case GEDCOMRecordType.rtGroup:
                        msg = string.Format(LangMan.LS(LSID.LSID_GroupDeleteQuery), ((GEDCOMGroupRecord)record).GroupName);
                        break;

                    case GEDCOMRecordType.rtResearch:
                        msg = string.Format(LangMan.LS(LSID.LSID_ResearchDeleteQuery), ((GEDCOMResearchRecord)record).ResearchName);
                        break;

                    case GEDCOMRecordType.rtTask:
                        msg = string.Format(LangMan.LS(LSID.LSID_TaskDeleteQuery), GKUtils.GetTaskGoalStr((GEDCOMTaskRecord)record));
                        break;

                    case GEDCOMRecordType.rtCommunication:
                        msg = string.Format(LangMan.LS(LSID.LSID_CommunicationDeleteQuery), ((GEDCOMCommunicationRecord)record).CommName);
                        break;

                    case GEDCOMRecordType.rtLocation:
                        msg = string.Format(LangMan.LS(LSID.LSID_LocationDeleteQuery), ((GEDCOMLocationRecord)record).LocationName);
                        break;
                }

                if (confirm && AppHost.StdDialogs.ShowQuestionYN(msg) != true)
                    return false;

                baseWin.NotifyRecord(record, RecordAction.raDelete);

                result = baseWin.Context.DeleteRecord(record);

                if (result) {
                    baseWin.Modified = true;
                    baseWin.Context.Tree.Header.TransmissionDateTime = DateTime.Now;
                }
            }

            return result;
        }

        public static bool AddIndividualFather(IBaseWindow baseWin, ChangeTracker localUndoman, GEDCOMIndividualRecord person)
        {
            bool result = false;

            GEDCOMIndividualRecord father = baseWin.Context.SelectPerson(person, TargetMode.tmChild, GEDCOMSex.svMale);
            if (father != null)
            {
                GEDCOMFamilyRecord family = baseWin.Context.GetChildFamily(person, true, father);
                if (family != null)
                {
                    if (family.Husband.Value == null) {
                        // new family
                        result = localUndoman.DoOrdinaryOperation(OperationType.otFamilySpouseAttach, family, father);
                    } else {
                        // selected family with husband
                        Logger.LogWrite("BaseController.AddFather(): fail, because family already has father");
                        result = true;
                    }
                }
            }

            return result;
        }

        public static bool DeleteIndividualFather(IBaseWindow baseWin, ChangeTracker localUndoman, GEDCOMIndividualRecord person)
        {
            bool result = false;

            if (AppHost.StdDialogs.ShowQuestionYN(LangMan.LS(LSID.LSID_DetachFatherQuery)))
            {
                GEDCOMFamilyRecord family = baseWin.Context.GetChildFamily(person, false, null);
                if (family != null)
                {
                    GEDCOMIndividualRecord father = family.GetHusband();
                    result = localUndoman.DoOrdinaryOperation(OperationType.otFamilySpouseDetach, family, father);
                }
            }

            return result;
        }

        public static bool AddIndividualMother(IBaseWindow baseWin, ChangeTracker localUndoman, GEDCOMIndividualRecord person)
        {
            bool result = false;

            GEDCOMIndividualRecord mother = baseWin.Context.SelectPerson(person, TargetMode.tmChild, GEDCOMSex.svFemale);
            if (mother != null) {
                GEDCOMFamilyRecord family = baseWin.Context.GetChildFamily(person, true, mother);
                if (family != null) {
                    if (family.Wife.Value == null) {
                        // new family
                        result = localUndoman.DoOrdinaryOperation(OperationType.otFamilySpouseAttach, family, mother);
                    } else {
                        // selected family with wife
                        Logger.LogWrite("BaseController.AddMother(): fail, because family already has mother");
                        result = true;
                    }
                }
            }

            return result;
        }

        public static bool DeleteIndividualMother(IBaseWindow baseWin, ChangeTracker localUndoman, GEDCOMIndividualRecord person)
        {
            bool result = false;

            if (AppHost.StdDialogs.ShowQuestionYN(LangMan.LS(LSID.LSID_DetachMotherQuery)))
            {
                GEDCOMFamilyRecord family = baseWin.Context.GetChildFamily(person, false, null);
                if (family != null)
                {
                    GEDCOMIndividualRecord mother = family.GetWife();
                    result = localUndoman.DoOrdinaryOperation(OperationType.otFamilySpouseDetach, family, mother);
                }
            }

            return result;
        }


        public static bool AddFamilyHusband(IBaseWindow baseWin, ChangeTracker localUndoman, GEDCOMFamilyRecord family)
        {
            bool result = false;

            GEDCOMIndividualRecord husband = baseWin.Context.SelectPerson(null, TargetMode.tmNone, GEDCOMSex.svMale);
            if (husband != null && family.Husband.StringValue == "")
            {
                result = localUndoman.DoOrdinaryOperation(OperationType.otFamilySpouseAttach, family, husband);
            }

            return result;
        }

        public static bool DeleteFamilyHusband(IBaseWindow baseWin, ChangeTracker localUndoman, GEDCOMFamilyRecord family)
        {
            bool result = false;

            GEDCOMIndividualRecord husband = family.GetHusband();
            if (!baseWin.Context.IsAvailableRecord(husband)) return false;

            if (AppHost.StdDialogs.ShowQuestionYN(LangMan.LS(LSID.LSID_DetachHusbandQuery)))
            {
                result = localUndoman.DoOrdinaryOperation(OperationType.otFamilySpouseDetach, family, husband);
            }

            return result;
        }

        public static bool AddFamilyWife(IBaseWindow baseWin, ChangeTracker localUndoman, GEDCOMFamilyRecord family)
        {
            bool result = false;

            GEDCOMIndividualRecord wife = baseWin.Context.SelectPerson(null, TargetMode.tmNone, GEDCOMSex.svFemale);
            if (wife != null && family.Wife.StringValue == "")
            {
                result = localUndoman.DoOrdinaryOperation(OperationType.otFamilySpouseAttach, family, wife);
            }

            return result;
        }

        public static bool DeleteFamilyWife(IBaseWindow baseWin, ChangeTracker localUndoman, GEDCOMFamilyRecord family)
        {
            bool result = false;

            GEDCOMIndividualRecord wife = family.GetWife();
            if (!baseWin.Context.IsAvailableRecord(wife)) return false;

            if (AppHost.StdDialogs.ShowQuestionYN(LangMan.LS(LSID.LSID_DetachWifeQuery)))
            {
                result = localUndoman.DoOrdinaryOperation(OperationType.otFamilySpouseDetach, family, wife);
            }

            return result;
        }

        public static bool AddIndividualPortrait(IBaseWindow baseWin, GEDCOMIndividualRecord iRec)
        {
            bool result = false;

            GEDCOMMultimediaRecord mmRec = baseWin.Context.SelectRecord(GEDCOMRecordType.rtMultimedia, null) as GEDCOMMultimediaRecord;
            if (mmRec == null) return false;

            // remove previous portrait link
            GEDCOMMultimediaLink mmLink = iRec.GetPrimaryMultimediaLink();
            if (mmLink != null) {
                mmLink.IsPrimary = false;
            }

            // set new portrait link
            mmLink = iRec.SetPrimaryMultimediaLink(mmRec);

            // select portrait area
            using (var selectDlg = AppHost.Container.Resolve<IPortraitSelectDlg>())
            {
                selectDlg.InitDialog(baseWin);
                selectDlg.MultimediaLink = mmLink;
                result = selectDlg.ShowModalX();
            }

            return result;
        }

        public static bool DeleteIndividualPortrait(IBaseWindow baseWin, GEDCOMIndividualRecord iRec)
        {
            GEDCOMMultimediaLink mmLink = iRec.GetPrimaryMultimediaLink();
            if (mmLink == null) return false;

            mmLink.IsPrimary = false;
            return true;
        }

        #endregion
    }
}
