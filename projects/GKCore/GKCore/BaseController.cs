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
using GKUI.Contracts;

namespace GKCore
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class BaseController : IBaseController
    {
        internal BaseController()
        {
        }

        #region UI control functions

        public GEDCOMFamilyRecord SelectFamily(IBaseWindow baseWin, GEDCOMIndividualRecord target)
        {
            GEDCOMFamilyRecord result;

            try
            {
                using (var dlg = AppHub.Container.Resolve<IRecordSelectDialog>())
                {
                    dlg.InitDialog(baseWin);

                    dlg.Target = target;
                    dlg.NeedSex = GEDCOMSex.svNone;
                    dlg.TargetMode = TargetMode.tmChildToFamily;
                    dlg.Mode = GEDCOMRecordType.rtFamily;
                    if (AppHub.MainWindow.ShowModalX(dlg, false)) {
                        result = (dlg.ResultRecord as GEDCOMFamilyRecord);
                    } else {
                        result = null;
                    }
                }
            }
            catch (Exception ex)
            {
                baseWin.Host.LogWrite("BaseController.SelectFamily(): " + ex.Message);
                result = null;
            }

            return result;
        }

        public GEDCOMIndividualRecord SelectPerson(IBaseWindow baseWin,
                                                   GEDCOMIndividualRecord target,
                                                   TargetMode targetMode, GEDCOMSex needSex)
        {
            GEDCOMIndividualRecord result;

            try
            {
                using (var dlg = AppHub.Container.Resolve<IRecordSelectDialog>())
                {
                    dlg.InitDialog(baseWin);

                    dlg.Target = target;
                    dlg.NeedSex = needSex;
                    dlg.TargetMode = targetMode;
                    dlg.Mode = GEDCOMRecordType.rtIndividual;
                    if (AppHub.MainWindow.ShowModalX(dlg, false)) {
                        result = (dlg.ResultRecord as GEDCOMIndividualRecord);
                    } else {
                        result = null;
                    }
                }
            }
            catch (Exception ex)
            {
                baseWin.Host.LogWrite("BaseController.SelectPerson(): " + ex.Message);
                result = null;
            }

            return result;
        }

        public GEDCOMRecord SelectRecord(IBaseWindow baseWin, GEDCOMRecordType mode,
                                         params object[] args)
        {
            GEDCOMRecord result;

            try
            {
                using (var dlg = AppHub.Container.Resolve<IRecordSelectDialog>())
                {
                    dlg.InitDialog(baseWin);

                    dlg.Mode = mode;

                    if (args != null && args.Length > 0) {
                        dlg.FastFilter = (args[0] as string);
                    }

                    if (AppHub.MainWindow.ShowModalX(dlg, false)) {
                        result = dlg.ResultRecord;
                    } else {
                        result = null;
                    }
                }
            }
            catch (Exception ex)
            {
                baseWin.Host.LogWrite("BaseController.SelectRecord(): " + ex.Message);
                result = null;
            }

            return result;
        }

        #endregion

        #region Data modification functions

        private GEDCOMFamilyRecord GetFamilyBySpouse(GEDCOMTree tree, GEDCOMIndividualRecord newParent)
        {
            GEDCOMFamilyRecord result = null;

            int num = tree.RecordsCount;
            for (int i = 0; i < num; i++)
            {
                GEDCOMRecord rec = tree[i];

                if (rec.RecordType == GEDCOMRecordType.rtFamily)
                {
                    GEDCOMFamilyRecord fam = (GEDCOMFamilyRecord) rec;
                    GEDCOMIndividualRecord husb = fam.GetHusband();
                    GEDCOMIndividualRecord wife = fam.GetWife();
                    if (husb == newParent || wife == newParent)
                    {
                        string msg = string.Format(LangMan.LS(LSID.LSID_ParentsQuery), GKUtils.GetFamilyString(fam));
                        if (AppHub.StdDialogs.ShowQuestionYN(msg) == true)
                        {
                            result = fam;
                            break;
                        }
                    }
                }
            }

            return result;
        }

        public GEDCOMFamilyRecord GetChildFamily(GEDCOMTree tree,
                                                 GEDCOMIndividualRecord iChild,
                                                 bool canCreate,
                                                 GEDCOMIndividualRecord newParent)
        {
            GEDCOMFamilyRecord result = null;

            if (iChild != null)
            {
                if (iChild.ChildToFamilyLinks.Count != 0)
                {
                    result = iChild.ChildToFamilyLinks[0].Family;
                }
                else
                {
                    if (canCreate)
                    {
                        GEDCOMFamilyRecord fam = GetFamilyBySpouse(tree, newParent);
                        if (fam == null)
                        {
                            fam = tree.CreateFamily();
                        }
                        fam.AddChild(iChild);
                        result = fam;
                    }
                }
            }

            return result;
        }

        public GEDCOMFamilyRecord AddFamilyForSpouse(GEDCOMTree tree, GEDCOMIndividualRecord spouse)
        {
            if (tree == null)
                throw new ArgumentNullException("tree");

            if (spouse == null)
                throw new ArgumentNullException("spouse");

            GEDCOMSex sex = spouse.Sex;
            if (sex < GEDCOMSex.svMale || sex >= GEDCOMSex.svUndetermined)
            {
                AppHub.StdDialogs.ShowError(LangMan.LS(LSID.LSID_IsNotDefinedSex));
                return null;
            }

            GEDCOMFamilyRecord family = tree.CreateFamily();
            family.AddSpouse(spouse);
            return family;
        }

        public GEDCOMIndividualRecord AddChildForParent(IBaseWindow baseWin, GEDCOMIndividualRecord parent, GEDCOMSex needSex)
        {
            GEDCOMIndividualRecord resultChild = null;

            if (parent != null)
            {
                if (parent.SpouseToFamilyLinks.Count > 1)
                {
                    AppHub.StdDialogs.ShowError(LangMan.LS(LSID.LSID_ThisPersonHasSeveralFamilies));
                }
                else
                {
                    GEDCOMFamilyRecord family;

                    if (parent.SpouseToFamilyLinks.Count == 0)
                    {
                        //GKUtils.ShowError(LangMan.LS(LSID.LSID_IsNotFamilies));

                        family = AddFamilyForSpouse(baseWin.Tree, parent);
                        if (family == null) {
                            return null;
                        }
                    } else {
                        family = parent.SpouseToFamilyLinks[0].Family;
                    }

                    GEDCOMIndividualRecord child = SelectPerson(baseWin, family.GetHusband(), TargetMode.tmParent, needSex);

                    if (child != null && family.AddChild(child))
                    {
                        // this repetition necessary, because the call of CreatePersonDialog only works if person already has a father,
                        // what to call AddChild () is no; all this is necessary in order to in the namebook were correct patronymics.
                        AppHub.NamesTable.ImportNames(child);

                        resultChild = child;
                    }
                }
            }

            return resultChild;
        }

        public GEDCOMIndividualRecord SelectSpouseFor(IBaseWindow baseWin, GEDCOMIndividualRecord iRec)
        {
            if (iRec == null)
                throw new ArgumentNullException("iRec");

            GEDCOMSex needSex;
            switch (iRec.Sex)
            {
                case GEDCOMSex.svMale:
                    needSex = GEDCOMSex.svFemale;
                    break;

                case GEDCOMSex.svFemale:
                    needSex = GEDCOMSex.svMale;
                    break;

                default:
                    AppHub.StdDialogs.ShowError(LangMan.LS(LSID.LSID_IsNotDefinedSex));
                    return null;
            }

            GEDCOMIndividualRecord target = null;
            TargetMode targetMode = TargetMode.tmNone;
            if (needSex == GEDCOMSex.svFemale) {
                target = iRec;
                targetMode = TargetMode.tmWife;
            }

            GEDCOMIndividualRecord result = SelectPerson(baseWin, target, targetMode, needSex);
            return result;
        }

        #endregion

        #region Name and sex functions

        public string DefinePatronymic(IBaseContext context, string name, GEDCOMSex sex, bool confirm)
        {
            ICulture culture = context.Culture;
            if (!culture.HasPatronymic()) return string.Empty;

            string result = "";

            INamesTable namesTable = AppHub.NamesTable;

            NameEntry n = namesTable.FindName(name);
            if (n == null) {
                if (!confirm) {
                    return result;
                }

                n = namesTable.AddName(name);
            }

            switch (sex)
            {
                case GEDCOMSex.svMale:
                    result = n.M_Patronymic;
                    break;

                case GEDCOMSex.svFemale:
                    result = n.F_Patronymic;
                    break;
            }

            if (result == "") {
                if (!confirm) {
                    return result;
                }

                ModifyName(context, ref n);
            }

            switch (sex)
            {
                case GEDCOMSex.svMale:
                    result = n.M_Patronymic;
                    break;

                case GEDCOMSex.svFemale:
                    result = n.F_Patronymic;
                    break;
            }

            return result;
        }

        public GEDCOMSex DefineSex(IBaseContext context, string iName, string iPatr)
        {
            //ICulture culture = fContext.Culture;
            INamesTable namesTable = AppHub.NamesTable;

            GEDCOMSex result = namesTable.GetSexByName(iName);

            if (result == GEDCOMSex.svNone)
            {
                using (var dlg = AppHub.Container.Resolve<ISexCheckDlg>())
                {
                    dlg.IndividualName = iName + " " + iPatr;
                    result = context.Culture.GetSex(iName, iPatr, false);

                    dlg.Sex = result;
                    if (AppHub.MainWindow.ShowModalX(dlg, false))
                    {
                        result = dlg.Sex;

                        if (result != GEDCOMSex.svNone)
                        {
                            namesTable.SetNameSex(iName, result);
                        }
                    }
                }
            }

            return result;
        }

        public void CheckPersonSex(IBaseContext context, GEDCOMIndividualRecord iRec)
        {
            if (iRec == null)
                throw new ArgumentNullException("iRec");

            try {
                context.BeginUpdate();

                if (iRec.Sex == GEDCOMSex.svNone || iRec.Sex == GEDCOMSex.svUndetermined)
                {
                    string fFam, fName, fPatr;
                    GKUtils.GetNameParts(iRec, out fFam, out fName, out fPatr);
                    iRec.Sex = DefineSex(context, fName, fPatr);
                }
            } finally {
                context.EndUpdate();
            }
        }

        #endregion

        #region Data search

        public GEDCOMSourceRecord FindSource(GEDCOMTree tree, string sourceName)
        {
            GEDCOMSourceRecord result = null;

            int num = tree.RecordsCount;
            for (int i = 0; i < num; i++)
            {
                GEDCOMRecord rec = tree[i];

                if (rec.RecordType == GEDCOMRecordType.rtSource && ((GEDCOMSourceRecord) rec).FiledByEntry == sourceName)
                {
                    result = (rec as GEDCOMSourceRecord);
                    break;
                }
            }

            return result;
        }

        public void GetSourcesList(GEDCOMTree tree, StringList sources)
        {
            if (sources == null) return;

            sources.Clear();

            int num = tree.RecordsCount;
            for (int i = 0; i < num; i++)
            {
                GEDCOMRecord rec = tree[i];
                if (rec is GEDCOMSourceRecord)
                {
                    sources.AddObject((rec as GEDCOMSourceRecord).FiledByEntry, rec);
                }
            }
        }

        #endregion

        #region Modify routines

        public bool ModifyMedia(IBaseWindow baseWin, ref GEDCOMMultimediaRecord mediaRec)
        {
            bool result;

            try {
                baseWin.Context.BeginUpdate();
                GEDCOMTree tree = baseWin.Context.Tree;

                using (var dlg = AppHub.Container.Resolve<IMediaEditDlg>())
                {
                    dlg.InitDialog(baseWin);

                    bool exists = mediaRec != null;
                    if (!exists) {
                        mediaRec = new GEDCOMMultimediaRecord(tree, tree, "", "");
                        mediaRec.FileReferences.Add(new GEDCOMFileReferenceWithTitle(tree, mediaRec, "", ""));
                        mediaRec.InitNew();
                    }

                    try {
                        baseWin.LockRecord(mediaRec);

                        dlg.MediaRec = mediaRec;
                        result = (AppHub.MainWindow.ShowModalX(dlg, false));
                    } finally {
                        baseWin.UnlockRecord(mediaRec);
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

        public bool ModifyNote(IBaseWindow baseWin, ref GEDCOMNoteRecord noteRec)
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
                    baseWin.LockRecord(noteRec);

                    if (GlobalOptions.Instance.UseExtendedNotes) {
                        using (var dlg = AppHub.Container.Resolve<INoteEditDlgEx>())
                        {
                            dlg.InitDialog(baseWin);

                            dlg.NoteRecord = noteRec;
                            result = (AppHub.MainWindow.ShowModalX(dlg, false));
                        }
                    } else {
                        using (var dlg = AppHub.Container.Resolve<INoteEditDlg>())
                        {
                            dlg.InitDialog(baseWin);

                            dlg.NoteRecord = noteRec;
                            result = (AppHub.MainWindow.ShowModalX(dlg, false));
                        }
                    }
                } finally {
                    baseWin.UnlockRecord(noteRec);
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

        public bool ModifySource(IBaseWindow baseWin, ref GEDCOMSourceRecord sourceRec)
        {
            bool result;

            try {
                baseWin.Context.BeginUpdate();
                GEDCOMTree tree = baseWin.Context.Tree;

                using (var dlg = AppHub.Container.Resolve<ISourceEditDlg>())
                {
                    dlg.InitDialog(baseWin);

                    bool exists = sourceRec != null;
                    if (!exists) {
                        sourceRec = new GEDCOMSourceRecord(tree, tree, "", "");
                        sourceRec.InitNew();
                    }

                    try {
                        baseWin.LockRecord(sourceRec);

                        dlg.SourceRecord = sourceRec;
                        result = (AppHub.MainWindow.ShowModalX(dlg, false));
                    } finally {
                        baseWin.UnlockRecord(sourceRec);
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

        public bool ModifySourceCitation(IBaseWindow baseWin, ChangeTracker undoman, IGEDCOMStructWithLists _struct, ref GEDCOMSourceCitation cit)
        {
            bool result;

            try {
                baseWin.Context.BeginUpdate();
                GEDCOMTree tree = baseWin.Context.Tree;

                using (var dlg = AppHub.Container.Resolve<ISourceCitEditDlg>())
                {
                    dlg.InitDialog(baseWin);

                    bool exists = cit != null;
                    if (!exists) {
                        cit = new GEDCOMSourceCitation(tree, _struct as GEDCOMObject, "", "");
                    }

                    dlg.SourceCitation = cit;
                    result = AppHub.MainWindow.ShowModalX(dlg, false);

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

        public bool ModifyRepository(IBaseWindow baseWin, ref GEDCOMRepositoryRecord repRec)
        {
            bool result;

            try {
                baseWin.Context.BeginUpdate();
                GEDCOMTree tree = baseWin.Context.Tree;

                using (var dlg = AppHub.Container.Resolve<IRepositoryEditDlg>())
                {
                    dlg.InitDialog(baseWin);

                    bool exists = repRec != null;
                    if (!exists) {
                        repRec = new GEDCOMRepositoryRecord(tree, tree, "", "");
                        repRec.InitNew();
                    }

                    try {
                        baseWin.LockRecord(repRec);

                        dlg.Repository = repRec;
                        result = AppHub.MainWindow.ShowModalX(dlg, false);
                    } finally {
                        baseWin.UnlockRecord(repRec);
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

        public bool ModifyGroup(IBaseWindow baseWin, ref GEDCOMGroupRecord groupRec)
        {
            bool result;

            try {
                baseWin.Context.BeginUpdate();
                GEDCOMTree tree = baseWin.Context.Tree;

                using (var dlg = AppHub.Container.Resolve<IGroupEditDlg>())
                {
                    dlg.InitDialog(baseWin);

                    bool exists = groupRec != null;
                    if (!exists) {
                        groupRec = new GEDCOMGroupRecord(tree, tree, "", "");
                        groupRec.InitNew();
                    }

                    try {
                        baseWin.LockRecord(groupRec);

                        dlg.Group = groupRec;
                        result = (AppHub.MainWindow.ShowModalX(dlg, false));
                    } finally {
                        baseWin.UnlockRecord(groupRec);
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

        public bool ModifyResearch(IBaseWindow baseWin, ref GEDCOMResearchRecord researchRec)
        {
            bool result;

            try {
                baseWin.Context.BeginUpdate();
                GEDCOMTree tree = baseWin.Context.Tree;

                using (var dlg = AppHub.Container.Resolve<IResearchEditDlg>())
                {
                    dlg.InitDialog(baseWin);

                    bool exists = researchRec != null;
                    if (!exists) {
                        researchRec = new GEDCOMResearchRecord(tree, tree, "", "");
                        researchRec.InitNew();
                    }

                    try {
                        baseWin.LockRecord(researchRec);

                        dlg.Research = researchRec;
                        result = AppHub.MainWindow.ShowModalX(dlg, false);
                    } finally {
                        baseWin.UnlockRecord(researchRec);
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

        public bool ModifyTask(IBaseWindow baseWin, ref GEDCOMTaskRecord taskRec)
        {
            bool result;

            try {
                baseWin.Context.BeginUpdate();
                GEDCOMTree tree = baseWin.Context.Tree;

                using (var dlg = AppHub.Container.Resolve<ITaskEditDlg>())
                {
                    dlg.InitDialog(baseWin);

                    bool exists = taskRec != null;
                    if (!exists) {
                        taskRec = new GEDCOMTaskRecord(tree, tree, "", "");
                        taskRec.InitNew();
                    }

                    try {
                        baseWin.LockRecord(taskRec);

                        dlg.Task = taskRec;
                        result = AppHub.MainWindow.ShowModalX(dlg, false);
                    } finally {
                        baseWin.UnlockRecord(taskRec);
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

        public bool ModifyCommunication(IBaseWindow baseWin, ref GEDCOMCommunicationRecord commRec)
        {
            bool result;

            try {
                baseWin.Context.BeginUpdate();
                GEDCOMTree tree = baseWin.Context.Tree;

                using (var dlg = AppHub.Container.Resolve<ICommunicationEditDlg>())
                {
                    dlg.InitDialog(baseWin);

                    bool exists = commRec != null;
                    if (!exists) {
                        commRec = new GEDCOMCommunicationRecord(tree, tree, "", "");
                        commRec.InitNew();
                    }

                    try {
                        baseWin.LockRecord(commRec);

                        dlg.Communication = commRec;
                        result = AppHub.MainWindow.ShowModalX(dlg, false);
                    } finally {
                        baseWin.UnlockRecord(commRec);
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

        public bool ModifyLocation(IBaseWindow baseWin, ref GEDCOMLocationRecord locRec)
        {
            bool result;

            try {
                baseWin.Context.BeginUpdate();
                GEDCOMTree tree = baseWin.Context.Tree;

                using (var dlg = AppHub.Container.Resolve<ILocationEditDlg>())
                {
                    dlg.InitDialog(baseWin);

                    bool exists = locRec != null;
                    if (!exists) {
                        locRec = new GEDCOMLocationRecord(tree, tree, "", "");
                        locRec.InitNew();
                    }

                    try {
                        baseWin.LockRecord(locRec);

                        dlg.LocationRecord = locRec;
                        result = AppHub.MainWindow.ShowModalX(dlg, false);
                    } finally {
                        baseWin.UnlockRecord(locRec);
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

        private void PostProcessPerson(IBaseWindow baseWin, GEDCOMIndividualRecord indivRec)
        {
            AppHub.NamesTable.ImportNames(indivRec);

            IListManager listMan = baseWin.GetRecordsListManByType(GEDCOMRecordType.rtIndividual);
            IndividualListFilter iFilter = (IndividualListFilter)listMan.Filter;

            if (iFilter.SourceMode == FilterGroupMode.Selected)
            {
                GEDCOMSourceRecord src = baseWin.Tree.XRefIndex_Find(iFilter.SourceRef) as GEDCOMSourceRecord;
                if (src != null && AppHub.StdDialogs.ShowQuestionYN(LangMan.LS(LSID.LSID_IncludedSourceFilter)) == true)
                {
                    indivRec.AddSource(src, "", 0);
                }
            }

            if (iFilter.FilterGroupMode == FilterGroupMode.Selected)
            {
                GEDCOMGroupRecord grp = baseWin.Tree.XRefIndex_Find(iFilter.GroupRef) as GEDCOMGroupRecord;
                if (grp != null && AppHub.StdDialogs.ShowQuestionYN(LangMan.LS(LSID.LSID_IncludedGroupFilter)) == true)
                {
                    grp.AddMember(indivRec);
                }
            }
        }

        public bool ModifyIndividual(IBaseWindow baseWin, ref GEDCOMIndividualRecord indivRec,
                                     GEDCOMIndividualRecord target, TargetMode targetMode, GEDCOMSex needSex)
        {
            bool result;

            try {
                baseWin.Context.BeginUpdate();
                GEDCOMTree tree = baseWin.Context.Tree;

                using (var dlg = AppHub.Container.Resolve<IPersonEditDlg>())
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
                        baseWin.LockRecord(indivRec);

                        dlg.Person = indivRec;

                        if (targetMode != TargetMode.tmNone) {
                            if (needSex == GEDCOMSex.svMale || needSex == GEDCOMSex.svFemale) {
                                dlg.SetNeedSex(needSex);
                            }
                            dlg.TargetMode = targetMode;
                            dlg.Target = target;
                        }

                        result = (AppHub.MainWindow.ShowModalX(dlg, false));
                    } finally {
                        baseWin.UnlockRecord(indivRec);
                    }

                    if (!exists) {
                        if (result) {
                            PostProcessPerson(baseWin, indivRec);

                            tree.AddRecord(indivRec);
                        } else {
                            GEDCOMUtils.CleanIndividual(indivRec);
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

        public bool ModifyFamily(IBaseWindow baseWin, ref GEDCOMFamilyRecord familyRec, FamilyTarget targetType, GEDCOMIndividualRecord target)
        {
            bool result;

            try {
                baseWin.Context.BeginUpdate();
                GEDCOMTree tree = baseWin.Context.Tree;

                if (targetType == FamilyTarget.Spouse && target != null) {
                    GEDCOMSex sex = target.Sex;
                    if (sex < GEDCOMSex.svMale || sex >= GEDCOMSex.svUndetermined) {
                        AppHub.StdDialogs.ShowError(LangMan.LS(LSID.LSID_IsNotDefinedSex));
                        return false;
                    }
                }

                using (var dlg = AppHub.Container.Resolve<IFamilyEditDlg>())
                {
                    dlg.InitDialog(baseWin);

                    bool exists = (familyRec != null);
                    if (!exists) {
                        familyRec = new GEDCOMFamilyRecord(tree, tree, "", "");
                        familyRec.InitNew();
                    }

                    try {
                        baseWin.LockRecord(familyRec);

                        dlg.Family = familyRec;

                        if (targetType != FamilyTarget.None && target != null) {
                            dlg.SetTarget(targetType, target);
                        }

                        result = (AppHub.MainWindow.ShowModalX(dlg, false));
                    } finally {
                        baseWin.UnlockRecord(familyRec);
                    }

                    if (!exists) {
                        if (result) {
                            tree.AddRecord(familyRec);
                        } else {
                            GEDCOMUtils.CleanFamily(familyRec);
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

        public bool ModifyAddress(IBaseWindow baseWin, GEDCOMAddress address)
        {
            bool result;

            try {
                baseWin.Context.BeginUpdate();

                using (var dlg = AppHub.Container.Resolve<IAddressEditDlg>())
                {
                    dlg.InitDialog(baseWin);

                    dlg.Address = address;
                    result = (AppHub.MainWindow.ShowModalX(dlg, false));
                }
            } finally {
                baseWin.Context.EndUpdate();
            }

            return result;
        }

        public bool ModifyName(IBaseContext context, ref NameEntry nameEntry)
        {
            bool result;

            try {
                context.BeginUpdate();

                using (var dlg = AppHub.Container.Resolve<INameEditDlg>())
                {
                    dlg.IName = nameEntry;
                    result = AppHub.MainWindow.ShowModalX(dlg, false);
                }
            } finally {
                context.EndUpdate();
            }

            return result;
        }

        #endregion

        #region Data modification functions for UI

        public bool AddIndividualFather(IBaseWindow baseWin, ChangeTracker localUndoman, GEDCOMIndividualRecord person)
        {
            bool result = false;

            GEDCOMIndividualRecord father = SelectPerson(baseWin, person, TargetMode.tmChild, GEDCOMSex.svMale);
            if (father != null)
            {
                GEDCOMFamilyRecord family = GetChildFamily(baseWin.Tree, person, true, father);
                if (family != null)
                {
                    if (family.Husband.Value == null) {
                        // new family
                        result = localUndoman.DoOrdinaryOperation(OperationType.otFamilySpouseAttach, family, father);
                    } else {
                        // selected family with husband
                        baseWin.Host.LogWrite("BaseController.AddFather(): fail, because family already has father");
                        result = true;
                    }
                }
            }

            return result;
        }

        public bool DeleteIndividualFather(IBaseWindow baseWin, ChangeTracker localUndoman, GEDCOMIndividualRecord person)
        {
            bool result = false;

            if (AppHub.StdDialogs.ShowQuestionYN(LangMan.LS(LSID.LSID_DetachFatherQuery)) == true)
            {
                GEDCOMFamilyRecord family = GetChildFamily(baseWin.Tree, person, false, null);
                if (family != null)
                {
                    GEDCOMIndividualRecord father = family.GetHusband();
                    result = localUndoman.DoOrdinaryOperation(OperationType.otFamilySpouseDetach, family, father);
                }
            }

            return result;
        }

        public bool AddIndividualMother(IBaseWindow baseWin, ChangeTracker localUndoman, GEDCOMIndividualRecord person)
        {
            bool result = false;

            GEDCOMIndividualRecord mother = SelectPerson(baseWin, person, TargetMode.tmChild, GEDCOMSex.svFemale);
            if (mother != null) {
                GEDCOMFamilyRecord family = GetChildFamily(baseWin.Tree, person, true, mother);
                if (family != null) {
                    if (family.Wife.Value == null) {
                        // new family
                        result = localUndoman.DoOrdinaryOperation(OperationType.otFamilySpouseAttach, family, mother);
                    } else {
                        // selected family with wife
                        baseWin.Host.LogWrite("BaseController.AddMother(): fail, because family already has mother");
                        result = true;
                    }
                }
            }

            return result;
        }

        public bool DeleteIndividualMother(IBaseWindow baseWin, ChangeTracker localUndoman, GEDCOMIndividualRecord person)
        {
            bool result = false;

            if (AppHub.StdDialogs.ShowQuestionYN(LangMan.LS(LSID.LSID_DetachMotherQuery)) == true)
            {
                GEDCOMFamilyRecord family = GetChildFamily(baseWin.Tree, person, false, null);
                if (family != null)
                {
                    GEDCOMIndividualRecord mother = family.GetWife();
                    result = localUndoman.DoOrdinaryOperation(OperationType.otFamilySpouseDetach, family, mother);
                }
            }

            return result;
        }

        #endregion
    }
}
