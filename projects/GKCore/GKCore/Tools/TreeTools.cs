/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2019 by Sergey V. Zhdanovskih.
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
using System.Threading;
using BSLib;
using BSLib.SmartGraph;
using GDModel;
using GDModel.Providers.GEDCOM;
using GKCore.Interfaces;
using GKCore.MVP.Controls;
using GKCore.Types;

namespace GKCore.Tools
{
    /// <summary>
    /// 
    /// </summary>
    public static class TreeTools
    {
        #region Patriarchs Search

        public static bool PL_SearchAnc(GDMIndividualRecord descendant, GDMIndividualRecord searchRec, bool onlyMaleLine)
        {
            if (descendant == null) return false;

            bool res = (descendant == searchRec);

            if (!res && descendant.ChildToFamilyLinks.Count > 0)
            {
                GDMFamilyRecord family = descendant.ChildToFamilyLinks[0].Family;

                GDMIndividualRecord ancestor = family.GetHusband();
                if (ancestor != null) {
                    res = PL_SearchAnc(ancestor, searchRec, onlyMaleLine);
                    if (res) return true;
                }

                /*if (!onlyMaleLine) {
					ancestor = family.GetWife();
					if (ancestor != null) {
						res = PL_SearchAnc2(ancestor, searchRec, onlyMaleLine);
						if (res) return true;
					}
				}*/
            }

            return res;
        }

        /// <summary>
        /// Search of crossing of two individuals.
        /// </summary>
        /// <param name="ancestorRec"></param>
        /// <param name="searchRec"></param>
        /// <returns>crossing of two individuals</returns>
        public static GDMIndividualRecord PL_SearchDesc(GDMIndividualRecord ancestorRec, GDMIndividualRecord searchRec)
        {
            GDMIndividualRecord cross = null;

            int num = ancestorRec.SpouseToFamilyLinks.Count;
            for (int i = 0; i < num; i++)
            {
                GDMFamilyRecord family = ancestorRec.SpouseToFamilyLinks[i].Family;
                GDMIndividualRecord spouse = family.GetSpouseBy(ancestorRec);

                bool res;
                if (spouse != null)
                {
                    res = PL_SearchAnc(spouse, searchRec, (ancestorRec.Sex == GDMSex.svFemale));
                    if (res) {
                        cross = ancestorRec;
                        return cross;
                    }
                }

                if (ancestorRec.Sex == GDMSex.svMale) {
                    int num2 = family.Children.Count;
                    for (int j = 0; j < num2; j++)
                    {
                        GDMIndividualRecord child = family.Children[j].Value as GDMIndividualRecord;

                        cross = PL_SearchDesc(child, searchRec);
                        if (cross != null) return cross;
                    }
                }
            }

            return null;
        }

        public static GDMFamilyRecord PL_SearchIntersection(GDMIndividualRecord ancestor, GDMIndividualRecord searchRec)
        {
            int num = ancestor.SpouseToFamilyLinks.Count;
            for (int i = 0; i < num; i++)
            {
                GDMFamilyRecord family = ancestor.SpouseToFamilyLinks[i].Family;
                GDMIndividualRecord spouse = family.GetSpouseBy(ancestor);

                if (spouse != null)
                {
                    bool res = PL_SearchAnc(spouse, searchRec, (ancestor.Sex == GDMSex.svFemale));
                    if (res) return family;
                }

                if (ancestor.Sex == GDMSex.svMale)
                {
                    int num2 = family.Children.Count;
                    for (int j = 0; j < num2; j++)
                    {
                        GDMIndividualRecord child = family.Children[j].Value as GDMIndividualRecord;

                        GDMFamilyRecord res = PL_SearchIntersection(child, searchRec);
                        if (res != null) return res;
                    }
                }
            }

            return null;
        }

        public static void GenPatriarchsGraphviz(IBaseWindow baseWin, string outpath, int minGens, bool loneSuppress = true)
        {
            if (baseWin == null)
                throw new ArgumentNullException("baseWin");

            string[] options = { "ratio=auto" };
            GraphvizWriter gvw = new GraphvizWriter("Family Tree", options);

            using (ExtList<PatriarchObj> patList = PatriarchsMan.GetPatriarchsLinks(baseWin.Context, minGens, false, loneSuppress))
            {
                int num = patList.Count;
                for (int i = 0; i < num; i++) {
                    PatriarchObj pObj = patList[i];

                    if (!loneSuppress || pObj.HasLinks) {
                        string color = (pObj.IRec.Sex == GDMSex.svFemale) ? "pink" : "blue";
                        gvw.WriteNode(pObj.IRec.XRef, GKUtils.GetNameString(pObj.IRec, true, false), "filled", color, "box");
                    }
                }

                for (int i = 0; i < num; i++) {
                    PatriarchObj pat1 = patList[i];

                    int num2 = pat1.Links.Count;
                    for (int k = 0; k < num2; k++) {
                        PatriarchObj pat2 = pat1.Links[k];
                        gvw.WriteEdge(pat1.IRec.XRef, pat2.IRec.XRef);
                    }
                }
            }

            gvw.SaveFile(outpath);
        }

        #endregion

        #region GEDCOM Format Verification

        private static void ReformNote(GDMTree tree, GDMNotes note)
        {
            GDMNoteRecord noteRec = tree.CreateNote();
            noteRec.Note = note.Notes;

            note.Clear();
            note.Value = noteRec;
        }

        private static void ReformMultimediaLink(GDMTree tree, GDMMultimediaLink mmLink)
        {
            string title = mmLink.Title;
            GDMMultimediaRecord mmRec = tree.CreateMultimedia();

            int num = mmLink.FileReferences.Count;
            for (int i = 0; i < num; i++) {
                GDMFileReference srcFileRef = mmLink.FileReferences[i];
                GDMFileReferenceWithTitle tgtFileRef = new GDMFileReferenceWithTitle(mmRec);

                tgtFileRef.LinkFile(srcFileRef.StringValue);

                if (srcFileRef.MultimediaFormat != GDMMultimediaFormat.mfNone) {
                    tgtFileRef.MultimediaFormat = srcFileRef.MultimediaFormat;
                }
                if (srcFileRef.MediaType != GDMMediaType.mtUnknown) {
                    tgtFileRef.MediaType = srcFileRef.MediaType;
                }
                tgtFileRef.Title = title;

                mmRec.FileReferences.Add(tgtFileRef);
            }

            mmLink.Clear();
            mmLink.Value = mmRec;
        }

        private static void ReformSourceCitation(GDMTree tree, GDMSourceCitation sourCit)
        {
            GDMSourceRecord sourRec = tree.CreateSource();

            StringList description = sourCit.Description;
            string page = sourCit.Page;
            int certaintyAssessment = sourCit.CertaintyAssessment;

            sourRec.Text = description;

            sourCit.Clear();
            sourCit.Value = sourRec;
            sourCit.Page = page;
            sourCit.CertaintyAssessment = certaintyAssessment;
        }

        private static void CheckRecord_PrepareTag(GDMTree tree, GEDCOMFormat format, GDMTagWithLists tag)
        {
            int num = tag.MultimediaLinks.Count;
            for (int i = 0; i < num; i++) {
                GDMMultimediaLink mmLink = tag.MultimediaLinks[i];
                if (!mmLink.IsPointer) ReformMultimediaLink(tree, mmLink);
            }

            num = tag.Notes.Count;
            for (int i = 0; i < num; i++) {
                GDMNotes note = tag.Notes[i];
                if (!note.IsPointer) ReformNote(tree, note);
            }

            num = tag.SourceCitations.Count;
            for (int i = 0; i < num; i++) {
                GDMSourceCitation sourCit = tag.SourceCitations[i];
                if (!sourCit.IsPointer) ReformSourceCitation(tree, sourCit);
            }
        }

        private static void CheckRecord_RepairTag(GDMTree tree, GEDCOMFormat format, GDMTagWithLists tag)
        {
            for (int i = tag.MultimediaLinks.Count - 1; i >= 0; i--) {
                GDMMultimediaLink mmLink = tag.MultimediaLinks[i];
                if (mmLink.IsPointer && mmLink.Value == null) tag.MultimediaLinks.DeleteAt(i);
            }

            for (int i = tag.Notes.Count - 1; i >= 0; i--) {
                GDMNotes note = tag.Notes[i];
                if (note.IsPointer && note.Value == null) tag.Notes.DeleteAt(i);
            }

            for (int i = tag.SourceCitations.Count - 1; i >= 0; i--) {
                GDMSourceCitation sourCit = tag.SourceCitations[i];
                if (sourCit.IsPointer && sourCit.Value == null) tag.SourceCitations.DeleteAt(i);
            }
        }

        private static void CheckRecord_PreparePtr(GDMTree tree, GEDCOMFormat format, GDMPointerWithNotes ptr)
        {
            // TODO: checkit!
            GDMRecord val = ptr.Value;
            if (!string.IsNullOrEmpty(ptr.XRef) && val == null) {
                ptr.Value = null;
            }

            int num = ptr.Notes.Count;
            for (int i = 0; i < num; i++) {
                GDMNotes note = ptr.Notes[i];
                if (!note.IsPointer) ReformNote(tree, note);
            }
        }

        private static void CheckRecord_IndiEvent(GDMCustomEvent evt, GEDCOMFormat format)
        {
            // Fix for Family Tree Maker 2008 which exports occupation as generic EVEN events
            if (format == GEDCOMFormat.gf_FamilyTreeMaker) {
                string subtype = evt.Classification;
                if (evt.Name == GEDCOMTagType.EVEN && subtype.ToLower() == "occupation") {
                    evt.SetName(GEDCOMTagType.OCCU);
                    evt.Classification = string.Empty;
                }
            }
        }

        private static void CheckRecord_EventPlace(GDMCustomEvent aEvent)
        {
            GDMPlace place = aEvent.FindTag(GEDCOMTagType.PLAC, 0) as GDMPlace;
            if (place == null) return;

            GDMPointer placeLocation = place.FindTag(GEDCOMTagType._LOC, 0) as GDMPointer;
            if (placeLocation == null) return;

            if (placeLocation.XRef != "" && placeLocation.Value == null) {
                placeLocation.XRef = "";
            }

            if (place.StringValue != "") {
                GDMLocationRecord loc = placeLocation.Value as GDMLocationRecord;
                if (loc != null && place.StringValue != loc.LocationName) {
                    place.StringValue = loc.LocationName;
                }
            }
        }

        private static void CheckRecord_AttrCompatible(GDMTree tree, GEDCOMFormat format, GDMIndividualRecord iRec, GDMCustomEvent aEvent)
        {
        }

        private static void CheckRecord_URefCompatible(GDMIndividualRecord iRec, GDMUserReference userRef)
        {
        }

        private static void CheckRecord_Name(GDMIndividualRecord iRec, GDMPersonalName persName, IBaseContext baseContext)
        {
            baseContext.CollectNameLangs(persName);
        }

        private static void CheckRecord_Individual(IBaseContext baseContext, GDMTree tree, GEDCOMFormat format,
                                                   GDMIndividualRecord iRec)
        {
            if (format == GEDCOMFormat.gf_Native) {
                for (int i = 0, num = iRec.Events.Count; i < num; i++) {
                    GDMCustomEvent evt = iRec.Events[i];

                    CheckRecord_EventPlace(evt);
                    CheckRecord_AttrCompatible(tree, format, iRec, evt);
                    CheckRecord_RepairTag(tree, format, evt);

                    baseContext.CollectEventValues(evt);
                }

                for (int i = 0, num = iRec.UserReferences.Count; i < num; i++) {
                    CheckRecord_URefCompatible(iRec, iRec.UserReferences[i]);
                }

                for (int i = 0, num = iRec.PersonalNames.Count; i < num; i++) {
                    CheckRecord_Name(iRec, iRec.PersonalNames[i], baseContext);
                }
            } else {
                for (int i = 0, num = iRec.Events.Count; i < num; i++) {
                    GDMCustomEvent evt = iRec.Events[i];

                    CheckRecord_IndiEvent(evt, format);
                    CheckRecord_PrepareTag(tree, format, evt);
                }

                for (int i = 0, num = iRec.ChildToFamilyLinks.Count; i < num; i++) {
                    CheckRecord_PreparePtr(tree, format, iRec.ChildToFamilyLinks[i]);
                }

                for (int i = 0, num = iRec.SpouseToFamilyLinks.Count; i < num; i++) {
                    CheckRecord_PreparePtr(tree, format, iRec.SpouseToFamilyLinks[i]);
                }

                for (int i = 0, num = iRec.Associations.Count; i < num; i++) {
                    CheckRecord_PreparePtr(tree, format, iRec.Associations[i]);
                }
            }

            for (int i = iRec.ChildToFamilyLinks.Count - 1; i >= 0; i--) {
                if (iRec.ChildToFamilyLinks[i].Family == null)
                    iRec.ChildToFamilyLinks.DeleteAt(i);
            }

            for (int i = iRec.SpouseToFamilyLinks.Count - 1; i >= 0; i--) {
                if (iRec.SpouseToFamilyLinks[i].Family == null)
                    iRec.SpouseToFamilyLinks.DeleteAt(i);
            }

            baseContext.ImportNames(iRec);
        }

        private static void CheckRecord_Family(IBaseContext baseContext, GDMTree tree, GEDCOMFormat format,
                                               GDMFamilyRecord fam)
        {
            if (format == GEDCOMFormat.gf_Native) {
                int num = fam.Events.Count;
                for (int i = 0; i < num; i++) {
                    CheckRecord_EventPlace(fam.Events[i]);
                }
            } else {
                int num2 = fam.Events.Count;
                for (int i = 0; i < num2; i++) {
                    CheckRecord_PrepareTag(tree, format, fam.Events[i]);
                }
            }

            for (int i = fam.Children.Count - 1; i >= 0; i--) {
                if (fam.Children[i].Value == null)
                    fam.Children.DeleteAt(i);
            }

            GDMRecord val = fam.Husband.Value;
            if (!string.IsNullOrEmpty(fam.Husband.XRef) && val == null) {
                fam.Husband.Value = null;
            }

            val = fam.Wife.Value;
            if (!string.IsNullOrEmpty(fam.Wife.XRef) && val == null) {
                fam.Wife.Value = null;
            }
        }

        private static void CheckRecord_Group(GDMGroupRecord group)
        {
            for (int i = group.Members.Count - 1; i >= 0; i--)
            {
                GDMPointer ptr = group.Members[i];
                GDMIndividualRecord irec = ptr.Value as GDMIndividualRecord;
                if (irec == null)
                {
                    group.Members.DeleteAt(i);
                }
                else
                {
                    if (irec.IndexOfGroup(group) < 0)
                    {
                        group.Members.DeleteAt(i);
                    }
                }
            }
        }

        private static void CheckRecord_Source(GDMSourceRecord src)
        {
            for (int i = src.RepositoryCitations.Count - 1; i >= 0; i--) {
                GDMRecord val = src.RepositoryCitations[i].Value;
                if (val == null) {
                    src.RepositoryCitations.DeleteAt(i);
                }
            }
        }

        private static void CheckRecord_Multimedia(GDMMultimediaRecord mmRec, GEDCOMFormat format, int fileVer)
        {
            for (int i = 0; i < mmRec.FileReferences.Count; i++) {
                GDMFileReferenceWithTitle fileRef = mmRec.FileReferences[i];

                GDMMultimediaFormat mmFormat = fileRef.MultimediaFormat;
                if (mmFormat == GDMMultimediaFormat.mfUnknown || mmFormat == GDMMultimediaFormat.mfNone) {
                    // tag 'FORM' can be corrupted or GEDCOMCore in past not recognize format attempt recovery
                    fileRef.MultimediaFormat = GDMFileReference.RecognizeFormat(fileRef.StringValue);
                }

                if (format == GEDCOMFormat.gf_Native && fileVer == 39) {
                    // the transition to normalized names after GKv39
                    // only for not direct references (platform specific paths)

                    var mediaStore = GKUtils.GetStoreType(fileRef);
                    if (mediaStore.StoreType != MediaStoreType.mstReference) {
                        fileRef.StringValue = FileHelper.NormalizeFilename(fileRef.StringValue);
                    }
                }
            }
        }

        private static void CheckRecord(IBaseContext baseContext, GDMTree tree, GDMRecord rec,
                                        GEDCOMFormat format, int fileVer)
        {
            if (format != GEDCOMFormat.gf_Native) {
                int num = rec.MultimediaLinks.Count;
                for (int i = 0; i < num; i++) {
                    GDMMultimediaLink mmLink = rec.MultimediaLinks[i];
                    if (!mmLink.IsPointer) ReformMultimediaLink(tree, mmLink);
                }

                num = rec.Notes.Count;
                for (int i = 0; i < num; i++) {
                    GDMNotes note = rec.Notes[i];
                    if (!note.IsPointer) ReformNote(tree, note);
                }

                num = rec.SourceCitations.Count;
                for (int i = 0; i < num; i++) {
                    GDMSourceCitation sourCit = rec.SourceCitations[i];
                    if (!sourCit.IsPointer) ReformSourceCitation(tree, sourCit);
                }
            }

            switch (rec.RecordType) {
                case GDMRecordType.rtIndividual:
                    CheckRecord_Individual(baseContext, tree, format, rec as GDMIndividualRecord);
                    break;

                case GDMRecordType.rtFamily:
                    CheckRecord_Family(baseContext, tree, format, rec as GDMFamilyRecord);
                    break;

                case GDMRecordType.rtGroup:
                    CheckRecord_Group(rec as GDMGroupRecord);
                    break;

                case GDMRecordType.rtSource:
                    CheckRecord_Source(rec as GDMSourceRecord);
                    break;

                case GDMRecordType.rtMultimedia:
                    CheckRecord_Multimedia(rec as GDMMultimediaRecord, format, fileVer);
                    break;
            }
        }

        private static bool CheckRecordXRef(GDMRecord record)
        {
            string stdSign = GEDCOMUtils.GetSignByRecord(record);
            string xrefNum = record.GetXRefNum();
            string recXRef = record.XRef;

            return ((recXRef == stdSign + xrefNum) && record.GetId() > 0);
        }

        private static void CorrectIds(GDMTree tree, IProgressController pc)
        {
            pc.ProgressInit(LangMan.LS(LSID.LSID_IDsCorrect), tree.RecordsCount);
            GDMXRefReplacer repMap = new GDMXRefReplacer();
            try {
                int num = tree.RecordsCount;
                for (int i = 0; i < num; i++) {
                    GDMRecord rec = tree[i];
                    if (!CheckRecordXRef(rec)) {
                        string newXRef = tree.XRefIndex_NewXRef(rec);
                        repMap.AddXRef(rec, rec.XRef, newXRef);
                        rec.XRef = newXRef;
                    }
                    pc.ProgressStep();
                }

                tree.Header.ReplaceXRefs(repMap);
                pc.ProgressInit(LangMan.LS(LSID.LSID_IDsCorrect), repMap.Count);

                int num2 = repMap.Count;
                for (int i = 0; i < num2; i++) {
                    GDMRecord rec = repMap[i].Rec;
                    rec.ReplaceXRefs(repMap);
                    pc.ProgressStep();
                }
            } finally {
                repMap.Dispose();
                pc.ProgressDone();
            }
        }

        public static bool CheckGEDCOMFormat(GDMTree tree, IBaseContext baseContext, IProgressController pc)
        {
            if (tree == null)
                throw new ArgumentNullException("tree");

            if (baseContext == null)
                throw new ArgumentNullException("baseContext");

            if (pc == null)
                throw new ArgumentNullException("pc");

            bool result = false;

            try {
                pc.ProgressInit(LangMan.LS(LSID.LSID_FormatCheck), 100);
                try {
                    GEDCOMFormat format = GEDCOMProvider.GetGEDCOMFormat(tree);
                    int fileVer;

                    // remove a deprecated features
                    if (format == GEDCOMFormat.gf_Native) {
                        GDMHeader header = tree.Header;
                        GDMTag tag;

                        tag = header.FindTag("_ADVANCED", 0);
                        if (tag != null) header.DeleteTag("_ADVANCED");

                        tag = header.FindTag("_EXT_NAME", 0);
                        if (tag != null) header.DeleteTag("_EXT_NAME");

                        fileVer = ConvertHelper.ParseInt(header.SourceVersion, GKData.APP_FORMAT_DEFVER);
                    } else {
                        fileVer = -1;
                    }

                    bool xrefValid = true;

                    int progress = 0;
                    int num = tree.RecordsCount;
                    for (int i = 0; i < num; i++) {
                        GDMRecord rec = tree[i];
                        CheckRecord(baseContext, tree, rec, format, fileVer);

                        if (format != GEDCOMFormat.gf_Native && xrefValid && !CheckRecordXRef(rec)) {
                            xrefValid = false;
                        }

                        int newProgress = (int)Math.Min(100, ((i + 1) * 100.0f) / num);
                        if (progress != newProgress) {
                            progress = newProgress;
                            pc.ProgressStep(progress);
                        }
                    }

                    // obsolete: AppHost.StdDialogs.ShowQuestionYN(LangMan.LS(LSID.LSID_IDsCorrectNeed))
                    if (!xrefValid) {
                        CorrectIds(tree, pc);
                    }

                    result = true;
                } finally {
                    pc.ProgressDone();
                }
            } catch (Exception ex) {
                Logger.LogWrite("TreeTools.CheckGEDCOMFormat(): " + ex.Message);
                AppHost.StdDialogs.ShowError(LangMan.LS(LSID.LSID_CheckGedComFailed));
            }

            return result;
        }

        #endregion

        #region Tree Walk

        public enum TreeWalkMode
        {
            twmAll,
            twmFamily,
            twmAncestors,
            twmDescendants,
            twmNone
        }

        public delegate bool WalkProc(GDMIndividualRecord iRec, TreeWalkMode mode, object extData);

        public static void WalkTree(GDMIndividualRecord iRec, TreeWalkMode mode, WalkProc walkProc, object extData)
        {
            if (iRec == null)
                throw new ArgumentNullException("iRec");

            if (walkProc == null)
                throw new ArgumentNullException("walkProc");

            if (extData == null)
                throw new ArgumentNullException("extData");

            WalkTreeInt(iRec, mode, walkProc, extData);
        }

        public static void WalkTree(GDMIndividualRecord iRec, TreeWalkMode mode, List<GDMRecord> walkList)
        {
            if (iRec == null)
                throw new ArgumentNullException("iRec");

            if (walkList == null)
                throw new ArgumentNullException("walkList");

            WalkTreeInt(iRec, mode, DefaultWalkProc, walkList);
        }

        private static bool DefaultWalkProc(GDMIndividualRecord iRec, TreeWalkMode mode, object extData)
        {
            List<GDMRecord> walkList = (List<GDMRecord>)extData;
            bool resContinue = (iRec != null && !walkList.Contains(iRec));
            if (resContinue) {
                walkList.Add(iRec);
            }
            return resContinue;
        }

        private static void WalkTreeInt(GDMIndividualRecord iRec, TreeWalkMode mode, WalkProc walkProc, object extData)
        {
            if (!walkProc(iRec, mode, extData)) return;

            if (mode == TreeWalkMode.twmNone) return;

            if (mode == TreeWalkMode.twmAll || mode == TreeWalkMode.twmAncestors) {
                GDMFamilyRecord family = iRec.GetParentsFamily();
                if (family != null) {
                    GDMIndividualRecord father, mother;
                    father = family.GetHusband();
                    mother = family.GetWife();

                    WalkTreeInt(father, mode, walkProc, extData);
                    WalkTreeInt(mother, mode, walkProc, extData);
                }
            }

            // twmAll, twmFamily, twmDescendants
            if (mode < TreeWalkMode.twmAncestors || mode == TreeWalkMode.twmDescendants) {
                int num = iRec.SpouseToFamilyLinks.Count;
                for (int i = 0; i < num; i++) {
                    GDMFamilyRecord family = iRec.SpouseToFamilyLinks[i].Family;
                    GDMIndividualRecord spouse = ((iRec.Sex == GDMSex.svMale) ? family.GetWife() : family.GetHusband());

                    TreeWalkMode intMode = ((mode == TreeWalkMode.twmAll) ? TreeWalkMode.twmAll : TreeWalkMode.twmNone);
                    WalkTreeInt(spouse, intMode, walkProc, extData);

                    switch (mode) {
                        case TreeWalkMode.twmAll:
                            intMode = TreeWalkMode.twmAll;
                            break;

                        case TreeWalkMode.twmFamily:
                            intMode = TreeWalkMode.twmNone;
                            break;

                        case TreeWalkMode.twmDescendants:
                            intMode = TreeWalkMode.twmDescendants;
                            break;
                    }

                    int num2 = family.Children.Count;
                    for (int j = 0; j < num2; j++) {
                        GDMIndividualRecord child = (GDMIndividualRecord)family.Children[j].Value;
                        WalkTreeInt(child, intMode, walkProc, extData);
                    }
                }
            }
        }

        #endregion

        #region Detect cycles

        private enum DCFlag { dcfAncWalk, dcfDescWalk }

        private static void SetIndiFlag(GDMIndividualRecord iRec, DCFlag flag)
        {
            object data = iRec.ExtData;
            if (data != null) {
                int flags = (int)data;
                flags = BitHelper.SetBit(flags, (int)flag);
                iRec.ExtData = flags;
            }
        }

        private static bool HasIndiFlag(GDMIndividualRecord iRec, DCFlag flag)
        {
            object data = iRec.ExtData;
            if (data != null) {
                int flags = (int)data;
                return BitHelper.IsSetBit(flags, (int)flag);
            } else {
                return false;
            }
        }

        private static GDMIndividualRecord DetectCycleAncestors(GDMIndividualRecord iRec, Stack<GDMIndividualRecord> stack)
        {
            if (iRec == null) return null;

            if (stack.Contains(iRec)) return iRec;

            SetIndiFlag(iRec, DCFlag.dcfAncWalk);

            stack.Push(iRec);

            GDMFamilyRecord family = iRec.GetParentsFamily();
            if (family != null) {
                var res = DetectCycleAncestors(family.GetHusband(), stack);
                if (res != null) return res;

                res = DetectCycleAncestors(family.GetWife(), stack);
                if (res != null) return res;
            }

            stack.Pop();
            return null;
        }

        private static GDMIndividualRecord DetectCycleDescendants(GDMIndividualRecord iRec, Stack<GDMIndividualRecord> stack)
        {
            if (iRec == null) return null;

            if (stack.Contains(iRec)) return iRec;

            SetIndiFlag(iRec, DCFlag.dcfDescWalk);

            stack.Push(iRec);

            int num = iRec.SpouseToFamilyLinks.Count;
            for (int i = 0; i < num; i++) {
                GDMFamilyRecord family = iRec.SpouseToFamilyLinks[i].Family;

                int num2 = family.Children.Count;
                for (int j = 0; j < num2; j++) {
                    GDMIndividualRecord child = (GDMIndividualRecord)family.Children[j].Value;
                    var res = DetectCycleDescendants(child, stack);
                    if (res != null) return res;
                }
            }

            stack.Pop();
            return null;
        }

        public static string DetectCycle(GDMIndividualRecord iRec)
        {
            var stack = new Stack<GDMIndividualRecord>();

            var hasCycle = DetectCycleAncestors(iRec, stack);
            if (hasCycle != null) {
                var lastRec = stack.Pop();
                return iRec.XRef + " ... " + lastRec.XRef + " -> " + hasCycle.XRef;
            }

            stack.Clear();

            hasCycle = DetectCycleDescendants(iRec, stack);
            if (hasCycle != null) {
                var lastRec = stack.Pop();
                return iRec.XRef + " ... " + lastRec.XRef + " -> " + hasCycle.XRef;
            }

            return string.Empty;
        }

        private static string CheckCycle(GDMIndividualRecord iRec)
        {
            var stack = new Stack<GDMIndividualRecord>();
            GDMIndividualRecord hasCycle = null;

            if (!HasIndiFlag(iRec, DCFlag.dcfAncWalk)) {
                hasCycle = DetectCycleAncestors(iRec, stack);
                if (hasCycle != null) {
                    var lastRec = stack.Pop();
                    return iRec.XRef + " ... " + lastRec.XRef + " -> " + hasCycle.XRef;
                }
                stack.Clear();
            }

            if (!HasIndiFlag(iRec, DCFlag.dcfDescWalk)) {
                hasCycle = DetectCycleDescendants(iRec, stack);
                if (hasCycle != null) {
                    var lastRec = stack.Pop();
                    return iRec.XRef + " ... " + lastRec.XRef + " -> " + hasCycle.XRef;
                }
            }

            return string.Empty;
        }

        #endregion

        #region Merge trees and records

        public static void MergeTree(GDMTree mainTree, GDMTree extTree, ITextBoxHandler logBox, bool selfTest = false)
        {
            if (mainTree == null)
                throw new ArgumentNullException("mainTree");

            if (extTree == null)
                throw new ArgumentNullException("extTree");

            if (logBox != null) {
                logBox.Clear();
                logBox.AppendText(string.Format(LangMan.LS(LSID.LSID_MainBaseSize), mainTree.RecordsCount.ToString()) + "\r\n");
            }

            List<int> fragments = new List<int>();
            if (selfTest) {
                var tmpFrags = TreeTools.SearchTreeFragments(mainTree, null);
                for (int i = 0; i < tmpFrags.Count; i++) {
                    fragments.Add(tmpFrags[i].Count);
                }

                tmpFrags = TreeTools.SearchTreeFragments(extTree, null);
                for (int i = 0; i < tmpFrags.Count; i++) {
                    fragments.Add(tmpFrags[i].Count);
                }
            }

            using (var repMap = new GDMXRefReplacer()) {
                extTree.Header.Clear();
                while (extTree.RecordsCount > 0) {
                    GDMRecord rec = extTree.Extract(0);
                    string newXRef = mainTree.XRefIndex_NewXRef(rec);
                    repMap.AddXRef(rec, rec.XRef, newXRef);
                    rec.XRef = newXRef;
                    rec.ResetOwner(mainTree);
                    mainTree.AddRecord(rec);
                }

                int num = repMap.Count;
                for (int i = 0; i < num; i++) {
                    GDMRecord rec = repMap[i].Rec;
                    rec.ReplaceXRefs(repMap);
                }

                if (logBox != null) {
                    logBox.AppendText(string.Format(LangMan.LS(LSID.LSID_MainBaseSize), mainTree.RecordsCount.ToString()) + "\r\n");
                }
            }

            if (selfTest) {
                // FIXME: error reporting refactoring
                var tmpFrags = TreeTools.SearchTreeFragments(mainTree, null);
                if (fragments.Count != tmpFrags.Count) {
                    if (logBox != null) {
                        logBox.AppendText("The number of fragments is not as expected.\r\n");
                    } else {
                        throw new Exception("The number of fragments is not as expected.");
                    }
                }
                for (int i = 0; i < tmpFrags.Count; i++) {
                    if (fragments[i] != tmpFrags[i].Count) {
                        if (logBox != null) {
                            logBox.AppendText("The number of persons in the fragment is not as expected.\r\n");
                        } else {
                            throw new Exception("The number of persons in the fragment is not as expected.");
                        }
                    }
                }
            }
        }

        public static void MergeTreeFile(GDMTree mainTree, string fileName, ITextBoxHandler logBox, bool selfTest = false)
        {
            if (mainTree == null)
                throw new ArgumentNullException("mainTree");

            if (string.IsNullOrEmpty(fileName))
                throw new ArgumentNullException("fileName");

            using (var extTree = new GDMTree()) {
                var gedcomProvider = new GEDCOMProvider(extTree);
                gedcomProvider.LoadFromFile(fileName);

                MergeTree(mainTree, extTree, logBox, selfTest);
            }
        }

        public static void MergeRecord(IBaseWindow baseWin, GDMRecord targetRec, GDMRecord sourceRec, bool bookmark)
        {
            if (baseWin == null)
                throw new ArgumentNullException("baseWin");

            if (targetRec == null)
                throw new ArgumentNullException("targetRec");

            if (sourceRec == null)
                throw new ArgumentNullException("sourceRec");

            using (var repMap = new GDMXRefReplacer()) {
                repMap.AddXRef(sourceRec, sourceRec.XRef, targetRec.XRef);

                GDMTree tree = baseWin.Context.Tree;
                int num = tree.RecordsCount;
                for (int i = 0; i < num; i++) {
                    tree[i].ReplaceXRefs(repMap);
                }

                sourceRec.MoveTo(targetRec, false);
                bool res = baseWin.Context.DeleteRecord(sourceRec);

                if (targetRec.RecordType == GDMRecordType.rtIndividual && bookmark) {
                    ((GDMIndividualRecord)targetRec).Bookmark = true;
                }

                baseWin.NotifyRecord(targetRec, RecordAction.raEdit);
                baseWin.RefreshLists(false);
            }
        }

        #endregion

        #region Base Checks

        public enum CheckDiag
        {
            cdPersonLonglived,
            cdPersonSexless,
            cdLiveYearsInvalid,
            cdStrangeSpouse,
            cdStrangeParent,
            cdEmptyFamily,
            cdFatherAsChild,
            cdMotherAsChild,
            cdDuplicateChildren,
            csDateInvalid,
            csCycle
        }

        public enum CheckSolve
        {
            csSkip,
            csSetIsDead,
            csDefineSex,
            csRemove,
            csEdit,
        }

        public sealed class CheckObj
        {
            public string Comment;
            public CheckDiag Diag;
            public GDMRecord Rec;
            public CheckSolve Solve;

            public CheckObj(GDMRecord rec, CheckDiag diag, CheckSolve solve)
            {
                Rec = rec;
                Diag = diag;
                Solve = solve;
            }

            public string GetRecordName()
            {
                string result = "[" + Rec.XRef + "] ";

                switch (Rec.RecordType)
                {
                    case GDMRecordType.rtIndividual:
                        result = result + GKUtils.GetNameString(((GDMIndividualRecord)Rec), true, false);
                        break;

                    case GDMRecordType.rtFamily:
                        result = result + GKUtils.GetFamilyString((GDMFamilyRecord)Rec);
                        break;
                }

                return result;
            }
        }

        private static void CheckRecordWithEvents(GDMRecordWithEvents rec, List<CheckObj> checksList)
        {
            var dateZero = new DateTime(0);

            int num = rec.Events.Count;
            for (int i = 0; i < num; i++) {
                GDMCustomEvent evt = rec.Events[i];

                bool invalid = false;
                try {
                    var dtx = evt.Date.GetDateTime();

                    /*if (dtx == dateZero) {
                        invalid = true;
                    }*/
                } catch {
                    invalid = true;
                }

                if (invalid) {
                    CheckObj checkObj = new CheckObj(rec, CheckDiag.csDateInvalid, CheckSolve.csEdit);
                    checkObj.Comment = LangMan.LS(LSID.LSID_DateInvalid) + " (" + evt.Date.StringValue + ")";
                    checksList.Add(checkObj);
                }
            }
        }

        private static void CheckIndividualRecord(GDMIndividualRecord iRec, List<CheckObj> checksList)
        {
            CheckRecordWithEvents(iRec, checksList);

            if (iRec.FindEvent(GEDCOMTagType.DEAT) == null)
            {
                int age = GKUtils.GetAge(iRec, -1);

                if (age != -1 && age >= GKData.PROVED_LIFE_LENGTH)
                {
                    CheckObj checkObj = new CheckObj(iRec, CheckDiag.cdPersonLonglived, CheckSolve.csSetIsDead);
                    checkObj.Comment = string.Format(LangMan.LS(LSID.LSID_PersonLonglived), age);
                    checksList.Add(checkObj);
                }
            }

            GDMSex sex = iRec.Sex;
            if (sex < GDMSex.svMale || sex >= GDMSex.svUndetermined)
            {
                CheckObj checkObj = new CheckObj(iRec, CheckDiag.cdPersonSexless, CheckSolve.csDefineSex);
                checkObj.Comment = LangMan.LS(LSID.LSID_PersonSexless);
                checksList.Add(checkObj);
            }

            int yBirth = iRec.GetChronologicalYear(GEDCOMTagType.BIRT);
            int yDeath = iRec.GetChronologicalYear(GEDCOMTagType.DEAT);
            if (yBirth != 0 && yDeath != 0)
            {
                int delta = (yDeath - yBirth);
                if (delta < 0) {
                    CheckObj checkObj = new CheckObj(iRec, CheckDiag.cdLiveYearsInvalid, CheckSolve.csSkip);
                    checkObj.Comment = LangMan.LS(LSID.LSID_LiveYearsInvalid);
                    checksList.Add(checkObj);
                }
            }

            int iAge = GKUtils.GetMarriageAge(iRec);
            if (iAge > 0 && (iAge <= 13 || iAge >= 50))
            {
                CheckObj checkObj = new CheckObj(iRec, CheckDiag.cdStrangeSpouse, CheckSolve.csSkip);
                checkObj.Comment = string.Format(LangMan.LS(LSID.LSID_StrangeSpouse), iAge.ToString());
                checksList.Add(checkObj);
            }

            iAge = GKUtils.GetFirstbornAge(iRec, GKUtils.GetFirstborn(iRec));
            if (iAge > 0 && (iAge <= 13 || iAge >= 50))
            {
                CheckObj checkObj = new CheckObj(iRec, CheckDiag.cdStrangeParent, CheckSolve.csSkip);
                checkObj.Comment = string.Format(LangMan.LS(LSID.LSID_StrangeParent), iAge.ToString());
                checksList.Add(checkObj);
            }

            string cycle = CheckCycle(iRec);
            if (!string.IsNullOrEmpty(cycle)) {
                CheckObj checkObj = new CheckObj(iRec, CheckDiag.csCycle, CheckSolve.csSkip);
                checkObj.Comment = string.Format(LangMan.LS(LSID.LSID_DetectedDataLoop), cycle);
                checksList.Add(checkObj);
            }
        }

        private static void CheckFamilyRecord(GDMFamilyRecord fRec, List<CheckObj> checksList)
        {
            CheckRecordWithEvents(fRec, checksList);

            GDMRecord husb = fRec.Husband.Value;
            GDMRecord wife = fRec.Wife.Value;

            bool empty = (fRec.Notes.Count == 0 && fRec.SourceCitations.Count == 0 && fRec.MultimediaLinks.Count == 0 && fRec.UserReferences.Count == 0);
            empty = empty && (fRec.Events.Count == 0 && fRec.Children.Count == 0);
            empty = empty && (husb == null && wife == null);

            if (empty) {
                CheckObj checkObj = new CheckObj(fRec, CheckDiag.cdEmptyFamily, CheckSolve.csRemove);
                checkObj.Comment = LangMan.LS(LSID.LSID_EmptyFamily);
                checksList.Add(checkObj);
            } else {
                if (fRec.IndexOfChild(husb) >= 0) {
                    CheckObj checkObj = new CheckObj(fRec, CheckDiag.cdFatherAsChild, CheckSolve.csRemove);
                    checkObj.Comment = LangMan.LS(LSID.LSID_FatherAsChild);
                    checksList.Add(checkObj);
                }

                if (fRec.IndexOfChild(wife) >= 0) {
                    CheckObj checkObj = new CheckObj(fRec, CheckDiag.cdMotherAsChild, CheckSolve.csRemove);
                    checkObj.Comment = LangMan.LS(LSID.LSID_MotherAsChild);
                    checksList.Add(checkObj);
                }

                bool hasDup = false;
                int chNum = fRec.Children.Count;
                for (int i = 0; i < chNum; i++) {
                    var child1 = fRec.Children[i].Value;
                    for (int k = i + 1; k < chNum; k++) {
                        var child2 = fRec.Children[k].Value;
                        if (child2 == child1) {
                            hasDup = true;
                            break;
                        }
                    }
                    if (hasDup) break;
                }
                if (hasDup) {
                    CheckObj checkObj = new CheckObj(fRec, CheckDiag.cdDuplicateChildren, CheckSolve.csEdit);
                    checkObj.Comment = LangMan.LS(LSID.LSID_DuplicateChildrenInFamily);
                    checksList.Add(checkObj);
                }
            }
        }

        public static void CheckBase(IBaseWindow baseWin, List<CheckObj> checksList)
        {
            if (baseWin == null)
                throw new ArgumentNullException("baseWin");

            if (checksList == null)
                throw new ArgumentNullException("checksList");

            IProgressController progress = AppHost.Progress;
            try {
                GDMTree tree = baseWin.Context.Tree;
                progress.ProgressInit(LangMan.LS(LSID.LSID_ToolOp_7), tree.RecordsCount);
                checksList.Clear();
                GKUtils.InitExtCounts(tree, 0);

                int num = tree.RecordsCount;
                for (int i = 0; i < num; i++) {
                    progress.ProgressStep();

                    GDMRecord rec = tree[i];

                    switch (rec.RecordType) {
                        case GDMRecordType.rtIndividual:
                            CheckIndividualRecord(rec as GDMIndividualRecord, checksList);
                            break;

                        case GDMRecordType.rtFamily:
                            CheckFamilyRecord(rec as GDMFamilyRecord, checksList);
                            break;
                    }
                }
            } finally {
                progress.ProgressDone();
            }
        }

        public static void RepairProblem(IBaseWindow baseWin, CheckObj checkObj)
        {
            if (baseWin == null)
                throw new ArgumentNullException("baseWin");

            if (checkObj == null)
                throw new ArgumentNullException("checkObj");

            GDMTree tree = baseWin.Context.Tree;
            GDMIndividualRecord iRec;

            switch (checkObj.Diag) {
                case CheckDiag.cdPersonLonglived:
                    iRec = checkObj.Rec as GDMIndividualRecord;
                    baseWin.Context.CreateEventEx(iRec, GEDCOMTagType.DEAT, "", "");
                    baseWin.NotifyRecord(iRec, RecordAction.raEdit);
                    break;

                case CheckDiag.cdPersonSexless:
                    iRec = checkObj.Rec as GDMIndividualRecord;
                    baseWin.Context.CheckPersonSex(iRec);
                    baseWin.NotifyRecord(iRec, RecordAction.raEdit);
                    break;

                case CheckDiag.cdEmptyFamily:
                    tree.DeleteRecord(checkObj.Rec);
                    break;

                case CheckDiag.cdFatherAsChild:
                    {
                        var fRec = ((GDMFamilyRecord)checkObj.Rec);
                        fRec.DeleteChild(fRec.Husband.Value);
                    }
                    break;

                case CheckDiag.cdMotherAsChild:
                    {
                        var fRec = ((GDMFamilyRecord)checkObj.Rec);
                        fRec.DeleteChild(fRec.Wife.Value);
                    }
                    break;

                case CheckDiag.cdDuplicateChildren:
                    if (checkObj.Solve == CheckSolve.csEdit) {
                        BaseController.EditRecord(baseWin, checkObj.Rec);
                    }
                    break;

                case CheckDiag.csDateInvalid:
                    if (checkObj.Solve == CheckSolve.csEdit) {
                        BaseController.EditRecord(baseWin, checkObj.Rec);
                    }
                    break;
            }
        }

        #endregion

        #region Tree Split

        private static void CheckRelations_AddRel(List<GDMRecord> splitList, GDMRecord aRec)
        {
            if (splitList.IndexOf(aRec) < 0)
            {
                splitList.Add(aRec);
            }
        }

        private static void CheckRelations_CheckRecord(List<GDMRecord> splitList, GDMRecord rec)
        {
            int num = rec.MultimediaLinks.Count;
            for (int i = 0; i < num; i++)
            {
                CheckRelations_AddRel(splitList, rec.MultimediaLinks[i].Value);
            }

            int num2 = rec.Notes.Count;
            for (int i = 0; i < num2; i++)
            {
                CheckRelations_AddRel(splitList, rec.Notes[i].Value);
            }

            int num3 = rec.SourceCitations.Count;
            for (int i = 0; i < num3; i++)
            {
                CheckRelations_AddRel(splitList, rec.SourceCitations[i].Value);
            }
        }

        private static void CheckRelations_CheckTag(List<GDMRecord> splitList, GDMTagWithLists tag)
        {
            int num = tag.MultimediaLinks.Count;
            for (int i = 0; i < num; i++)
            {
                CheckRelations_AddRel(splitList, tag.MultimediaLinks[i].Value);
            }

            int num2 = tag.Notes.Count;
            for (int i = 0; i < num2; i++)
            {
                CheckRelations_AddRel(splitList, tag.Notes[i].Value);
            }

            int num3 = tag.SourceCitations.Count;
            for (int i = 0; i < num3; i++)
            {
                CheckRelations_AddRel(splitList, tag.SourceCitations[i].Value);
            }
        }

        private static void CheckRelations_CheckIndividual(List<GDMRecord> splitList, GDMIndividualRecord iRec)
        {
            CheckRelations_CheckRecord(splitList, iRec);

            int num = iRec.ChildToFamilyLinks.Count;
            for (int i = 0; i < num; i++)
            {
                CheckRelations_AddRel(splitList, iRec.ChildToFamilyLinks[i].Family);
            }

            int num2 = iRec.SpouseToFamilyLinks.Count;
            for (int i = 0; i < num2; i++)
            {
                CheckRelations_AddRel(splitList, iRec.SpouseToFamilyLinks[i].Family);
            }

            int num3 = iRec.Events.Count;
            for (int i = 0; i < num3; i++)
            {
                CheckRelations_CheckTag(splitList, iRec.Events[i]);
            }

            int num5 = iRec.Submittors.Count;
            for (int i = 0; i < num5; i++)
            {
                CheckRelations_AddRel(splitList, iRec.Submittors[i].Value);
            }

            int num6 = iRec.Associations.Count;
            for (int i = 0; i < num6; i++)
            {
                CheckRelations_AddRel(splitList, iRec.Associations[i].Value);
            }

            int num7 = iRec.Aliases.Count;
            for (int i = 0; i < num7; i++)
            {
                CheckRelations_AddRel(splitList, iRec.Aliases[i].Value);
            }

            int num10 = iRec.Groups.Count;
            for (int i = 0; i < num10; i++)
            {
                CheckRelations_AddRel(splitList, iRec.Groups[i].Value);
            }
        }

        private static void CheckRelations_CheckFamily(List<GDMRecord> splitList, GDMFamilyRecord fRec)
        {
            CheckRelations_CheckRecord(splitList, fRec);

            int num = fRec.Events.Count;
            for (int i = 0; i < num; i++)
            {
                CheckRelations_CheckTag(splitList, fRec.Events[i]);
            }

            int num5 = fRec.Submittors.Count;
            for (int i = 0; i < num5; i++)
            {
                CheckRelations_AddRel(splitList, fRec.Submittors[i].Value);
            }
        }

        private static void CheckRelations_CheckSource(List<GDMRecord> splitList, GDMSourceRecord sRec)
        {
            CheckRelations_CheckRecord(splitList, sRec);

            int num = sRec.RepositoryCitations.Count;
            for (int i = 0; i < num; i++) {
                CheckRelations_AddRel(splitList, sRec.RepositoryCitations[i].Value);
            }
        }

        public static void CheckRelations(List<GDMRecord> splitList)
        {
            if (splitList == null)
                throw new ArgumentNullException("splitList");

            int num = splitList.Count;
            for (int i = 0; i < num; i++)
            {
                GDMRecord rec = splitList[i];
                switch (rec.RecordType)
                {
                    case GDMRecordType.rtIndividual:
                        CheckRelations_CheckIndividual(splitList, rec as GDMIndividualRecord);
                        break;

                    case GDMRecordType.rtFamily:
                        CheckRelations_CheckFamily(splitList, rec as GDMFamilyRecord);
                        break;

                    case GDMRecordType.rtNote:
                        CheckRelations_CheckRecord(splitList, rec);
                        break;

                    case GDMRecordType.rtMultimedia:
                        CheckRelations_CheckRecord(splitList, rec);
                        break;

                    case GDMRecordType.rtSource:
                        CheckRelations_CheckSource(splitList, rec as GDMSourceRecord);
                        break;

                    case GDMRecordType.rtRepository:
                        CheckRelations_CheckRecord(splitList, rec);
                        break;

                    case GDMRecordType.rtSubmitter:
                        CheckRelations_CheckRecord(splitList, rec);
                        break;
                }
            }
        }

        #endregion

        #region Tree Compare

        public class IndividualRecordComparer: IComparer<ULIndividual>
        {
            public int Compare(ULIndividual x, ULIndividual y)
            {
                return string.Compare(x.Family, y.Family, false);
            }
        }

        public class ULIndividual
        {
            public string Family;
            public GDMIndividualRecord IRec;
        }

        public static List<ULIndividual> GetUnlinkedNamesakes(IBaseWindow baseWin)
        {
            if (baseWin == null)
                throw new ArgumentNullException("baseWin");

            GDMTree tree = baseWin.Context.Tree;

            List<ULIndividual> result = new List<ULIndividual>();

            Dictionary<string, List<GDMIndividualRecord>> families = new Dictionary<string, List<GDMIndividualRecord>>();

            IProgressController progress = AppHost.Progress;
            progress.ProgressInit(LangMan.LS(LSID.LSID_Stage) + "1", tree.RecordsCount);

            // make a table of surnames and persons, related to these surnames
            int num = tree.RecordsCount;
            for (int i = 0; i < num; i++)
            {
                GDMRecord rec = tree[i];

                if (rec.RecordType == GDMRecordType.rtIndividual)
                {
                    GDMIndividualRecord iRec = (GDMIndividualRecord)rec;

                    string[] fams = baseWin.Context.Culture.GetSurnames(iRec);

                    for (int k = 0; k < fams.Length; k++)
                    {
                        string f = fams[k];
                        if (f.Length > 1)
                        {
                            List<GDMIndividualRecord> ps;
                            if (!families.TryGetValue(f, out ps)) {
                                ps = new List<GDMIndividualRecord>();
                                families.Add(f, ps);
                            }
                            ps.Add(iRec);
                        }
                    }
                }

                progress.ProgressStep();
            }

            progress.ProgressInit(LangMan.LS(LSID.LSID_Stage) + "2", families.Count);

            // find all persons of one surname, not related by ties of kinship
            foreach (KeyValuePair<string, List<GDMIndividualRecord>> entry in families)
            {
                string fam = entry.Key;
                List<GDMIndividualRecord> ps = entry.Value;

                int i = 0;
                while (i < ps.Count)
                {
                    GDMIndividualRecord iRec = ps[i];

                    List<GDMRecord> lst = new List<GDMRecord>();
                    WalkTree(iRec, TreeWalkMode.twmAll, lst);

                    int num3 = lst.Count;
                    for (int k = 0; k < num3; k++)
                    {
                        GDMIndividualRecord item = lst[k] as GDMIndividualRecord;

                        int idx = ps.IndexOf(item);
                        if (item != iRec && idx >= 0 && idx > i) ps.RemoveAt(idx);
                    }

                    i++;
                }

                int num2 = ps.Count;
                for (i = 0; i < num2; i++) {
                    ULIndividual indiv = new ULIndividual();
                    indiv.Family = fam;
                    indiv.IRec = ps[i];
                    result.Add(indiv);
                }

                progress.ProgressStep();
            }

            result.Sort(new IndividualRecordComparer());
            
            progress.ProgressDone();

            return result;
        }

        public delegate void DuplicateFoundFunc(GDMIndividualRecord indivA, GDMIndividualRecord indivB);

        public static void FindDuplicates(GDMTree treeA, GDMTree treeB, float matchThreshold,
                                          DuplicateFoundFunc foundFunc, IProgressController pc)
        {
            if (treeA == null)
                throw new ArgumentNullException("treeA");

            if (treeB == null)
                throw new ArgumentNullException("treeB");

            if (foundFunc == null)
                throw new ArgumentNullException("foundFunc");

            if (pc == null)
                throw new ArgumentNullException("pc");

            MatchParams mParams;
            //mParams.IndistinctMatching = true;
            mParams.NamesIndistinctThreshold = 90.0f / 100.0f;
            mParams.DatesCheck = true;
            mParams.YearsInaccuracy = 3;
            mParams.CheckEventPlaces = false;

            pc.ProgressInit(LangMan.LS(LSID.LSID_DuplicatesSearch), treeA.RecordsCount);
            try
            {
                for (int i = 0; i < treeA.RecordsCount; i++) {
                    GDMRecord recA = treeA[i];
                    if (recA.RecordType == GDMRecordType.rtIndividual) {
                        for (int k = 0; k < treeB.RecordsCount; k++) {
                            GDMRecord recB = treeB[k];
                            if (recB.RecordType == GDMRecordType.rtIndividual) {
                                GDMIndividualRecord indivA = (GDMIndividualRecord) recA;
                                GDMIndividualRecord indivB = (GDMIndividualRecord) recB;

                                if (indivA != indivB && indivA.IsMatch(indivB, mParams) >= matchThreshold)
                                {
                                    foundFunc(indivA, indivB);
                                }
                            }
                        }
                    }

                    pc.ProgressStep();
                    Thread.Sleep(1);
                }
            }
            finally
            {
                pc.ProgressDone();
            }
        }

        public static void CompareTree(IBaseContext context, string fileName, ITextBoxHandler logBox)
        {
            if (context == null)
                throw new ArgumentNullException("context");

            if (logBox == null)
                throw new ArgumentNullException("logBox");

            GDMTree mainTree = context.Tree;
            GDMTree tempTree = new GDMTree();

            var gedcomProvider = new GEDCOMProvider(tempTree);
            gedcomProvider.LoadFromFile(fileName);

            StringList fams = new StringList();
            StringList names = new StringList();

            try
            {
                logBox.AppendText(LangMan.LS(LSID.LSID_SearchMatches) + "\r\n");

                int num = mainTree.RecordsCount;
                for (int i = 0; i < num; i++)
                {
                    GDMRecord rec = mainTree[i];
                    if (rec.RecordType == GDMRecordType.rtIndividual)
                    {
                        GDMIndividualRecord iRec = (GDMIndividualRecord)rec;

                        int idx = names.AddObject(GKUtils.GetNameString(iRec, true, false), new ExtList<GDMIndividualRecord>());
                        ((ExtList<GDMIndividualRecord>)names.GetObject(idx)).Add(iRec);

                        var parts = GKUtils.GetNameParts(iRec);
                        fams.AddObject(context.Culture.NormalizeSurname(parts.Surname, iRec.Sex == GDMSex.svFemale), null);
                    }
                }

                int num2 = tempTree.RecordsCount;
                for (int i = 0; i < num2; i++)
                {
                    GDMRecord rec = tempTree[i];
                    if (rec.RecordType == GDMRecordType.rtIndividual)
                    {
                        GDMIndividualRecord iRec = (GDMIndividualRecord)tempTree[i];

                        string tm = GKUtils.GetNameString(iRec, true, false);
                        int idx = names.IndexOf(tm);
                        if (idx >= 0)
                        {
                            ((ExtList<GDMIndividualRecord>)names.GetObject(idx)).Add(iRec);
                        }

                        var parts = GKUtils.GetNameParts(iRec);
                        tm = context.Culture.NormalizeSurname(parts.Surname, iRec.Sex == GDMSex.svFemale);
                        idx = fams.IndexOf(tm);
                        if (idx >= 0)
                        {
                            fams.SetObject(idx, 1);
                        }
                    }
                }

                for (int i = fams.Count - 1; i >= 0; i--)
                {
                    if (fams.GetObject(i) == null || fams[i] == "?")
                        fams.Delete(i);
                }

                for (int i = names.Count - 1; i >= 0; i--)
                {
                    ExtList<GDMIndividualRecord> lst = (ExtList<GDMIndividualRecord>)names.GetObject(i);

                    if (lst.Count == 1)
                    {
                        lst.Dispose();
                        names.Delete(i);
                    }
                }

                if (fams.Count != 0)
                {
                    logBox.AppendText(LangMan.LS(LSID.LSID_SimilarSurnames) + "\r\n");

                    int num3 = fams.Count;
                    for (int i = 0; i < num3; i++)
                    {
                        logBox.AppendText("    " + fams[i] + "\r\n");
                    }
                }

                if (names.Count != 0)
                {
                    logBox.AppendText(LangMan.LS(LSID.LSID_SimilarNames) + "\r\n");

                    int num4 = names.Count;
                    for (int i = 0; i < num4; i++)
                    {
                        logBox.AppendText("    " + names[i] + "\r\n");
                        ExtList<GDMIndividualRecord> lst = (ExtList<GDMIndividualRecord>)names.GetObject(i);

                        int num5 = lst.Count;
                        for (int j = 0; j < num5; j++)
                        {
                            GDMIndividualRecord iRec = lst[j];
                            logBox.AppendText("      * " + GKUtils.GetNameString(iRec, true, false) + " " + GKUtils.GetLifeStr(iRec) + "\r\n");
                        }
                    }
                }
            }
            finally
            {
                int num6 = names.Count;
                for (int i = 0; i < num6; i++)
                {
                    IDisposable inst = names.GetObject(i) as IDisposable;
                    if (inst != null) inst.Dispose();
                }

                names.Dispose();
                fams.Dispose();

                tempTree.Dispose();
            }
        }

        #endregion

        #region Places Management

        public static void SearchPlaces_Clear(StringList placesList)
        {
            if (placesList == null)
                throw new ArgumentNullException("placesList");

            for (int i = placesList.Count - 1; i >= 0; i--) ((PlaceObj)placesList.GetObject(i)).Dispose();
            placesList.Clear();
        }

        private static void SearchPlaces_CheckEventPlace(StringList placesList, GDMCustomEvent evt)
        {
            string placeStr = evt.Place.StringValue;
            if (string.IsNullOrEmpty(placeStr)) return;

            GDMLocationRecord loc = evt.Place.Location.Value as GDMLocationRecord;
            if (loc != null) {
                placeStr = "[*] " + placeStr;
            }

            int idx = placesList.IndexOf(placeStr);

            PlaceObj placeObj;
            if (idx >= 0) {
                placeObj = (PlaceObj)placesList.GetObject(idx);
            } else {
                placeObj = new PlaceObj(placeStr);
                placesList.AddObject(placeStr, placeObj);
            }
            placeObj.Facts.Add(evt);
        }

        public static void SearchPlaces(GDMTree tree, StringList placesList, IProgressController pc)
        {
            if (tree == null)
                throw new ArgumentNullException("tree");

            if (placesList == null)
                throw new ArgumentNullException("placesList");

            if (pc == null)
                throw new ArgumentNullException("pc");

            SearchPlaces_Clear(placesList);

            try
            {
                int recsCount = tree.RecordsCount;
                pc.ProgressInit(LangMan.LS(LSID.LSID_PlacesPrepare), recsCount);

                for (int i = 0; i < recsCount; i++) {
                    pc.ProgressStep();

                    var evsRec = tree[i] as GDMRecordWithEvents;
                    if (evsRec != null) {
                        int num2 = evsRec.Events.Count;
                        for (int j = 0; j < num2; j++) {
                            GDMCustomEvent evt = evsRec.Events[j];

                            SearchPlaces_CheckEventPlace(placesList, evt);
                        }
                    }
                }
            } finally {
                pc.ProgressDone();
            }
        }

        #endregion

        #region Tree fragments

        public static List<List<GDMRecord>> SearchTreeFragments(GDMTree tree, IProgressController progress)
        {
            List<List<GDMRecord>> result = new List<List<GDMRecord>>();

            if (progress != null) {
                progress.ProgressInit(LangMan.LS(LSID.LSID_CheckFamiliesConnection), tree.RecordsCount);
            }

            List<GDMRecord> prepared = new List<GDMRecord>();
            try {
                int num = tree.RecordsCount;
                for (int i = 0; i < num; i++) {
                    GDMIndividualRecord iRec = tree[i] as GDMIndividualRecord;
                    if (iRec != null) {
                        if (prepared.IndexOf(iRec) < 0) {
                            var groupRecords = new List<GDMRecord>();
                            TreeTools.WalkTree(iRec, TreeTools.TreeWalkMode.twmAll, groupRecords);
                            result.Add(groupRecords);
                            prepared.AddRange(groupRecords);
                        }
                    }

                    if (progress != null) {
                        progress.ProgressStep();
                    }
                }
            } finally {
                prepared.Clear();

                if (progress != null) {
                    progress.ProgressDone();
                }
            }

            return result;
        }

        #endregion
    }
}
