/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
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
using System.Text.RegularExpressions;
using System.Threading.Tasks;
using BSLib;
using BSLib.DataViz.SmartGraph;
using GDModel;
using GDModel.Providers.GEDCOM;
using GKCore.Design.Controls;
using GKCore.Interfaces;
using GKCore.Types;

namespace GKCore.Tools
{
    /// <summary>
    ///
    /// </summary>
    public static class TreeTools
    {
        private const string CRLF = "\r\n";


        #region Patriarchs Search

        public static bool PL_SearchAnc(GDMTree tree, GDMIndividualRecord descendant, GDMIndividualRecord searchRec, bool onlyMaleLine)
        {
            if (descendant == null) return false;

            bool res = (descendant == searchRec);

            if (!res && descendant.ChildToFamilyLinks.Count > 0)
            {
                GDMFamilyRecord family = tree.GetPtrValue(descendant.ChildToFamilyLinks[0]);

                GDMIndividualRecord ancestor = tree.GetPtrValue(family.Husband);
                if (ancestor != null) {
                    res = PL_SearchAnc(tree, ancestor, searchRec, onlyMaleLine);
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
        public static GDMIndividualRecord PL_SearchDesc(GDMTree tree, GDMIndividualRecord ancestorRec, GDMIndividualRecord searchRec)
        {
            GDMIndividualRecord cross = null;

            int num = ancestorRec.SpouseToFamilyLinks.Count;
            for (int i = 0; i < num; i++) {
                GDMFamilyRecord family = tree.GetPtrValue(ancestorRec.SpouseToFamilyLinks[i]);
                GDMIndividualRecord spouse = tree.GetSpouseBy(family, ancestorRec);

                if (spouse != null) {
                    bool res = PL_SearchAnc(tree, spouse, searchRec, (ancestorRec.Sex == GDMSex.svFemale));
                    if (res) {
                        cross = ancestorRec;
                        return cross;
                    }
                }

                if (ancestorRec.Sex == GDMSex.svMale) {
                    int num2 = family.Children.Count;
                    for (int j = 0; j < num2; j++) {
                        GDMIndividualRecord child = tree.GetPtrValue(family.Children[j]);
                        cross = PL_SearchDesc(tree, child, searchRec);
                        if (cross != null) return cross;
                    }
                }
            }

            return null;
        }

        public static GDMFamilyRecord PL_SearchIntersection(GDMTree tree, GDMIndividualRecord ancestor, GDMIndividualRecord searchRec)
        {
            int num = ancestor.SpouseToFamilyLinks.Count;
            for (int i = 0; i < num; i++) {
                GDMFamilyRecord family = tree.GetPtrValue(ancestor.SpouseToFamilyLinks[i]);
                GDMIndividualRecord spouse = tree.GetSpouseBy(family, ancestor);

                if (spouse != null) {
                    bool res = PL_SearchAnc(tree, spouse, searchRec, (ancestor.Sex == GDMSex.svFemale));
                    if (res) return family;
                }

                if (ancestor.Sex == GDMSex.svMale) {
                    int num2 = family.Children.Count;
                    for (int j = 0; j < num2; j++) {
                        GDMIndividualRecord child = tree.GetPtrValue(family.Children[j]);

                        GDMFamilyRecord res = PL_SearchIntersection(tree, child, searchRec);
                        if (res != null) return res;
                    }
                }
            }

            return null;
        }

        public static void GenPatriarchsGraphviz(IBaseWindow baseWin, string outpath, int minGens, bool loneSuppress, IProgressController progress)
        {
            if (baseWin == null)
                throw new ArgumentNullException("baseWin");

            string[] options = { "ratio=auto" };
            GraphvizWriter gvw = new GraphvizWriter("Family Tree", options);

            var patList = PatriarchsMan.GetPatriarchsLinks(baseWin.Context, minGens, false, loneSuppress, progress);

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

            gvw.SaveFile(outpath);
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

        public static void WalkTree(GDMTree tree, GDMIndividualRecord iRec, TreeWalkMode mode, WalkProc walkProc, object extData)
        {
            if (tree == null)
                throw new ArgumentNullException("tree");

            if (iRec == null)
                throw new ArgumentNullException("iRec");

            if (walkProc == null)
                throw new ArgumentNullException("walkProc");

            if (extData == null)
                throw new ArgumentNullException("extData");

            WalkTreeInt(tree, iRec, mode, walkProc, extData);
        }

        public static void WalkTree(GDMTree tree, GDMIndividualRecord iRec, TreeWalkMode mode, List<GDMRecord> walkList)
        {
            if (tree == null)
                throw new ArgumentNullException("tree");

            if (iRec == null)
                throw new ArgumentNullException("iRec");

            if (walkList == null)
                throw new ArgumentNullException("walkList");

            WalkTreeInt(tree, iRec, mode, DefaultWalkProc, walkList);
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

        private static void WalkTreeInt(GDMTree tree, GDMIndividualRecord iRec, TreeWalkMode mode, WalkProc walkProc, object extData)
        {
            if (!walkProc(iRec, mode, extData)) return;

            if (mode == TreeWalkMode.twmNone) return;

            if (mode == TreeWalkMode.twmAll || mode == TreeWalkMode.twmAncestors) {
                for (int i = 0, num = iRec.ChildToFamilyLinks.Count; i < num; i++) {
                    var family = tree.GetPtrValue(iRec.ChildToFamilyLinks[i]);
                    if (family != null) {
                        GDMIndividualRecord father, mother;
                        tree.GetSpouses(family, out father, out mother);

                        WalkTreeInt(tree, father, mode, walkProc, extData);
                        WalkTreeInt(tree, mother, mode, walkProc, extData);
                    }
                }
            }

            // twmAll, twmFamily, twmDescendants
            if (mode < TreeWalkMode.twmAncestors || mode == TreeWalkMode.twmDescendants) {
                int num = iRec.SpouseToFamilyLinks.Count;
                for (int i = 0; i < num; i++) {
                    GDMFamilyRecord family = tree.GetPtrValue(iRec.SpouseToFamilyLinks[i]);
                    GDMIndividualRecord spouse = (iRec.Sex == GDMSex.svMale) ? tree.GetPtrValue(family.Wife) : tree.GetPtrValue(family.Husband);

                    TreeWalkMode intMode = ((mode == TreeWalkMode.twmAll) ? TreeWalkMode.twmAll : TreeWalkMode.twmNone);
                    WalkTreeInt(tree, spouse, intMode, walkProc, extData);

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
                        GDMIndividualRecord child = tree.GetPtrValue(family.Children[j]);
                        WalkTreeInt(tree, child, intMode, walkProc, extData);
                    }
                }
            }
        }

        #endregion

        #region Merge trees and records

        public static void MergeTree(GDMTree mainTree, GDMTree extTree, ITextBox logBox, bool selfTest = false)
        {
            if (mainTree == null)
                throw new ArgumentNullException("mainTree");

            if (extTree == null)
                throw new ArgumentNullException("extTree");

            if (logBox != null) {
                logBox.Clear();
                logBox.AppendText(string.Format(LangMan.LS(LSID.MainBaseSize), mainTree.RecordsCount.ToString()) + CRLF);
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
                    var oldXRef = rec.XRef;
                    var newXRef = mainTree.NewXRef(rec);
                    repMap.AddXRef(rec, oldXRef, newXRef);
                    rec.ResetTree(mainTree);
                    mainTree.AddRecord(rec);
                }

                for (int i = 0, num = repMap.Count; i < num; i++) {
                    GDMRecord rec = repMap[i].Rec;
                    rec.ReplaceXRefs(repMap);
                }

                if (logBox != null) {
                    logBox.AppendText(string.Format(LangMan.LS(LSID.MainBaseSize), mainTree.RecordsCount.ToString()) + CRLF);
                }
            }

            if (selfTest) {
                var tmpFrags = TreeTools.SearchTreeFragments(mainTree, null);
                if (fragments.Count != tmpFrags.Count) {
                    ThrowError(logBox, "The number of fragments is not as expected.");
                }
                for (int i = 0; i < tmpFrags.Count; i++) {
                    if (fragments[i] != tmpFrags[i].Count) {
                        ThrowError(logBox, "The number of persons in the fragment is not as expected.");
                    }
                }
            }
        }

        private static void ThrowError(ITextBox logBox, string message)
        {
            if (logBox != null) {
                logBox.AppendText(message + CRLF);
            } else {
                throw new GKException(message);
            }
        }

        public static void MergeTreeFile(GDMTree mainTree, string fileName, ITextBox logBox, bool selfTest = false)
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

        public static async Task MergeRecord(IBaseWindow baseWin, GDMRecord targetRec, GDMRecord sourceRec, bool bookmark)
        {
            if (baseWin == null)
                throw new ArgumentNullException("baseWin");

            if (targetRec == null)
                throw new ArgumentNullException("targetRec");

            if (sourceRec == null)
                throw new ArgumentNullException("sourceRec");

            if (targetRec.RecordType == GDMRecordType.rtRepository) {
                var res = await AppHost.StdDialogs.ShowQuestion(LangMan.LS(LSID.RepoRecsMergeWarning));
                if (!res) {
                    return;
                }
            }

            using (var repMap = new GDMXRefReplacer()) {
                repMap.AddXRef(sourceRec, sourceRec.XRef, targetRec.XRef);

                GDMTree tree = baseWin.Context.Tree;
                int num = tree.RecordsCount;
                for (int i = 0; i < num; i++) {
                    tree[i].ReplaceXRefs(repMap);
                }

                sourceRec.MoveTo(targetRec);
                await baseWin.Context.DeleteRecord(sourceRec);

                if (targetRec.RecordType == GDMRecordType.rtIndividual && bookmark) {
                    ((GDMIndividualRecord)targetRec).Bookmark = true;
                }

                baseWin.NotifyRecord(targetRec, RecordAction.raEdit);
                baseWin.RefreshLists(false);
            }
        }

        #endregion

        #region Tree Split

        private static void CheckRelations_AddRel(List<GDMRecord> splitList, GDMRecord rec, bool required = false)
        {
            if (rec != null && (rec.RecordType > GDMRecordType.rtFamily || required) && splitList.IndexOf(rec) < 0) {
                splitList.Add(rec);
            }
        }

        private static void CheckRelations_CheckNotes(GDMTree tree, List<GDMRecord> splitList, IGDMStructWithNotes tag)
        {
            if (tag == null || !tag.HasNotes) return;

            for (int i = 0, num = tag.Notes.Count; i < num; i++) {
                CheckRelations_AddRel(splitList, tree.GetPtrValue<GDMRecord>(tag.Notes[i]));
            }
        }

        private static void CheckRelations_CheckSourceCit(GDMTree tree, List<GDMRecord> splitList, IGDMStructWithSourceCitations tag)
        {
            if (tag == null || !tag.HasSourceCitations) return;

            for (int i = 0, num = tag.SourceCitations.Count; i < num; i++) {
                CheckRelations_AddRel(splitList, tree.GetPtrValue<GDMRecord>(tag.SourceCitations[i]));
            }
        }

        private static void CheckRelations_CheckMediaLink(GDMTree tree, List<GDMRecord> splitList, IGDMStructWithMultimediaLinks tag)
        {
            if (tag == null || !tag.HasMultimediaLinks) return;

            for (int i = 0, num = tag.MultimediaLinks.Count; i < num; i++) {
                CheckRelations_AddRel(splitList, tree.GetPtrValue<GDMRecord>(tag.MultimediaLinks[i]));
            }
        }

        private static void CheckRelations_CheckSWL(GDMTree tree, List<GDMRecord> splitList, IGDMStructWithLists tag)
        {
            if (tag == null) return;

            CheckRelations_CheckNotes(tree, splitList, tag);
            CheckRelations_CheckSourceCit(tree, splitList, tag);
            CheckRelations_CheckMediaLink(tree, splitList, tag);
        }

        private static void CheckRelations_CheckRecord(GDMTree tree, List<GDMRecord> splitList, GDMRecord rec)
        {
            if (rec == null) return;

            CheckRelations_CheckSWL(tree, splitList, rec);
        }

        private static void CheckRelations_CheckIndividual(GDMTree tree, List<GDMRecord> splitList, GDMIndividualRecord iRec)
        {
            if (iRec == null) return;

            CheckRelations_CheckRecord(tree, splitList, iRec);

            for (int i = 0, num = iRec.ChildToFamilyLinks.Count; i < num; i++) {
                var cfl = iRec.ChildToFamilyLinks[i];
                CheckRelations_CheckNotes(tree, splitList, cfl);
                CheckRelations_AddRel(splitList, tree.GetPtrValue(cfl));
            }

            for (int i = 0, num = iRec.SpouseToFamilyLinks.Count; i < num; i++) {
                var sfl = iRec.SpouseToFamilyLinks[i];
                CheckRelations_CheckNotes(tree, splitList, sfl);
                CheckRelations_AddRel(splitList, tree.GetPtrValue(sfl));
            }

            if (iRec.HasEvents) {
                for (int i = 0, num = iRec.Events.Count; i < num; i++) {
                    CheckRelations_CheckSWL(tree, splitList, iRec.Events[i]);
                }
            }

            if (iRec.HasAssociations) {
                for (int i = 0, num = iRec.Associations.Count; i < num; i++) {
                    var asso = iRec.Associations[i];
                    CheckRelations_CheckNotes(tree, splitList, asso);
                    CheckRelations_CheckSourceCit(tree, splitList, asso);
                    CheckRelations_AddRel(splitList, tree.GetPtrValue(asso));
                }
            }

            if (iRec.HasGroups) {
                for (int i = 0, num = iRec.Groups.Count; i < num; i++) {
                    CheckRelations_AddRel(splitList, tree.GetPtrValue<GDMRecord>(iRec.Groups[i]));
                }
            }
        }

        private static void CheckRelations_CheckFamily(GDMTree tree, List<GDMRecord> splitList, GDMFamilyRecord fRec)
        {
            if (fRec == null) return;

            CheckRelations_CheckRecord(tree, splitList, fRec);

            CheckRelations_AddRel(splitList, tree.GetPtrValue<GDMRecord>(fRec.Husband));
            CheckRelations_AddRel(splitList, tree.GetPtrValue<GDMRecord>(fRec.Wife));

            for (int i = 0, num = fRec.Children.Count; i < num; i++) {
                CheckRelations_AddRel(splitList, tree.GetPtrValue<GDMRecord>(fRec.Children[i]));
            }

            if (fRec.HasEvents) {
                for (int i = 0, num = fRec.Events.Count; i < num; i++) {
                    CheckRelations_CheckSWL(tree, splitList, fRec.Events[i]);
                }
            }
        }

        private static void CheckRelations_CheckSource(GDMTree tree, List<GDMRecord> splitList, GDMSourceRecord sRec)
        {
            if (sRec == null) return;

            CheckRelations_CheckRecord(tree, splitList, sRec);

            for (int i = 0, num = sRec.RepositoryCitations.Count; i < num; i++) {
                CheckRelations_AddRel(splitList, tree.GetPtrValue<GDMRecord>(sRec.RepositoryCitations[i]));
            }
        }

        private static void CheckRelations_RequireFamily(GDMTree tree, List<GDMRecord> splitList, GDMFamilyRecord fRec)
        {
            if (fRec == null) return;

            if (splitList.Contains(tree.GetPtrValue<GDMRecord>(fRec.Husband))) {
                CheckRelations_AddRel(splitList, fRec, true);
                return;
            }

            if (splitList.Contains(tree.GetPtrValue<GDMRecord>(fRec.Wife))) {
                CheckRelations_AddRel(splitList, fRec, true);
                return;
            }

            for (int i = 0, num = fRec.Children.Count; i < num; i++) {
                if (splitList.Contains(tree.GetPtrValue<GDMRecord>(fRec.Children[i]))) {
                    CheckRelations_AddRel(splitList, fRec, true);
                    return;
                }
            }
        }

        public static void CheckRelations(GDMTree tree, List<GDMRecord> splitList)
        {
            if (splitList == null)
                throw new ArgumentNullException("splitList");

            // check relations betweeen individuals from list
            // put in the relationship queue only the families between the persons of the primary queue
            int num = splitList.Count;
            for (int k = 0; k < num; k++) {
                var iRec = splitList[k] as GDMIndividualRecord;
                if (iRec == null) continue;

                for (int i = 0; i < iRec.ChildToFamilyLinks.Count; i++) {
                    var cfl = iRec.ChildToFamilyLinks[i];
                    CheckRelations_RequireFamily(tree, splitList, tree.GetPtrValue(cfl));
                }

                for (int i = 0; i < iRec.SpouseToFamilyLinks.Count; i++) {
                    var sfl = iRec.SpouseToFamilyLinks[i];
                    CheckRelations_RequireFamily(tree, splitList, tree.GetPtrValue(sfl));
                }
            }

            // check all other relations
            int m = 0;
            while (m < splitList.Count) {
                GDMRecord rec = splitList[m];

                switch (rec.RecordType) {
                    case GDMRecordType.rtIndividual:
                        CheckRelations_CheckIndividual(tree, splitList, rec as GDMIndividualRecord);
                        break;

                    case GDMRecordType.rtFamily:
                        CheckRelations_CheckFamily(tree, splitList, rec as GDMFamilyRecord);
                        break;

                    case GDMRecordType.rtNote:
                        CheckRelations_CheckRecord(tree, splitList, rec);
                        break;

                    case GDMRecordType.rtMultimedia:
                        CheckRelations_CheckRecord(tree, splitList, rec);
                        break;

                    case GDMRecordType.rtSource:
                        CheckRelations_CheckSource(tree, splitList, rec as GDMSourceRecord);
                        break;

                    case GDMRecordType.rtRepository:
                        CheckRelations_CheckRecord(tree, splitList, rec);
                        break;

                    case GDMRecordType.rtSubmitter:
                        CheckRelations_CheckRecord(tree, splitList, rec);
                        break;
                }

                m += 1;
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

        public static List<ULIndividual> GetUnlinkedNamesakes(IBaseWindow baseWin, IProgressController progress)
        {
            if (baseWin == null)
                throw new ArgumentNullException("baseWin");

            GDMTree tree = baseWin.Context.Tree;

            List<ULIndividual> result = new List<ULIndividual>();

            Dictionary<string, List<GDMIndividualRecord>> families = new Dictionary<string, List<GDMIndividualRecord>>();

            progress.Begin(LangMan.LS(LSID.Stage) + "1", tree.RecordsCount, true);

            // make a table of surnames and persons, related to these surnames
            int num = tree.RecordsCount;
            for (int i = 0; i < num; i++) {
                GDMRecord rec = tree[i];

                if (rec.RecordType == GDMRecordType.rtIndividual) {
                    GDMIndividualRecord iRec = (GDMIndividualRecord)rec;

                    string[] fams = baseWin.Context.GetIndividualSurnames(iRec);

                    for (int k = 0; k < fams.Length; k++) {
                        string f = fams[k];
                        if (f.Length > 1) {
                            List<GDMIndividualRecord> ps;
                            if (!families.TryGetValue(f, out ps)) {
                                ps = new List<GDMIndividualRecord>();
                                families.Add(f, ps);
                            }
                            ps.Add(iRec);
                        }
                    }
                }

                if (progress.IsCanceled) break;
                progress.Increment();
            }

            progress.Begin(LangMan.LS(LSID.Stage) + "2", families.Count, true);

            // find all persons of one surname, not related by ties of kinship
            foreach (KeyValuePair<string, List<GDMIndividualRecord>> entry in families) {
                string fam = entry.Key;
                List<GDMIndividualRecord> ps = entry.Value;

                int i = 0;
                while (i < ps.Count) {
                    GDMIndividualRecord iRec = ps[i];

                    List<GDMRecord> lst = new List<GDMRecord>();
                    WalkTree(tree, iRec, TreeWalkMode.twmAll, lst);

                    int num3 = lst.Count;
                    for (int k = 0; k < num3; k++) {
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

                if (progress.IsCanceled) break;
                progress.Increment();
            }

            result.Sort(new IndividualRecordComparer());

            progress.End();

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

            pc.Begin(LangMan.LS(LSID.DuplicatesSearch), treeA.RecordsCount, true);
            try {
                for (int i = 0; i < treeA.RecordsCount; i++) {
                    GDMRecord recA = treeA[i];
                    if (recA.RecordType == GDMRecordType.rtIndividual) {
                        for (int k = 0; k < treeB.RecordsCount; k++) {
                            GDMRecord recB = treeB[k];
                            if (recB.RecordType == GDMRecordType.rtIndividual) {
                                GDMIndividualRecord indivA = (GDMIndividualRecord)recA;
                                GDMIndividualRecord indivB = (GDMIndividualRecord)recB;

                                if (indivA != indivB && indivA.IsMatch(indivB, mParams) >= matchThreshold) {
                                    foundFunc(indivA, indivB);
                                }
                            }
                        }
                    }

                    if (pc.IsCanceled) break;
                    pc.Increment();
                }
            } finally {
                pc.End();
            }
        }

        public static void CompareTree(IBaseContext context, string fileName, ITextBox logBox)
        {
            if (context == null)
                throw new ArgumentNullException("context");

            if (logBox == null)
                throw new ArgumentNullException("logBox");

            using (var tempTree = new GDMTree()) {
                var gedcomProvider = new GEDCOMProvider(tempTree);
                gedcomProvider.LoadFromFile(fileName);

                CompareTree(context, tempTree, logBox);
            }
        }

        public static void CompareTree(IBaseContext context, GDMTree tempTree, ITextBox logBox)
        {
            if (context == null)
                throw new ArgumentNullException("context");

            if (tempTree == null)
                throw new ArgumentNullException("tempTree");

            if (logBox == null)
                throw new ArgumentNullException("logBox");

            GDMTree mainTree = context.Tree;

            StringList fams = new StringList();
            StringList names = new StringList();

            try {
                logBox.AppendText(LangMan.LS(LSID.SearchMatches) + CRLF);

                int mainCount = mainTree.RecordsCount;
                for (int i = 0; i < mainCount; i++) {
                    GDMIndividualRecord iRec = mainTree[i] as GDMIndividualRecord;
                    if (iRec != null) {
                        int idx = names.AddObject(GKUtils.GetNameString(iRec, true, false), new List<GDMIndividualRecord>());
                        ((IList<GDMIndividualRecord>)names.GetObject(idx)).Add(iRec);

                        var parts = GKUtils.GetNameParts(mainTree, iRec);
                        fams.AddObject(context.Culture.NormalizeSurname(parts.Surname, iRec.Sex == GDMSex.svFemale), null);
                    }
                }

                int tempCount = tempTree.RecordsCount;
                for (int i = 0; i < tempCount; i++) {
                    GDMIndividualRecord iRec = tempTree[i] as GDMIndividualRecord;
                    if (iRec != null) {
                        string tm = GKUtils.GetNameString(iRec, true, false);
                        int idx = names.IndexOf(tm);
                        if (idx >= 0) {
                            ((IList<GDMIndividualRecord>)names.GetObject(idx)).Add(iRec);
                        }

                        var parts = GKUtils.GetNameParts(tempTree, iRec);
                        tm = context.Culture.NormalizeSurname(parts.Surname, iRec.Sex == GDMSex.svFemale);
                        idx = fams.IndexOf(tm);
                        if (idx >= 0) {
                            fams.SetObject(idx, 1);
                        }
                    }
                }

                for (int i = fams.Count - 1; i >= 0; i--) {
                    if (fams.GetObject(i) == null || fams[i] == "?")
                        fams.Delete(i);
                }

                for (int i = names.Count - 1; i >= 0; i--) {
                    var lst = (IList<GDMIndividualRecord>)names.GetObject(i);

                    if (lst.Count == 1) {
                        names.Delete(i);
                    }
                }

                int famsCount = fams.Count;
                if (famsCount != 0) {
                    logBox.AppendText(LangMan.LS(LSID.SimilarSurnames) + CRLF);
                    for (int i = 0; i < famsCount; i++) {
                        logBox.AppendText("    " + fams[i] + CRLF);
                    }
                }

                int namesCount = names.Count;
                if (namesCount != 0) {
                    logBox.AppendText(LangMan.LS(LSID.SimilarNames) + CRLF);
                    for (int i = 0; i < namesCount; i++) {
                        logBox.AppendText("    " + names[i] + CRLF);
                        var lst = (IList<GDMIndividualRecord>) names.GetObject(i);

                        int num5 = lst.Count;
                        for (int j = 0; j < num5; j++) {
                            GDMIndividualRecord iRec = lst[j];
                            logBox.AppendText("      * " + GKUtils.GetNameString(iRec, true, false) + " " + GKUtils.GetLifeStr(iRec) + CRLF);
                        }
                    }
                }

                if (famsCount == 0 && namesCount == 0) {
                    logBox.AppendText(LangMan.LS(LSID.MatchesNotFound) + CRLF);
                }
            } finally {
                int namesCount = names.Count;
                for (int i = 0; i < namesCount; i++) {
                    IDisposable inst = names.GetObject(i) as IDisposable;
                    if (inst != null) inst.Dispose();
                }
                names.Dispose();

                fams.Dispose();
            }
        }

        #endregion

        #region Places Management

        private static string fMask;
        private static Regex fRegexMask;

        private static bool IsMatchesMask(string str, string mask)
        {
            if (string.IsNullOrEmpty(mask) || mask == "*") {
                return true;
            }

            if (fMask != mask) {
                fMask = mask;
                fRegexMask = new Regex(GKUtils.PrepareMask(fMask), GKUtils.RegexOpts);
            }

            return fRegexMask.IsMatch(str, 0);
        }

        public static void SearchPlaces_Clear(StringList placesList)
        {
            if (placesList == null)
                throw new ArgumentNullException("placesList");

            placesList.Clear();
        }

        private static void SearchPlaces_CheckEventPlace(GDMTree tree, StringList placesList, GDMCustomEvent evt, string filter, bool checkLocation)
        {
            if (!evt.HasPlace) return;
            string placeStr = evt.Place.StringValue;
            if (string.IsNullOrEmpty(placeStr)) return;

            if (!IsMatchesMask(placeStr, filter)) return;

            if (checkLocation) {
                var locRec = tree.GetPtrValue<GDMLocationRecord>(evt.Place.Location);
                if (locRec != null) {
                    if (locRec.Names.Count > 1 || locRec.TopLevels.Count > 0) {
                        placeStr = "[**] " + placeStr;
                    } else {
                        placeStr = "[*] " + placeStr;
                    }
                }
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

        public static void SearchPlaces(GDMTree tree, StringList placesList, IProgressController pc, string filter = "*", bool checkLocation = true)
        {
            if (tree == null)
                throw new ArgumentNullException("tree");

            if (placesList == null)
                throw new ArgumentNullException("placesList");

            if (pc == null)
                throw new ArgumentNullException("pc");

            SearchPlaces_Clear(placesList);

            try {
                int recsCount = tree.RecordsCount;
                pc.Begin(LangMan.LS(LSID.PlacesPrepare), recsCount);

                for (int i = 0; i < recsCount; i++) {
                    pc.Increment();

                    var evsRec = tree[i] as GDMRecordWithEvents;
                    if (evsRec != null && evsRec.HasEvents) {
                        int num2 = evsRec.Events.Count;
                        for (int j = 0; j < num2; j++) {
                            GDMCustomEvent evt = evsRec.Events[j];

                            SearchPlaces_CheckEventPlace(tree, placesList, evt, filter, checkLocation);
                        }
                    }
                }
            } finally {
                pc.End();
            }
        }

        #endregion

        #region Tree fragments

        public static List<List<GDMRecord>> SearchTreeFragments(GDMTree tree, IProgressController progress)
        {
            var result = new List<List<GDMRecord>>();

            if (progress != null) {
                progress.Begin(LangMan.LS(LSID.CheckFamiliesConnection), tree.RecordsCount);
            }

            var prepared = new List<GDMRecord>();
            try {
                int num = tree.RecordsCount;
                for (int i = 0; i < num; i++) {
                    GDMIndividualRecord iRec = tree[i] as GDMIndividualRecord;
                    if (iRec != null) {
                        if (prepared.IndexOf(iRec) < 0) {
                            var groupRecords = new List<GDMRecord>();
                            WalkTree(tree, iRec, TreeWalkMode.twmAll, groupRecords);
                            result.Add(groupRecords);
                            prepared.AddRange(groupRecords);
                        }
                    }

                    if (progress != null) {
                        progress.Increment();
                    }
                }
            } finally {
                prepared.Clear();

                if (progress != null) {
                    progress.End();
                }
            }

            return result;
        }

        #endregion
    }
}
