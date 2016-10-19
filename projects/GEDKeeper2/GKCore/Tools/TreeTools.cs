/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
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
using System.Collections;
using System.Collections.Generic;
using System.Windows.Forms;

using GKCommon;
using GKCommon.GEDCOM;
using GKCommon.SmartGraph;
using GKCore.Interfaces;
using GKCore.Kinships;
using GKCore.Options;
using GKCore.Types;
using GKUI;

namespace GKCore.Tools
{
    /// <summary>
    /// 
    /// </summary>
    public static class TreeTools
    {
        #region Patriarchs Search

        private static bool PL_SearchAnc(GEDCOMIndividualRecord descendant, GEDCOMIndividualRecord searchRec, bool onlyMaleLine)
        {
            if (descendant == null) return false;

            bool res = (descendant == searchRec);

            if (!res && descendant.ChildToFamilyLinks.Count > 0)
            {
                GEDCOMFamilyRecord family = descendant.ChildToFamilyLinks[0].Family;

                GEDCOMIndividualRecord ancestor = family.GetHusband();
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

        public static bool PL_SearchDesc(GEDCOMIndividualRecord ancestorRec, GEDCOMIndividualRecord searchRec, out GEDCOMIndividualRecord cross)
        {
            cross = null;

            int num = ancestorRec.SpouseToFamilyLinks.Count;
            for (int i = 0; i < num; i++)
            {
                GEDCOMFamilyRecord family = ancestorRec.SpouseToFamilyLinks[i].Family;
                GEDCOMIndividualRecord spouse = family.GetSpouseBy(ancestorRec);

                bool res;
                if (spouse != null)
                {
                    res = PL_SearchAnc(spouse, searchRec, (ancestorRec.Sex == GEDCOMSex.svFemale));
                    if (res) {
                        cross = ancestorRec;
                        return res;
                    }
                }

                if (ancestorRec.Sex == GEDCOMSex.svMale) {
                    int num2 = family.Childrens.Count;
                    for (int j = 0; j < num2; j++)
                    {
                        GEDCOMIndividualRecord child = family.Childrens[j].Value as GEDCOMIndividualRecord;

                        res = PL_SearchDesc(child, searchRec, out cross);
                        if (res) return true;
                    }
                }
            }

            return false;
        }

        public static GEDCOMFamilyRecord PL_SearchIntersection(GEDCOMIndividualRecord ancestor, GEDCOMIndividualRecord searchRec)
        {
            int num = ancestor.SpouseToFamilyLinks.Count;
            for (int i = 0; i < num; i++)
            {
                GEDCOMFamilyRecord family = ancestor.SpouseToFamilyLinks[i].Family;
                GEDCOMIndividualRecord spouse = family.GetSpouseBy(ancestor);

                if (spouse != null)
                {
                    bool res = PL_SearchAnc(spouse, searchRec, (ancestor.Sex == GEDCOMSex.svFemale));
                    if (res) return family;
                }

                if (ancestor.Sex == GEDCOMSex.svMale)
                {
                    int num2 = family.Childrens.Count;
                    for (int j = 0; j < num2; j++)
                    {
                        GEDCOMIndividualRecord child = family.Childrens[j].Value as GEDCOMIndividualRecord;

                        GEDCOMFamilyRecord res = PL_SearchIntersection(child, searchRec);
                        if (res != null) return res;
                    }
                }
            }

            return null;
        }

        public static void GenPatriarchsGraphviz(IBaseWindow aBase, string outpath, int minGens, bool loneSuppress = true)
        {
            if (aBase == null)
                throw new ArgumentNullException("aBase");

            string[] options = { "ratio=auto" };
            GraphvizWriter gvw = new GraphvizWriter("Family Tree", options);

            using (ExtList<PatriarchObj> patList = aBase.Context.GetPatriarchsLinks(minGens, false, loneSuppress))
            {
                int num = patList.Count;
                for (int i = 0; i < num; i++) {
                    PatriarchObj pObj = patList[i];

                    if ((!loneSuppress) || (loneSuppress && pObj.HasLinks)) {
                        string color = (pObj.IRec.Sex == GEDCOMSex.svFemale) ? "pink" : "blue";
                        gvw.WriteNode(pObj.IRec.XRef, pObj.IRec.GetNameString(true, false), "filled", color, "box");
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

        #region Tree Check

        private static void ReformNote(GEDCOMTree tree, GEDCOMNotes note)
        {
            StringList strData = new StringList();
            try
            {
                strData.Text = note.Notes.Text;

                GEDCOMNoteRecord noteRec = tree.CreateNote();
                noteRec.Note = strData;

                note.Clear();
                note.Value = noteRec;
            }
            finally
            {
                strData.Dispose();
            }
        }

        private static void ReformMultimediaLink(GEDCOMTree tree, GEDCOMMultimediaLink mmLink)
        {
            try
            {
                string title = mmLink.Title;
                GEDCOMMultimediaRecord mmRec = tree.CreateMultimedia();

                int num = mmLink.FileReferences.Count;
                for (int i = 0; i < num; i++)
                {
                    GEDCOMFileReference srcFileRef = mmLink.FileReferences[i];
                    GEDCOMFileReferenceWithTitle tgtFileRef = new GEDCOMFileReferenceWithTitle(tree, mmRec, "", "");

                    tgtFileRef.LinkFile(srcFileRef.StringValue);

                    if (srcFileRef.MultimediaFormat != GEDCOMMultimediaFormat.mfNone)
                    {
                        tgtFileRef.MultimediaFormat = srcFileRef.MultimediaFormat;
                    }
                    if (srcFileRef.MediaType != GEDCOMMediaType.mtNone)
                    {
                        tgtFileRef.MediaType = srcFileRef.MediaType;
                    }

                    mmRec.FileReferences.Add(tgtFileRef);
                }

                mmLink.Clear();
                mmLink.Value = mmRec;
                mmLink.Title = title;
            }
            finally
            {
            }
        }

        private static void ReformSourceCitation(GEDCOMTree tree, GEDCOMSourceCitation sourCit)
        {
        }

        private static void CheckRecord_PrepareTag(GEDCOMTree tree, GEDCOMFormat format, GEDCOMTagWithLists tag)
        {
            int num = tag.MultimediaLinks.Count;
            for (int i = 0; i < num; i++) {
                GEDCOMMultimediaLink mmLink = tag.MultimediaLinks[i];
                if (!mmLink.IsPointer) ReformMultimediaLink(tree, mmLink);
            }

            num = tag.Notes.Count;
            for (int i = 0; i < num; i++) {
                GEDCOMNotes note = tag.Notes[i];
                if (!note.IsPointer) ReformNote(tree, note);
            }

            num = tag.SourceCitations.Count;
            for (int i = 0; i < num; i++) {
                GEDCOMSourceCitation sourCit = tag.SourceCitations[i];
                if (!sourCit.IsPointer) ReformSourceCitation(tree, sourCit);
            }
        }

        private static void CheckRecord_RepairTag(GEDCOMTree tree, GEDCOMFormat format, GEDCOMTagWithLists tag)
        {
            for (int i = tag.MultimediaLinks.Count - 1; i >= 0; i--) {
                GEDCOMMultimediaLink mmLink = tag.MultimediaLinks[i];
                if (mmLink.IsPointer && mmLink.Value == null) tag.MultimediaLinks.DeleteAt(i);
            }

            for (int i = tag.Notes.Count - 1; i >= 0; i--) {
                GEDCOMNotes note = tag.Notes[i];
                if (note.IsPointer && note.Value == null) tag.Notes.DeleteAt(i);
            }

            for (int i = tag.SourceCitations.Count - 1; i >= 0; i--) {
                GEDCOMSourceCitation sourCit = tag.SourceCitations[i];
                if (sourCit.IsPointer && sourCit.Value == null) tag.SourceCitations.DeleteAt(i);
            }
        }

        private static void CheckRecord_PreparePtr(GEDCOMTree tree, GEDCOMFormat format, GEDCOMPointerWithNotes ptr)
        {
            /*GEDCOMRecord val = ptr.Value;
			if (!string.IsNullOrEmpty(ptr.XRef) && val == null) {
				ptr.Value = null;
			}*/

            int num = ptr.Notes.Count;
            for (int i = 0; i < num; i++)
            {
                GEDCOMNotes note = ptr.Notes[i];
                if (!note.IsPointer) ReformNote(tree, note);
            }
        }

        private static void CheckRecord_EventPlace(GEDCOMCustomEvent aEvent)
        {
            GEDCOMPlace place = aEvent.Detail.Place;
            if (place.Location.XRef != "" && place.Location.Value == null)
            {
                place.Location.XRef = "";
            }
            if (place.StringValue != "")
            {
                GEDCOMLocationRecord loc = place.Location.Value as GEDCOMLocationRecord;
                if (loc != null && place.StringValue != loc.LocationName)
                {
                    place.StringValue = loc.LocationName;
                }
            }
        }

        private static void CheckRecord_AttrCompatible(GEDCOMTree tree, GEDCOMFormat format, GEDCOMIndividualRecord iRec, GEDCOMCustomEvent aEvent)
        {
        }

        private static void CheckRecord_URefCompatible(GEDCOMIndividualRecord iRec, GEDCOMUserReference userRef)
        {
        }

        private static void CheckRecord_Individual(GEDCOMTree tree, GEDCOMFormat format, GEDCOMIndividualRecord iRec, ValuesCollection valuesCollection)
        {
            if (format == GEDCOMFormat.gf_Native)
            {
                int num = iRec.Events.Count;
                for (int i = 0; i < num; i++)
                {
                    GEDCOMCustomEvent evt = iRec.Events[i];

                    CheckRecord_EventPlace(evt);
                    CheckRecord_AttrCompatible(tree, format, iRec, evt);
                    CheckRecord_RepairTag(tree, format, evt.Detail);

                    GKUtils.CollectEventValues(evt, valuesCollection);
                }

                int num2 = iRec.UserReferences.Count;
                for (int i = 0; i < num2; i++)
                {
                    CheckRecord_URefCompatible(iRec, iRec.UserReferences[i]);
                }
            }
            else
            {
                int num3 = iRec.Events.Count;
                for (int i = 0; i < num3; i++)
                {
                    CheckRecord_PrepareTag(tree, format, iRec.Events[i].Detail);
                }

                int num4 = iRec.ChildToFamilyLinks.Count;
                for (int i = 0; i < num4; i++)
                {
                    CheckRecord_PreparePtr(tree, format, iRec.ChildToFamilyLinks[i]);
                }

                int num5 = iRec.SpouseToFamilyLinks.Count;
                for (int i = 0; i < num5; i++)
                {
                    CheckRecord_PreparePtr(tree, format, iRec.SpouseToFamilyLinks[i]);
                }

                int num6 = iRec.Associations.Count;
                for (int i = 0; i < num6; i++)
                {
                    CheckRecord_PreparePtr(tree, format, iRec.Associations[i]);
                }
            }

            for (int i = iRec.ChildToFamilyLinks.Count - 1; i >= 0; i--)
            {
                if (iRec.ChildToFamilyLinks[i].Family == null)
                    iRec.ChildToFamilyLinks.DeleteAt(i);
            }

            for (int i = iRec.SpouseToFamilyLinks.Count - 1; i >= 0; i--)
            {
                if (iRec.SpouseToFamilyLinks[i].Family == null)
                    iRec.SpouseToFamilyLinks.DeleteAt(i);
            }
            
            MainWin.Instance.NamesTable.ImportNames(iRec);
        }

        private static void CheckRecord_Family(GEDCOMTree tree, GEDCOMFormat format, GEDCOMFamilyRecord fam, ValuesCollection valuesCollection)
        {
            if (format == GEDCOMFormat.gf_Native)
            {
                int num = fam.Events.Count;
                for (int i = 0; i < num; i++)
                {
                    CheckRecord_EventPlace(fam.Events[i]);
                }
            }
            else
            {
                int num2 = fam.Events.Count;
                for (int i = 0; i < num2; i++)
                {
                    CheckRecord_PrepareTag(tree, format, fam.Events[i].Detail);
                }
            }

            for (int i = fam.Childrens.Count - 1; i >= 0; i--)
            {
                if (fam.Childrens[i].Value == null)
                    fam.Childrens.DeleteAt(i);
            }

            GEDCOMRecord val = fam.Husband.Value;
            if (!string.IsNullOrEmpty(fam.Husband.XRef) && val == null) {
                fam.Husband.Value = null;
            }
            
            val = fam.Wife.Value;
            if (!string.IsNullOrEmpty(fam.Wife.XRef) && val == null) {
                fam.Wife.Value = null;
            }
            
            fam.SortChilds();
        }

        private static void CheckRecord_Group(GEDCOMGroupRecord group)
        {
            for (int i = group.Members.Count - 1; i >= 0; i--)
            {
                GEDCOMPointer ptr = group.Members[i];
                GEDCOMIndividualRecord irec = ptr.Value as GEDCOMIndividualRecord;
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

        private static void CheckRecord_Source(GEDCOMSourceRecord src)
        {
            for (int i = src.RepositoryCitations.Count - 1; i >= 0; i--) {
                GEDCOMRecord val = src.RepositoryCitations[i].Value;
                if (val == null) {
                    src.RepositoryCitations.DeleteAt(i);
                }
            }
        }

        private static void CheckRecord(GEDCOMTree tree, GEDCOMRecord rec, GEDCOMFormat format, ValuesCollection valuesCollection)
        {
            rec.RequireUID();

            if (format != GEDCOMFormat.gf_Native)
            {
                int num = rec.MultimediaLinks.Count;
                for (int i = 0; i < num; i++)
                {
                    GEDCOMMultimediaLink mmLink = rec.MultimediaLinks[i];
                    if (!mmLink.IsPointer) ReformMultimediaLink(tree, mmLink);
                }

                num = rec.Notes.Count;
                for (int i = 0; i < num; i++)
                {
                    GEDCOMNotes note = rec.Notes[i];
                    if (!note.IsPointer) ReformNote(tree, note);
                }

                num = rec.SourceCitations.Count;
                for (int i = 0; i < num; i++)
                {
                    GEDCOMSourceCitation sourCit = rec.SourceCitations[i];
                    if (!sourCit.IsPointer) ReformSourceCitation(tree, sourCit);
                }
            }

            switch (rec.RecordType) {
                case GEDCOMRecordType.rtIndividual:
                    CheckRecord_Individual(tree, format, rec as GEDCOMIndividualRecord, valuesCollection);
                    break;

                case GEDCOMRecordType.rtFamily:
                    CheckRecord_Family(tree, format, rec as GEDCOMFamilyRecord, valuesCollection);
                    break;

                case GEDCOMRecordType.rtGroup:
                    CheckRecord_Group(rec as GEDCOMGroupRecord);
                    break;

                case GEDCOMRecordType.rtSource:
                    CheckRecord_Source(rec as GEDCOMSourceRecord);
                    break;
            }
        }

        private static void CorrectIds(GEDCOMTree tree, IProgressController pc)
        {
            pc.ProgressInit(LangMan.LS(LSID.LSID_IDsCorrect), tree.RecordsCount);
            XRefReplacer repMap = new XRefReplacer();
            try
            {
                int num = tree.RecordsCount;
                for (int i = 0; i < num; i++)
                {
                    GEDCOMRecord rec = tree[i];
                    if (rec.GetId() < 0)
                    {
                        string newXRef = tree.XRefIndex_NewXRef(rec);
                        repMap.AddXRef(rec, rec.XRef, newXRef);
                        rec.XRef = newXRef;
                    }
                    pc.ProgressStep();
                }

                tree.Header.ReplaceXRefs(repMap);
                pc.ProgressInit(LangMan.LS(LSID.LSID_IDsCorrect), repMap.Count);

                int num2 = repMap.Count;
                for (int i = 0; i < num2; i++)
                {
                    GEDCOMRecord rec = repMap[i].Rec;
                    rec.ReplaceXRefs(repMap);
                    pc.ProgressStep();
                }
            }
            finally
            {
                repMap.Dispose();
                pc.ProgressDone();
            }
        }

        public static bool CheckGEDCOMFormat(GEDCOMTree tree, ValuesCollection valuesCollection, IProgressController pc)
        {
            if (tree == null)
                throw new ArgumentNullException("tree");

            if (valuesCollection == null)
                throw new ArgumentNullException("valuesCollection");

            if (pc == null)
                throw new ArgumentNullException("pc");

            bool result = false;

            try
            {
                pc.ProgressInit(LangMan.LS(LSID.LSID_FormatCheck), tree.RecordsCount);
                try
                {
                    GEDCOMFormat format = tree.GetGEDCOMFormat();
                    bool idCheck = true;


                    // remove a deprecated features
                    if (format == GEDCOMFormat.gf_Native)
                    {
                        GEDCOMHeader header = tree.Header;
                        GEDCOMTag tag;

                        tag = header.FindTag("_ADVANCED", 0);
                        if (tag != null) header.DeleteTag("_ADVANCED");

                        tag = header.FindTag("_EXT_NAME", 0);
                        if (tag != null) header.DeleteTag("_EXT_NAME");
                    }


                    int num = tree.RecordsCount;
                    for (int i = 0; i < num; i++)
                    {
                        GEDCOMRecord rec = tree[i];
                        CheckRecord(tree, rec, format, valuesCollection);

                        if (format != GEDCOMFormat.gf_Native && idCheck && rec.GetId() < 0)
                        {
                            idCheck = false;
                        }

                        pc.ProgressStep();
                    }

                    if (!idCheck && GKUtils.ShowQuestion(LangMan.LS(LSID.LSID_IDsCorrectNeed)) == DialogResult.Yes)
                    {
                        CorrectIds(tree, pc);
                    }

                    result = true;
                }
                finally
                {
                    pc.ProgressDone();
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("TreeTools.CheckGEDCOMFormat(): " + ex.Message);
                GKUtils.ShowError(LangMan.LS(LSID.LSID_CheckGedComFailed));
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

        public static void TreeWalk(GEDCOMIndividualRecord iRec, TreeWalkMode mode, List<GEDCOMRecord> walkList)
        {
            if (iRec == null)
                throw new ArgumentNullException("iRec");

            if (walkList == null)
                throw new ArgumentNullException("walkList");

            TreeWalkInt(iRec, mode, walkList);
        }

        private static void TreeWalkInt(GEDCOMIndividualRecord iRec, TreeWalkMode mode, List<GEDCOMRecord> walkList)
        {
            if (iRec == null || walkList.IndexOf(iRec) >= 0) return;

            walkList.Add(iRec);

            if (mode == TreeWalkMode.twmNone) return;
            
            if ((mode == TreeWalkMode.twmAll || mode == TreeWalkMode.twmAncestors) && iRec.ChildToFamilyLinks.Count > 0)
            {
                GEDCOMIndividualRecord father, mother;
                iRec.GetParents(out father, out mother);

                TreeWalkInt(father, mode, walkList);
                TreeWalkInt(mother, mode, walkList);
            }

            // twmAll, twmFamily, twmDescendants
            if (mode < TreeWalkMode.twmAncestors || mode == TreeWalkMode.twmDescendants)
            {
                int num = iRec.SpouseToFamilyLinks.Count;
                for (int i = 0; i < num; i++)
                {
                    GEDCOMFamilyRecord family = iRec.SpouseToFamilyLinks[i].Family;

                    GEDCOMPointer spPtr = ((iRec.Sex == GEDCOMSex.svMale) ? family.Wife : family.Husband);
                    GEDCOMIndividualRecord spouse = spPtr.Value as GEDCOMIndividualRecord;

                    TreeWalkMode intMode = ((mode == TreeWalkMode.twmAll) ? TreeWalkMode.twmAll : TreeWalkMode.twmNone);
                    TreeWalkInt(spouse, intMode, walkList);

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

                    int num2 = family.Childrens.Count;
                    for (int j = 0; j < num2; j++)
                    {
                        GEDCOMIndividualRecord child = (GEDCOMIndividualRecord)family.Childrens[j].Value;
                        TreeWalkInt(child, intMode, walkList);
                    }
                }
            }
        }

        #endregion

        #region KinshipsGraph

        public static KinshipsGraph SearchKinshipsGraph(GEDCOMIndividualRecord iRec)
        {
            if (iRec == null)
                throw new ArgumentNullException("iRec");

            KinshipsGraph graph = new KinshipsGraph();

            SearchKGInt(null, iRec, graph, RelationKind.rkUndefined, RelationKind.rkUndefined);

            return graph;
        }

        private static void SearchKGInt(IVertex prevNode, GEDCOMIndividualRecord iRec,
                                        KinshipsGraph graph, RelationKind relation, RelationKind inverseRelation)
        {
            if (iRec == null) return;

            IVertex currNode = graph.FindVertex(iRec.XRef);
            if (currNode != null) {
                if (prevNode != null) {
                    graph.AddRelation(prevNode, currNode, relation, inverseRelation);
                }

                return;
            } else {
                currNode = graph.AddIndividual(iRec);

                if (prevNode != null) {
                    graph.AddRelation(prevNode, currNode, relation, inverseRelation);
                }
            }

            if (iRec.ChildToFamilyLinks.Count > 0)
            {
                GEDCOMIndividualRecord father, mother;
                iRec.GetParents(out father, out mother);

                SearchKGInt(currNode, father, graph, RelationKind.rkParent, RelationKind.rkChild);
                SearchKGInt(currNode, mother, graph, RelationKind.rkParent, RelationKind.rkChild);
            }

            int num = iRec.SpouseToFamilyLinks.Count;
            for (int i = 0; i < num; i++)
            {
                GEDCOMFamilyRecord family = iRec.SpouseToFamilyLinks[i].Family;

                GEDCOMPointer spPtr = ((iRec.Sex == GEDCOMSex.svMale) ? family.Wife : family.Husband);
                GEDCOMIndividualRecord spouse = spPtr.Value as GEDCOMIndividualRecord;

                SearchKGInt(currNode, spouse, graph, RelationKind.rkSpouse, RelationKind.rkSpouse);

                int num2 = family.Childrens.Count;
                for (int j = 0; j < num2; j++)
                {
                    GEDCOMIndividualRecord child = (GEDCOMIndividualRecord)family.Childrens[j].Value;
                    SearchKGInt(currNode, child, graph, RelationKind.rkChild, RelationKind.rkParent);
                }
            }
        }

        #endregion

        #region Tree Merge

        public static void TreeMerge(GEDCOMTree mainTree, string fileName, TextBox logBox)
        {
            if (mainTree == null)
                throw new ArgumentNullException("mainTree");

            if (logBox == null)
                throw new ArgumentNullException("logBox");

            logBox.Clear();
            logBox.AppendText(string.Format(LangMan.LS(LSID.LSID_MainBaseSize), mainTree.RecordsCount.ToString()) + "\r\n");

            GEDCOMTree extTree = new GEDCOMTree();

            XRefReplacer repMap = new XRefReplacer();
            try
            {
                extTree.LoadFromFile(fileName);
                extTree.Header.Clear();
                while (extTree.RecordsCount > 0)
                {
                    GEDCOMRecord rec = extTree.Extract(0);
                    string newXRef = mainTree.XRefIndex_NewXRef(rec);
                    repMap.AddXRef(rec, rec.XRef, newXRef);
                    rec.XRef = newXRef;
                    rec.ResetOwner(mainTree);
                    mainTree.AddRecord(rec);
                }

                int num = repMap.Count;
                for (int i = 0; i < num; i++) {
                    GEDCOMRecord rec = repMap[i].Rec;
                    rec.ReplaceXRefs(repMap);
                }

                logBox.AppendText(string.Format(LangMan.LS(LSID.LSID_MainBaseSize), mainTree.RecordsCount.ToString()) + "\r\n");
            }
            finally
            {
                repMap.Dispose();
                extTree.Dispose();
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
            cdEmptyFamily
        }

        public enum CheckSolve
        {
            csSkip,
            csSetIsDead,
            csDefineSex,
            csRemove
        }

        public sealed class CheckObj
        {
            public string Comment;
            public CheckDiag Diag;
            public GEDCOMRecord Rec;
            public CheckSolve Solve;

            public CheckObj(GEDCOMRecord rec, CheckDiag diag, CheckSolve solve)
            {
                this.Rec = rec;
                this.Diag = diag;
                this.Solve = solve;
            }

            public string GetRecordName()
            {
                string result = "[" + this.Rec.XRef + "] ";

                switch (this.Rec.RecordType)
                {
                    case GEDCOMRecordType.rtIndividual:
                        result = result + ((GEDCOMIndividualRecord)this.Rec).GetNameString(true, false);
                        break;

                    case GEDCOMRecordType.rtFamily:
                        result = result + GKUtils.GetFamilyString((GEDCOMFamilyRecord)this.Rec);
                        break;
                }

                return result;
            }
        }

        private static void CheckIndividualRecord(GEDCOMIndividualRecord iRec, List<CheckObj> checksList)
        {
            if (iRec.FindEvent("DEAT") == null)
            {
                int age = GKUtils.GetAge(iRec, -1);

                if (age != -1 && age >= GKConsts.PROVED_LIFE_LENGTH)
                {
                    CheckObj checkObj = new CheckObj(iRec, CheckDiag.cdPersonLonglived, CheckSolve.csSetIsDead);
                    checkObj.Comment = string.Format(LangMan.LS(LSID.LSID_PersonLonglived), age);
                    checksList.Add(checkObj);
                }
            }

            GEDCOMSex sex = iRec.Sex;
            if (sex < GEDCOMSex.svMale || sex >= GEDCOMSex.svUndetermined)
            {
                CheckObj checkObj = new CheckObj(iRec, CheckDiag.cdPersonSexless, CheckSolve.csDefineSex);
                checkObj.Comment = LangMan.LS(LSID.LSID_PersonSexless);
                checksList.Add(checkObj);
            }

            int yBirth = GEDCOMUtils.GetRelativeYear(iRec, "BIRT");
            int yDeath = GEDCOMUtils.GetRelativeYear(iRec, "DEAT");
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

            GEDCOMIndividualRecord iDummy;
            iAge = GKUtils.GetFirstbornAge(iRec, out iDummy);
            if (iAge > 0 && (iAge <= 13 || iAge >= 50))
            {
                CheckObj checkObj = new CheckObj(iRec, CheckDiag.cdStrangeParent, CheckSolve.csSkip);
                checkObj.Comment = string.Format(LangMan.LS(LSID.LSID_StrangeParent), iAge.ToString());
                checksList.Add(checkObj);
            }
        }

        private static void CheckFamilyRecord(GEDCOMFamilyRecord fRec, List<CheckObj> checksList)
        {
            bool empty = (fRec.Notes.Count == 0 && fRec.SourceCitations.Count == 0 && fRec.MultimediaLinks.Count == 0 && fRec.UserReferences.Count == 0);
            empty = empty && (fRec.Events.Count == 0 && fRec.Childrens.Count == 0 && fRec.SpouseSealings.Count == 0);
            empty = empty && (fRec.Husband.Value == null && fRec.Wife.Value == null);

            if (empty) {
                CheckObj checkObj = new CheckObj(fRec, CheckDiag.cdEmptyFamily, CheckSolve.csRemove);
                checkObj.Comment = LangMan.LS(LSID.LSID_EmptyFamily);
                checksList.Add(checkObj);
            }
        }

        public static void CheckBase(IBaseWindow aBase, List<CheckObj> checksList)
        {
            if (aBase == null)
                throw new ArgumentNullException("aBase");

            if (checksList == null)
                throw new ArgumentNullException("checksList");

            try
            {
                GEDCOMTree tree = aBase.Tree;

                aBase.ProgressInit(LangMan.LS(LSID.LSID_ToolOp_7), tree.RecordsCount);
                checksList.Clear();

                int num = tree.RecordsCount;
                for (int i = 0; i < num; i++) {
                    aBase.ProgressStep();

                    GEDCOMRecord rec = tree[i];

                    switch (rec.RecordType) {
                        case GEDCOMRecordType.rtIndividual:
                            CheckIndividualRecord(rec as GEDCOMIndividualRecord, checksList);
                            break;

                        case GEDCOMRecordType.rtFamily:
                            CheckFamilyRecord(rec as GEDCOMFamilyRecord, checksList);
                            break;
                    }
                }
            }
            finally
            {
                aBase.ProgressDone();
            }
        }

        public static void RepairProblem(IBaseWindow aBase, CheckObj checkObj)
        {
            if (aBase == null)
                throw new ArgumentNullException("aBase");

            if (checkObj == null)
                throw new ArgumentNullException("checkObj");

            GEDCOMTree tree = aBase.Tree;
            GEDCOMIndividualRecord iRec;

            switch (checkObj.Diag)
            {
                case CheckDiag.cdPersonLonglived:
                    iRec = checkObj.Rec as GEDCOMIndividualRecord;
                    aBase.Context.CreateEventEx(iRec, "DEAT", "", "");
                    //this.Base.ChangeRecord(iRec);
                    break;

                case CheckDiag.cdPersonSexless:
                    iRec = checkObj.Rec as GEDCOMIndividualRecord;
                    aBase.CheckPersonSex(iRec);
                    //this.Base.ChangeRecord(iRec);
                    break;

                case CheckDiag.cdEmptyFamily:
                    tree.DeleteRecord(checkObj.Rec);
                    break;
            }
        }

        #endregion

        #region Tree Split

        private static void _CheckRelations_AddRel(List<GEDCOMRecord> splitList, GEDCOMRecord aRec)
        {
            if (splitList.IndexOf(aRec) < 0)
            {
                splitList.Add(aRec);
            }
        }

        private static void _CheckRelations_CheckRecord(List<GEDCOMRecord> splitList, GEDCOMRecord rec)
        {
            int num = rec.MultimediaLinks.Count;
            for (int i = 0; i < num; i++)
            {
                _CheckRelations_AddRel(splitList, rec.MultimediaLinks[i].Value);
            }

            int num2 = rec.Notes.Count;
            for (int i = 0; i < num2; i++)
            {
                _CheckRelations_AddRel(splitList, rec.Notes[i].Value);
            }

            int num3 = rec.SourceCitations.Count;
            for (int i = 0; i < num3; i++)
            {
                _CheckRelations_AddRel(splitList, rec.SourceCitations[i].Value);
            }
        }

        private static void _CheckRelations_CheckTag(List<GEDCOMRecord> splitList, GEDCOMTagWithLists tag)
        {
            int num = tag.MultimediaLinks.Count;
            for (int i = 0; i < num; i++)
            {
                _CheckRelations_AddRel(splitList, tag.MultimediaLinks[i].Value);
            }

            int num2 = tag.Notes.Count;
            for (int i = 0; i < num2; i++)
            {
                _CheckRelations_AddRel(splitList, tag.Notes[i].Value);
            }

            int num3 = tag.SourceCitations.Count;
            for (int i = 0; i < num3; i++)
            {
                _CheckRelations_AddRel(splitList, tag.SourceCitations[i].Value);
            }
        }

        private static void _CheckRelations_CheckIndividual(List<GEDCOMRecord> splitList, GEDCOMIndividualRecord iRec)
        {
            _CheckRelations_CheckRecord(splitList, iRec);

            int num = iRec.ChildToFamilyLinks.Count;
            for (int i = 0; i < num; i++)
            {
                _CheckRelations_AddRel(splitList, iRec.ChildToFamilyLinks[i].Family);
            }

            int num2 = iRec.SpouseToFamilyLinks.Count;
            for (int i = 0; i < num2; i++)
            {
                _CheckRelations_AddRel(splitList, iRec.SpouseToFamilyLinks[i].Family);
            }

            int num3 = iRec.Events.Count;
            for (int i = 0; i < num3; i++)
            {
                _CheckRelations_CheckTag(splitList, iRec.Events[i].Detail);
            }

            int num4 = iRec.IndividualOrdinances.Count;
            for (int i = 0; i < num4; i++)
            {
                _CheckRelations_CheckTag(splitList, iRec.IndividualOrdinances[i]);
            }

            int num5 = iRec.Submittors.Count;
            for (int i = 0; i < num5; i++)
            {
                _CheckRelations_AddRel(splitList, iRec.Submittors[i].Value);
            }

            int num6 = iRec.Associations.Count;
            for (int i = 0; i < num6; i++)
            {
                _CheckRelations_AddRel(splitList, iRec.Associations[i].Value);
            }

            int num7 = iRec.Aliases.Count;
            for (int i = 0; i < num7; i++)
            {
                _CheckRelations_AddRel(splitList, iRec.Aliases[i].Value);
            }

            int num8 = iRec.AncestorsInterest.Count;
            for (int i = 0; i < num8; i++)
            {
                _CheckRelations_AddRel(splitList, iRec.AncestorsInterest[i].Value);
            }

            int num9 = iRec.DescendantsInterest.Count;
            for (int i = 0; i < num9; i++)
            {
                _CheckRelations_AddRel(splitList, iRec.DescendantsInterest[i].Value);
            }

            int num10 = iRec.Groups.Count;
            for (int i = 0; i < num10; i++)
            {
                _CheckRelations_AddRel(splitList, iRec.Groups[i].Value);
            }
        }

        private static void _CheckRelations_CheckFamily(List<GEDCOMRecord> splitList, GEDCOMFamilyRecord fRec)
        {
            _CheckRelations_CheckRecord(splitList, fRec);

            int num = fRec.Events.Count;
            for (int i = 0; i < num; i++)
            {
                _CheckRelations_CheckTag(splitList, fRec.Events[i].Detail);
            }
            _CheckRelations_AddRel(splitList, fRec.Submitter.Value);

            int num2 = fRec.SpouseSealings.Count;
            for (int i = 0; i < num2; i++)
            {
                _CheckRelations_CheckTag(splitList, fRec.SpouseSealings[i]);
            }
        }

        private static void _CheckRelations_CheckSource(List<GEDCOMRecord> splitList, GEDCOMSourceRecord sRec)
        {
            _CheckRelations_CheckRecord(splitList, sRec);

            int num = sRec.RepositoryCitations.Count;
            for (int i = 0; i < num; i++) {
                _CheckRelations_AddRel(splitList, sRec.RepositoryCitations[i].Value);
            }
        }

        public static void CheckRelations(List<GEDCOMRecord> splitList)
        {
            if (splitList == null)
                throw new ArgumentNullException("splitList");

            int num = splitList.Count;
            for (int i = 0; i < num; i++)
            {
                GEDCOMRecord rec = splitList[i];
                switch (rec.RecordType)
                {
                    case GEDCOMRecordType.rtIndividual:
                        _CheckRelations_CheckIndividual(splitList, rec as GEDCOMIndividualRecord);
                        break;

                    case GEDCOMRecordType.rtFamily:
                        _CheckRelations_CheckFamily(splitList, rec as GEDCOMFamilyRecord);
                        break;

                    case GEDCOMRecordType.rtNote:
                        _CheckRelations_CheckRecord(splitList, rec);
                        break;

                    case GEDCOMRecordType.rtMultimedia:
                        _CheckRelations_CheckRecord(splitList, rec);
                        break;

                    case GEDCOMRecordType.rtSource:
                        _CheckRelations_CheckSource(splitList, rec as GEDCOMSourceRecord);
                        break;

                    case GEDCOMRecordType.rtRepository:
                        _CheckRelations_CheckRecord(splitList, rec);
                        break;

                    case GEDCOMRecordType.rtSubmitter:
                        _CheckRelations_CheckRecord(splitList, rec);
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
            public GEDCOMIndividualRecord IRec;
        }

        public static List<ULIndividual> GetUnlinkedNamesakes(GEDCOMTree tree, IProgressController pc)
        {
            if (tree == null)
                throw new ArgumentNullException("tree");

            if (pc == null)
                throw new ArgumentNullException("pc");

            List<ULIndividual> result = new List<ULIndividual>();

            Hashtable families = new Hashtable();

            pc.ProgressInit(LangMan.LS(LSID.LSID_Stage) + "1", tree.RecordsCount);

            // make a table of surnames and persons, related to these surnames
            int num = tree.RecordsCount;
            for (int i = 0; i < num; i++)
            {
                GEDCOMRecord rec = tree[i];

                if (rec is GEDCOMIndividualRecord)
                {
                    GEDCOMIndividualRecord iRec = rec as GEDCOMIndividualRecord;

                    string[] fams = GlobalOptions.CurrentCulture.GetSurnames(iRec);

                    for (int k = 0; k < fams.Length; k++)
                    {
                        string f = fams[k];
                        if (f.Length > 1)
                        {
                            List<GEDCOMIndividualRecord> ps = (List<GEDCOMIndividualRecord>)families[f];
                            if (ps == null) {
                                ps = new List<GEDCOMIndividualRecord>();
                                families[f] = ps;
                            }
                            ps.Add(iRec);
                        }
                    }
                }

                pc.ProgressStep();
            }

            pc.ProgressInit(LangMan.LS(LSID.LSID_Stage) + "2", families.Count);

            // find all persons of one surname, not related by ties of kinship
            foreach (DictionaryEntry entry in families)
            {
                string fam = (string)entry.Key;
                List<GEDCOMIndividualRecord> ps = (List<GEDCOMIndividualRecord>)entry.Value;

                int i = 0;
                while (i < ps.Count)
                {
                    GEDCOMIndividualRecord iRec = ps[i];

                    List<GEDCOMRecord> lst = new List<GEDCOMRecord>();
                    TreeWalk(iRec, TreeWalkMode.twmAll, lst);

                    int num3 = lst.Count;
                    for (int k = 0; k < num3; k++)
                    {
                        GEDCOMIndividualRecord item = lst[k] as GEDCOMIndividualRecord;

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

                pc.ProgressStep();
            }

            result.Sort(new IndividualRecordComparer());
            
            pc.ProgressDone();

            return result;
        }

        public delegate void DuplicateFoundFunc(GEDCOMIndividualRecord indivA, GEDCOMIndividualRecord indivB);

        public static void FindDuplicates(GEDCOMTree treeA, GEDCOMTree treeB, float matchThreshold,
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

            pc.ProgressInit(LangMan.LS(LSID.LSID_DuplicatesSearch), treeA.RecordsCount);
            try
            {
                for (int i = 0; i < treeA.RecordsCount; i++) {
                    GEDCOMRecord recA = treeA[i];
                    if (recA is GEDCOMIndividualRecord) {
                        for (int k = 0; k < treeB.RecordsCount; k++) {
                            GEDCOMRecord recB = treeB[k];
                            if (recB is GEDCOMIndividualRecord) {
                                GEDCOMIndividualRecord indivA = recA as GEDCOMIndividualRecord;
                                GEDCOMIndividualRecord indivB = recB as GEDCOMIndividualRecord;

                                if (indivA != indivB && indivA.IsMatch(indivB, mParams) >= matchThreshold)
                                {
                                    foundFunc(indivA, indivB);
                                }
                            }
                        }
                    }

                    pc.ProgressStep();
                    Application.DoEvents();
                }
            }
            finally
            {
                pc.ProgressDone();
            }
        }

        public static void TreeCompare(GEDCOMTree mainTree, string fileName, TextBox logBox)
        {
            if (mainTree == null)
                throw new ArgumentNullException("mainTree");

            if (logBox == null)
                throw new ArgumentNullException("logBox");

            GEDCOMTree tempTree = new GEDCOMTree();
            tempTree.LoadFromFile(fileName);

            StringList fams = new StringList();
            StringList names = new StringList();

            try
            {
                logBox.AppendText(LangMan.LS(LSID.LSID_SearchMatches) + "\r\n");

                int num = mainTree.RecordsCount;
                for (int i = 0; i < num; i++)
                {
                    GEDCOMRecord rec = mainTree[i];
                    if (rec.RecordType == GEDCOMRecordType.rtIndividual)
                    {
                        GEDCOMIndividualRecord iRec = (GEDCOMIndividualRecord)rec;

                        int idx = names.AddObject(iRec.GetNameString(true, false), new ExtList<GEDCOMIndividualRecord>());
                        ((ExtList<GEDCOMIndividualRecord>)names.GetObject(idx)).Add(iRec);

                        string fam, nam, pat;
                        GKUtils.GetNameParts(iRec, out fam, out nam, out pat);

                        fams.AddObject(GlobalOptions.CurrentCulture.NormalizeSurname(fam, iRec.Sex == GEDCOMSex.svFemale), null);
                    }
                }

                int num2 = tempTree.RecordsCount;
                for (int i = 0; i < num2; i++)
                {
                    GEDCOMRecord rec = tempTree[i];
                    if (rec.RecordType == GEDCOMRecordType.rtIndividual)
                    {
                        GEDCOMIndividualRecord iRec = (GEDCOMIndividualRecord)tempTree[i];

                        string tm = iRec.GetNameString(true, false);
                        int idx = names.IndexOf(tm);
                        if (idx >= 0)
                        {
                            ((ExtList<GEDCOMIndividualRecord>)names.GetObject(idx)).Add(iRec);
                        }

                        string fam, nam, pat;
                        GKUtils.GetNameParts(iRec, out fam, out nam, out pat);

                        tm = GlobalOptions.CurrentCulture.NormalizeSurname(fam, iRec.Sex == GEDCOMSex.svFemale);
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
                    ExtList<GEDCOMIndividualRecord> lst = (ExtList<GEDCOMIndividualRecord>)names.GetObject(i);

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
                        ExtList<GEDCOMIndividualRecord> lst = (ExtList<GEDCOMIndividualRecord>)names.GetObject(i);

                        int num5 = lst.Count;
                        for (int j = 0; j < num5; j++)
                        {
                            GEDCOMIndividualRecord iRec = lst[j];
                            logBox.AppendText("      * " + iRec.GetNameString(true, false) + " " + GKUtils.GetLifeStr(iRec) + "\r\n");
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

        public static void PlacesSearch_Clear(StringList placesList)
        {
            if (placesList == null)
                throw new ArgumentNullException("placesList");

            for (int i = placesList.Count - 1; i >= 0; i--) ((PlaceObj)placesList.GetObject(i)).Dispose();
            placesList.Clear();
        }

        private static void PlacesSearch_CheckEventPlace(StringList placesList, GEDCOMCustomEvent evt)
        {
            string placeStr = evt.Detail.Place.StringValue;
            if (string.IsNullOrEmpty(placeStr)) return;

            GEDCOMLocationRecord loc = evt.Detail.Place.Location.Value as GEDCOMLocationRecord;
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

        public static void PlacesSearch(GEDCOMTree tree, StringList placesList, IProgressController pc)
        {
            if (tree == null)
                throw new ArgumentNullException("tree");

            if (placesList == null)
                throw new ArgumentNullException("placesList");

            if (pc == null)
                throw new ArgumentNullException("pc");

            PlacesSearch_Clear(placesList);

            try
            {
                int recsCount = tree.RecordsCount;
                pc.ProgressInit(LangMan.LS(LSID.LSID_PlacesPrepare), recsCount);

                for (int i = 0; i < recsCount; i++) {
                    pc.ProgressStep();
                    GEDCOMRecord record = tree[i];

                    if (record is GEDCOMRecordWithEvents) {
                        GEDCOMRecordWithEvents evsRec = record as GEDCOMRecordWithEvents;

                        int num2 = evsRec.Events.Count;
                        for (int j = 0; j < num2; j++) {
                            GEDCOMCustomEvent evt = evsRec.Events[j];

                            PlacesSearch_CheckEventPlace(placesList, evt);
                        }
                    }
                }
            } finally {
                pc.ProgressDone();
            }
        }

        #endregion
    }
}
