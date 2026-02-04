/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;
using BSLib.DataViz.SmartGraph;
using GDModel;
using GKCore.Design;
using GKCore.Locales;
using GKCore.Utilities;

namespace GKCore.Tools
{
    public enum PGNodeType
    {
        Default,
        Patriarch,
        Intersection
    }


    /// <summary>
    /// PGNode - it's node class for Patriarchs Graph.
    /// </summary>
    public sealed class PGNode
    {
        public string FamilyXRef;
        public PGNodeType Type;
        public int Size;

        public PGNode(string label, PGNodeType type, int size = 1)
        {
            FamilyXRef = label;
            Type = type;
            Size = size;
        }
    }


    public sealed class PatriarchObj
    {
        public bool Mark;

        public GDMIndividualRecord IRec;
        public int BirthYear;
        public int DescendantsCount;
        public int DescGenerations;
        public readonly List<PatriarchObj> Links;
        public bool HasLinks;

        public object ExtData;

        public PatriarchObj()
        {
            Links = new List<PatriarchObj>();
        }
    }


    /// <summary>
    ///
    /// </summary>
    public static class PatriarchsMan
    {
        public static IList<PatriarchObj> GetPatriarchsList(
            BaseContext context, int gensMin, bool datesCheck, IProgressController progress, bool progressDone)
        {
            var patList = new List<PatriarchObj>();

            progress.Begin(LangMan.LS(LSID.PatSearch), context.Tree.RecordsCount);

            try {
                int num = context.Tree.RecordsCount;
                for (int i = 0; i < num; i++) {
                    GDMRecord rec = context.Tree[i];

                    if (rec is GDMIndividualRecord iRec) {
                        var parts = GKUtils.GetNameParts(context.Tree, iRec);

                        int birthDate = context.FindBirthYear(iRec);
                        int descGens = GKUtils.GetDescGenerations(context.Tree, iRec);

                        bool res = (iRec.ChildToFamilyLinks.Count == 0);
                        res = (res && iRec.Sex == GDMSex.svMale);
                        res = (res && /*nf != "" && nf != "?" &&*/ parts.Name != "" && parts.Name != "?");
                        res = (res && descGens >= gensMin);

                        if (datesCheck) {
                            res = (res && birthDate != 0);
                        }

                        if (res) {
                            PatriarchObj pObj = new PatriarchObj();
                            pObj.IRec = iRec;
                            pObj.BirthYear = birthDate;
                            pObj.DescendantsCount = GKUtils.GetDescendantsCount(context.Tree, iRec) - 1;
                            pObj.DescGenerations = descGens;
                            patList.Add(pObj);
                        }
                    }

                    progress.Increment();
                }
            } finally {
                if (progressDone)
                    progress.End();
            }

            return patList;
        }

        public static IList<PatriarchObj> GetPatriarchsLinks(
            BaseContext context, int gensMin, bool datesCheck, bool loneSuppress, IProgressController progress)
        {
            var patList = GetPatriarchsList(context, gensMin, datesCheck, progress, false);

            progress.Begin(LangMan.LS(LSID.LinksSearch), patList.Count);
            try {
                int num2 = patList.Count;
                for (int i = 0; i < num2; i++) {
                    PatriarchObj patr = patList[i];

                    for (int j = i + 1; j < num2; j++) {
                        PatriarchObj patr2 = patList[j];

                        GDMIndividualRecord cross = TreeTools.PL_SearchDesc(context.Tree, patr.IRec, patr2.IRec);
                        if (cross != null) {
                            patr.HasLinks = true;
                            patr2.HasLinks = true;

                            if (cross.Sex == GDMSex.svFemale) {
                                patr.Links.Add(patr2);
                            } else {
                                patr2.Links.Add(patr);
                            }
                        }
                    }

                    progress.Increment();
                }
            } finally {
                progress.End();
            }

            if (loneSuppress) {
                for (int i = patList.Count - 1; i >= 0; i--) {
                    var patr = patList[i];
                    if (!patr.HasLinks) patList.RemoveAt(i);
                }
            }

            return patList;
        }

        private static void PL_WalkDescLinks(GDMTree tree, Graph graph, GKVarCache<GDMFamilyRecord, PGNode> pgNodes, PGNode prevNode, GDMIndividualRecord ancestor)
        {
            for (int i = 0, count = ancestor.SpouseToFamilyLinks.Count; i < count; i++) {
                var family = tree.GetPtrValue(ancestor.SpouseToFamilyLinks[i]);
                var node = pgNodes[family];

                if (node != null && node.Type != PGNodeType.Default) {
                    Vertex vtx = graph.FindVertex(node.FamilyXRef);
                    if (vtx == null) {
                        vtx = graph.AddVertex(node.FamilyXRef, node);
                    }

                    if (prevNode != null) {
                        graph.AddDirectedEdge(prevNode.FamilyXRef, node.FamilyXRef, 1, null);
                    }

                    prevNode = node;
                }

                for (int k = 0, count2 = family.Children.Count; k < count2; k++) {
                    GDMIndividualRecord child = tree.GetPtrValue(family.Children[k]);
                    PL_WalkDescLinks(tree, graph, pgNodes, prevNode, child);
                }
            }
        }

        public static Graph GetPatriarchsGraph(
            BaseContext context, int gensMin, bool datesCheck, bool loneSuppress, IProgressController progress)
        {
            Graph graph = new Graph();

            try {
                var patList = GetPatriarchsList(context, gensMin, datesCheck, progress, false);

                var pgNodes = new GKVarCache<GDMFamilyRecord, PGNode>();

                // prepare
                int count = patList.Count;
                for (int i = 0; i < count; i++) {
                    PatriarchObj patNode = patList[i];
                    GDMIndividualRecord iRec = patNode.IRec;

                    for (int k = 0, num2 = iRec.SpouseToFamilyLinks.Count; k < num2; k++) {
                        GDMFamilyRecord family = context.Tree.GetPtrValue(iRec.SpouseToFamilyLinks[k]);
                        pgNodes[family] = new PGNode(family.XRef, PGNodeType.Patriarch, patNode.DescGenerations);
                    }
                }

                try {
                    int patCount = patList.Count;
                    progress.Begin(LangMan.LS(LSID.LinksSearch), patCount);

                    for (int i = 0; i < patCount; i++) {
                        PatriarchObj patr = patList[i];

                        for (int j = i + 1; j < patCount; j++) {
                            PatriarchObj patr2 = patList[j];

                            GDMFamilyRecord cross = TreeTools.PL_SearchIntersection(context.Tree, patr.IRec, patr2.IRec);

                            if (cross != null) {
                                var node = pgNodes[cross];

                                if (node != null && node.Type == PGNodeType.Patriarch) {
                                    // dummy
                                } else {
                                    int size = GKUtils.GetDescGenerations(context.Tree, context.Tree.GetPtrValue(cross.Husband));
                                    if (size == 0) size = 1;
                                    pgNodes[cross] = new PGNode(cross.XRef, PGNodeType.Intersection, size);
                                }
                            }
                        }

                        progress.Increment();
                    }

                    // create graph
                    for (int i = 0; i < patCount; i++) {
                        PatriarchObj patNode = patList[i];
                        PL_WalkDescLinks(context.Tree, graph, pgNodes, null, patNode.IRec);
                    }

                    if (loneSuppress) {
                        for (int i = patCount - 1; i >= 0; i--) {
                            PatriarchObj patr = patList[i];
                            if (patr.Links.Count == 0)
                                patList.RemoveAt(i);
                        }
                    }
                } finally {
                    progress.End();
                }
            } catch (Exception ex) {
                Logger.WriteError("PatriarchsMan.GetPatriarchsGraph()", ex);
            }

            return graph;
        }
    }
}
